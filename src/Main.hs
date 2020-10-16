module Main
  where

import           Control.Category               ( (>>>) )
import           Data.Fix                       ( Fix(..) )
import           Data.Foldable
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Nix.Expr
import           Nix.Parser
import           Say
import           System.Environment             ( getArgs )
import           System.Exit

import           Data.List                      ( dropWhileEnd
                                                , sortOn
                                                )
import           Span

main :: IO ()
main = do
  t <- getArgs >>= \case
    []    -> Text.getContents
    f : _ -> Text.readFile f
  case parseNixTextLoc t of
    Failure e -> do
      sayErrShow e
      exitFailure
    Success e -> do
      let (setLoc, fs) = extractFunctions e
      let updates =
            SpanUpdate
                (SrcSpan (spanBegin setLoc) (spanBegin setLoc))
                "let unflipped = import ./lib.nix { inherit pkgs lib; };\nin "
              : (functionUpdate <$> fs)
      let u = updateSpans updates t
      Text.putStr u

-- | Create an update for a function, putting "drv" and "pkg" arguments last.
functionUpdate :: Function -> SpanUpdate
functionUpdate Function {..} =
  let pats           = sortOn (`elem` ["drv", "pkg"]) functionArgs
      apps           = functionArgs
      (pats', apps') = etaReduce pats apps
      fun x = x <> ":"
  in  SpanUpdate functionBodyLoc
                 (Text.unwords ((fun <$> pats') <> ["unflipped." <> functionName] <> apps'))

etaReduce :: [Text] -> [Text] -> ([Text], [Text])
etaReduce pats apps = unzip . dropWhileEnd (uncurry (==)) $ zip pats apps

-- | Get a list of functions in the attrset in this file (after the opening
-- attrset parameter). Also return the span of the attrset conating the
-- functions.
extractFunctions :: NExprLoc -> (SrcSpan, [Function])
extractFunctions = unFix >>> \case
  NAbs_ _ _ (Fix (NSet_ l _ bs)) ->
    ( l
    , [ Function name args loc
      | NamedVar n f _ <- bs
      , let args = funArgs f
      , let loc = annotation . getCompose . unFix $ f
      , Just name <- pure $ pathToName n
      ]
    )
  _ -> error "Unexpected file layout"

-- | Get the names of the arguments to a function
funArgs :: NExprLoc -> [Text]
funArgs = unFix >>> \case
  NAbs_ _ (Param n) e -> n : funArgs e
  _                   -> []

-- | Construct a name from an attribute path
pathToName :: NAttrPath NExprLoc -> Maybe Text
pathToName = fmap (Text.intercalate ".") . traverse toText . toList
 where
  toText = \case
    StaticKey n -> Just n
    _           -> Nothing

data Function = Function
  { functionName    :: Text
  , functionArgs    :: [Text]
  , functionBodyLoc :: SrcSpan
  }
  deriving Show
