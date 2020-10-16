# Nix flipper

Given a file of the form:

```nix
x:
{
  f = drv: y: e;
}
```

It will produce a file like

```nix
x:
let unflipped = import ./lib.nix x;
in {
  f = y: drv: e;
}
```
