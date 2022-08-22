let
  defaultNix = (import ./default.nix);
in
  defaultNix.env
