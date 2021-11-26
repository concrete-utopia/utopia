let
  defaultNix = (import ./default.nix);
in
  defaultNix.server-shell.env
