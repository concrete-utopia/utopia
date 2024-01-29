let
  node = pkgs.nodejs-18_x;
  stdenv = pkgs.stdenv;
  pnpm = node.pkgs.pnpm;
  yarn = pkgs.yarn;

  nodePackages = [
    node
    pnpm
    (yarn.override { nodejs = node; })
  ];
