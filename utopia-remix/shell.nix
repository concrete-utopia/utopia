let
  node = pkgs.nodejs-18_x;
  stdenv = pkgs.stdenv;
  yarn = pkgs.yarn;

  nodePackages = [
    node
	pnpm
	pnpm = node.pkgs.pnpm;
    (yarn.override { nodejs = node; })
  ];
