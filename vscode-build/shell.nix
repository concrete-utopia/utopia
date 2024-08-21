let
  release = (import ../release.nix {});
  pkgs = release.recentPkgs;
  node = pkgs.nodejs_20;
  stdenv = pkgs.stdenv;
  pnpm = node.pkgs.pnpm;
  yarn = pkgs.yarn.override { nodejs = node; };

  nodePackages = [
    node
    pnpm
    yarn
  ];

  scripts = [
    (pkgs.writeScriptBin "update-vscode-patch-inner" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${yarn}/bin/yarn
      ${yarn}/bin/yarn run make-patch
    '')
    (pkgs.writeScriptBin "pull-extension-inner" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${yarn}/bin/yarn
      ${yarn}/bin/yarn run pull-utopia-extension
    '')
    (pkgs.writeScriptBin "update-vscode-build-extension-inner" ''
      #!/usr/bin/env bash
      set -e
      build-utopia-vscode-extension
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${yarn}/bin/yarn
      ${yarn}/bin/yarn run pull-utopia-extension
    '')
    (pkgs.writeScriptBin "build-vscode-inner" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      rm -rf ./dist ./node_modules
      ${yarn}/bin/yarn
      ${yarn}/bin/yarn run build
    '')
  ];

in pkgs.mkShell {
  buildInputs = [
    (pkgs.stdenv.mkDerivation {
      name = "scripts";
      phases = "installPhase";
      installPhase = ''
        mkdir -p $out/bin
      '' + (builtins.concatStringsSep "" (builtins.map (script: ''
        for f in $(ls -d ${script}/bin/*); do ln -s $f $out/bin; done
      '') scripts));
    })
  ] ++ nodePackages;
}
