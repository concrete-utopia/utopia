{pkgs ? import <nixpkgs> {}}:

let
  stdenv = pkgs.stdenv;
  system = builtins.currentSystem;

  build = (pkgs.callPackage ./default.nix {});
  nodeDependencies = build.nodeDependencies;
  vscodeDTS = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/microsoft/vscode/1.61.2/src/vs/vscode.d.ts";
    sha256 = "0d13d20flfjchsd7j2bawjw3i4v7w9qgzwm8j2a2zm4yavwxfz7a";
  };
  vscodeProposedDTS = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/microsoft/vscode/1.61.2/src/vs/vscode.proposed.d.ts";
    sha256 = "1x47ik33xf3ak2nly47h98nxg9xpjg06wywmb4nw0c44kf78p86j";
  };
  modifiedPackage = build.package.override {
    dontNpmInstall = true;
    postInstall = "true";
    preRebuild = ''
      rm -rf dist
      rm -rf src/vscode-types/*
      cp ${vscodeDTS} src/vscode-types/vscode.d.ts
      cp ${vscodeProposedDTS} src/vscode-types/vscode.proposed.d.ts
    '';
  };
in
  stdenv.mkDerivation {
    name = "utopia-vscode-extension";
    src = ./.;
    buildInputs = [ pkgs.nodejs pkgs.yarn ];
    buildPhase = ''
      ln -s ${modifiedPackage}/lib/node_modules/utopia-vscode-extension/node_modules ./node_modules
      export PATH="${modifiedPackage}/bin:$PATH"
  
      rm -rf src/vscode-types/*
      ln -s ${vscodeDTS} src/vscode-types/vscode.d.ts
      ln -s ${vscodeProposedDTS} src/vscode-types/vscode.proposed.d.ts
  
      # Build the distribution bundle in "dist"
      npm run build
      cp -r dist $out/
    '';
    installPhase = ''true'';
  }
