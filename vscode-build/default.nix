{}:

let
  release = (import ../release.nix {});
  nixpkgs = release.pkgs;

  inherit (nixpkgs) stdenv lib;
in
  stdenv.mkDerivation {
    name = "vscode-build-1.0";
    src = ./dist;
    installPhase = ''
      mkdir $out
      cp -R * $out/
    '';
  }
