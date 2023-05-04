{}:

let
  release = (import ../release.nix {});
  nixpkgs = release.pkgs;

  inherit (nixpkgs) stdenv lib;
in
  stdenv.mkDerivation {
    name = "utopia-editor-1.0";
    srcs = [./resources/editor ./lib];
    sourceRoot = ".";
    installPhase = ''
      mkdir $out
      cp -R * $out/
    '';
  }
