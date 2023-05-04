{}:

let
  release = (import ../release.nix {});
  nixpkgs = release.pkgs;

  inherit (nixpkgs) stdenv lib;
in
  stdenv.mkDerivation {
    name = "utopia-website-1.0";
    src = ./out;
    installPhase = ''
      mkdir $out
      cp -R * $out/
    '';
  }
