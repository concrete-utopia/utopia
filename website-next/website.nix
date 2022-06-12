{pkgs ? import <nixpkgs> {}}:

let
  stdenv = pkgs.stdenv;
  system = builtins.currentSystem;
  nodeDependencies = (pkgs.callPackage ./default.nix {}).nodeDependencies;
in
  stdenv.mkDerivation {
    name = "website";
    src = ./.;
    buildInputs = [pkgs.nodejs];
    buildPhase = ''
      ln -s ${nodeDependencies}/lib/node_modules ./node_modules
      export PATH="${nodeDependencies}/bin:$PATH"

      # Build the distribution bundle in "dist"
      npm run export
      cp -r out $out/
    '';
    installPhase = ''true'';
  }
