{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8107"}:
let
  pkgs = nixpkgs.pkgs;
  haskLib = pkgs.haskell.lib;
  overriddenHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      utopia-clientmodel = super.callCabal2nix "utopia-clientmodel" (./lib) {};
    };
  };
  utopia-clientmodel = overriddenHaskellPackages.utopia-clientmodel;
in
  utopia-clientmodel
