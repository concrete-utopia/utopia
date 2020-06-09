{}:

let
  config = {
    allowUnfree = true;
  };

  pkgs = import (builtins.fetchTarball {
    name = "nixos-19.09";
    url = https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  }) { inherit config; };
  
  haskell-lib = pkgs.haskell.lib;
  custom-cabal-plan = haskell-lib.addExtraLibraries (haskell-lib.appendConfigureFlag pkgs.haskellPackages.cabal-plan "-flicense-report") [pkgs.haskellPackages.zlib pkgs.haskellPackages.tar]; 
in
  { 
    pkgs = pkgs;
    custom-cabal-plan = custom-cabal-plan;
  }