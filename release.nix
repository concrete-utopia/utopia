{}:

let
  config = {
    allowUnfree = true;
  };

  pkgs = import (builtins.fetchTarball {
    name = "nixos-20.09";
    url = https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
  }) { inherit config; };
  
  haskell-lib = pkgs.haskell.lib;
  custom-cabal-plan = haskell-lib.addExtraLibraries (haskell-lib.appendConfigureFlag pkgs.haskellPackages.cabal-plan "-flicense-report") [pkgs.haskellPackages.zlib pkgs.haskellPackages.tar]; 
in
  { 
    pkgs = pkgs;
    custom-cabal-plan = custom-cabal-plan;
  }