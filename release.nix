{}:

let
  config = {
    allowUnfree = true;
  };

  pkgs = import (builtins.fetchTarball {
    name = "nixos-master";
    url = https://github.com/NixOS/nixpkgs/archive/43e3b6af08f29c4447a6073e3d5b86a4f45dd420.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "11hhdifv8nn56qakw9n37k7vcrlp0ra77dzvbklr9q5ygf0mh75h";
  }) { inherit config; };
in
  { 
    pkgs = pkgs;
  }
