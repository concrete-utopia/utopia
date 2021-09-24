{}:

let
  config = {
    allowUnfree = true;
  };

  pkgs = import (builtins.fetchTarball {
    name = "nixos-master";
    url = https://github.com/NixOS/nixpkgs/archive/6120ac5cd201f6cb593d1b80e861be0342495be9.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "04mrjxr1qsdcgcryx7yy72cgcw14c0770gfcgzrdfpnvmjdgbi9i";
  }) { inherit config; };
in
  { 
    pkgs = pkgs;
  }
