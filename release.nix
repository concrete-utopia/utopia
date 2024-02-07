{}:

let
  config = {
    allowUnfree = true;
  };

  pkgs = import (builtins.fetchTarball {
    name = "nixos-22.11";
    url = https://github.com/NixOS/nixpkgs/archive/22.11.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "11w3wn2yjhaa5pv20gbfbirvjq6i3m7pqrq2msf0g7cv44vijwgw";
  }) { inherit config; };

in
  { 
    pkgs = pkgs;
  }
