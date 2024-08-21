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

  recentPkgs = import (builtins.fetchTarball {
    name = "nixos-24.05";
    url = https://github.com/NixOS/nixpkgs/archive/24.05.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1lr1h35prqkd1mkmzriwlpvxcb34kmhc9dnr48gkm8hh089hifmx";
  }) { inherit config; };

in
  { 
    pkgs = pkgs;
    recentPkgs = recentPkgs;
  }
