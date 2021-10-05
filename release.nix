{}:

let
  config = {
    allowUnfree = true;
  };

  pkgs = import (builtins.fetchTarball {
    name = "nixos-master";
    url = https://github.com/NixOS/nixpkgs/archive/62126f8c155d58f7836b2c3873a40e4ad00cf46e.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0vzbm8hfc8nqp8nkw2riphgz1bbrrrx0m6717a2yyzxpg0na26wr";
  }) { inherit config; };
in
  { 
    pkgs = pkgs;
  }
