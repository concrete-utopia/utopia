{}:

let
  config = {
    allowUnfree = true;
  };

  pkgs = import (builtins.fetchTarball {
    name = "nixos-master";
    url = https://github.com/NixOS/nixpkgs/archive/f513127ffe5e3f5225d814933d554488c9f33c5c.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "18l6qn7plavxhw6h1xpg7h1szjc0bvqdy1wfzhc2zqw9j8890a0j";
  }) { inherit config; };
in
  { 
    pkgs = pkgs;
  }
