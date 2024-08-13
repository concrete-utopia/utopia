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
    name = "nixos-23.11";
    url = https://github.com/NixOS/nixpkgs/archive/23.11.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
  }) { inherit config; };

in
  { 
    pkgs = pkgs;
    recentPkgs = recentPkgs;
  }
