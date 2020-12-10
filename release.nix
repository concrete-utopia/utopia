{}:

let
  config = {
    allowUnfree = true;
  };

  pkgs = import (builtins.fetchTarball {
    name = "nixos-master";
    url = https://github.com/NixOS/nixpkgs/archive/1f65211aa37adfc5a28c4849bbe73dcb2d2d1e2b.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "07i4kbxpn0pvqnyvpvxp310ylj5yan9hp89gk3y38qfxcz39ik71";
  }) { inherit config; };
in
  { 
    pkgs = pkgs;
  }
