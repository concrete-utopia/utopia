{}: 


let
  release = (import ./release.nix {});
  nixpkgs = release.pkgs;
  inherit (nixpkgs) stdenv lib;
  editor = (import ./editor/default.nix {});
  website = (import ./website-next/default.nix {});
  server = (import ./server/default.nix {});
  vscode = (import ./vscode-build/default.nix {});

  bundleFiles = stdenv.mkDerivation {
    name = "utopia-bundle-1.0";
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/server
      ln -s ${server.utopia-web}/bin/utopia-web $out/server/utopia-web

      ln -s ${server.migrations} $out/server/migrations

      ln -s ${vscode} $out/server/vscode
      
      ln -s ${editor} $out/server/editor

      ln -s ${website} $out/server/public
    '';
  };
in
  nixpkgs.dockerTools.buildImage {
    name = "utopia-image";
    tag = "latest";
    copyToRoot = bundleFiles;
    config = {
      WorkingDir = "/server";
      Cmd = [ "./utopia-web +RTS -N -c" ];
    };
  }

