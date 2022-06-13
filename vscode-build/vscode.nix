{}:

let
  release = (import ../release.nix {});
  pkgs = release.pkgs;
  lib = pkgs.lib;

  stdenv = pkgs.stdenv;
  system = builtins.currentSystem;

  vscodeVersion = "1.61.2";
  vscodeSource = pkgs.fetchFromGitHub {
    owner = "microsoft";
    repo = "vscode";
    rev = vscodeVersion;
    sha256 = "0d0mw3j6ja1rmn85gw55jkyl1a0k1y9j319b4aswpv769p6sxpak";
  };

  vscodePatchedSource =stdenv.mkDerivation {
    name = "utopia-vscode-patched-source-${vscodeVersion}";
    src = builtins.filterSource (path: type: false) ./.;
    patches = [ ./vscode.patch ];
    unpackPhase = ''
      cp -r ${vscodeSource}/. .
      chmod -R u+rw .
    '';
    installPhase = ''
      mkdir -p $out
      cp -r . $out/.
    '';
  };

  stupidTest = stdenv.mkDerivation {
    name = "stupid-test";
    src = builtins.filterSource (path: type: false) ./.;
    outputHashAlgo = "sha256";
    outputHash = "d2ytgsVKv2yemcgOyIX1iTiEsfvDubg4iGpqV5QWjsk=";
    outputHashMode = "recursive";
    buildPhase = ''
    '';
    installPhase = ''
      mkdir -p $out/
      echo "Test" > $out/test-file
    '';
  };

  vsCodeYarnInstall = stdenv.mkDerivation {
    name = "utopia-vscode-install-${vscodeVersion}";
    pname = "utopia-vscode-install-${vscodeVersion}";
    src = builtins.filterSource (path: type: false) ./.;
    outputHashAlgo = "sha256";
    outputHash = "WHp9hIurZkPAOGYNbSyONfFtJ18/GrygZd4ZFjJag79=";
    outputHashMode = "recursive";
    buildInputs = with pkgs; [
      yarn
      python3
      pkg-config
      xorg.libxkbfile
      xorg.libX11
      libsecret
      git
      cacert
    ];
    unpackPhase = ''
      cp -r ${vscodePatchedSource}/. .
      chmod -R u+rw .
    '';
    buildPhase = ''
      mkdir .home
      HOME=/build/.home
      # Needed because some subsequent step tries to use `git config`.
      git init
      # Replace with `yarn install` once I've figured out why this is fighting me.
      yarn --ignore-scripts add gulp
    '';
    installPhase = ''
      # Related nix bugs:
      # - https://github.com/NixOS/nix/issues/5509
      # - https://github.com/NixOS/nix/issues/4859
      mkdir -p $out/
      mkdir -p $out/node_modules/gulp/
      mkdir -p $out/node_modules/gulp-cli/
      cat node_modules/gulp/node_modules/.bin/*
      #rm -rf node_modules/gulp/node_modules/.bin/gulp
      #rm -rf node_modules/gulp/bin
      #rm -rf node_modules/gulp-cli/bin/gulp.js
      #rm -rf node_modules/gulp-cli/node_modules/.bin/color-support
      cp -L -r node_modules/gulp/* $out/node_modules/gulp/
      cp -L -r node_modules/gulp-cli/* $out/node_modules/gulp-cli/
      find $out/node_modules
    '';
  };

  vsCode = stdenv.mkDerivation {
    name = "utopia-vscode-${vscodeVersion}";
    src = builtins.filterSource (path: type: false) ./.;
    buildInputs = with pkgs; [
      nodejs-16_x
      yarn
      python3
      pkg-config
      xorg.libxkbfile
      xorg.libX11
      libsecret
      git
      cacert
    ];
    unpackPhase = ''
      ls -al ${vsCodeYarnInstall}
      cp -r ${vscodePatchedSource}/. .
      chmod -R u+rw .
    '';
    buildPhase = ''
      ln -s ${vsCodeYarnInstall}/node_modules ./node_modules
      export PATH="${vsCodeYarnInstall}/node_modules/.bin:$PATH"
      yarn gulp compile-build
      yarn gulp minify-vscode
      yarn compile-web
    '';
    installPhase = ''
      # Remove maps.
      rm -f out-vscode-min/**/*.js.map

      mkdir -p $out
      mkdir -p $out/vscode
      #cp -r out-vscode-min/. $out/vscode

      #mkdir -p $out/lib/semver-umd
      #cp -r node_modules/semver-umd/. $out/lib/semver-umd

      mkdir -p $out/lib/vscode-oniguruma
      cp -r node_modules/vscode-oniguruma/. $out/lib/vscode-oniguruma

      mkdir -p $out/lib/vscode-textmate
      cp -r node_modules/vscode-textmate/. $out/lib/vscode-textmate

      mkdir -p $out/extensions
      cp -r extensions/. $out/extensions
    '';
  };
in
  {
    vsCode = vsCode;
    stupidTest = stupidTest;
    vsCodeYarnInstall = vsCodeYarnInstall;
  }

