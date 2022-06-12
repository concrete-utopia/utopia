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

  vsCodeYarnInstall = stdenv.mkDerivation {
    name = "utopia-vscode-install-${vscodeVersion}";
    src = builtins.filterSource (path: type: false) ./.;
    outputHashAlgo = "sha256";
    outputHash = "vjuu6Qq3QakB9ab/TMLjk8MZdOGhCXPXdbXt+H0Lf9s=";
    outputHashMode = "recursive";
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
      cp -r ${vscodePatchedSource}/. .
      chmod -R u+rw .
    '';
    buildPhase = ''
      mkdir .home
      HOME=/build/.home
      # Needed because some subsequent step tries to use `git config`.
      git init
      yarn install
      ls -al /build/.home/
    '';
    installPhase = ''
      mkdir -p $out/node_modules/gulp
      cp -L -r node_modules/gulp/. $out/node_modules/gulp/
      ls -lR $out
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
    vsCodeYarnInstall = vsCodeYarnInstall;
  }

