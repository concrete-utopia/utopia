{ 
  compiler ? "ghc865",
  includeServerBuildSupport ? true,
  includeEditorBuildSupport ? true,
  includeRunLocallySupport ? true,
  includeReleaseSupport ? false
}:

let
  release = (import ./release.nix {});
  pkgs = release.pkgs;
  lib = pkgs.lib;
  node = pkgs.nodejs-14_x;
  stdenv = pkgs.stdenv;

  cabal = pkgs.haskellPackages.cabal-install;
  # Slightly kludgy because the zlib Haskell package is a pain in the face.
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (hpkgs: with hpkgs; [
    zlib
  ]);

  baseEditorScripts = [
    (pkgs.writeScriptBin "install-editor" ''
      #!/usr/bin/env bash
      set -e
      update-vscode-build-extension
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${node}/bin/npm --scripts-prepend-node-path=true install
    '')
    (pkgs.writeScriptBin "install-editor-ci" ''
      #!/usr/bin/env bash
      set -e
      build-utopia-vscode-common
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${node}/bin/npm --scripts-prepend-node-path=true install
    '')
    (pkgs.writeScriptBin "install-website" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/website-next
      ${node}/bin/npm --scripts-prepend-node-path=true install
    '')
    (pkgs.writeScriptBin "test-editor" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${node}/bin/npm --scripts-prepend-node-path=true test
    '')
    (pkgs.writeScriptBin "test-utopia-api" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-api
      ${node}/bin/npm --scripts-prepend-node-path=true test
    '')
    (pkgs.writeScriptBin "test-website" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/website-next
      # ${node}/bin/npm --scripts-prepend-node-path=true test
    '')
    (pkgs.writeScriptBin "test-editor-all" ''
      #!/usr/bin/env bash
      set -e
      test-editor
      test-website
    '')
    (pkgs.writeScriptBin "check-editor" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${node}/bin/npm --scripts-prepend-node-path=true run check
    '')
    (pkgs.writeScriptBin "check-editor-ci" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${node}/bin/npm --scripts-prepend-node-path=true run check-ci
    '')
    (pkgs.writeScriptBin "check-editor-all-ci" ''
      #!/usr/bin/env bash
      set -e
      install-editor-ci
      install-website
      check-editor-ci
      test-website
    '')
    (pkgs.writeScriptBin "build-editor-staging-ci" ''
      #!/usr/bin/env bash
      set -e
      install-editor-ci
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${node}/bin/npm --scripts-prepend-node-path=true run staging
    '')
    (pkgs.writeScriptBin "build-utopia-vscode-common" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-vscode-common
      ${node}/bin/npm --scripts-prepend-node-path=true install
      ${node}/bin/npm --scripts-prepend-node-path=true run build
    '')
    (pkgs.writeScriptBin "build-utopia-vscode-extension" ''
      #!/usr/bin/env bash
      set -e
      build-utopia-vscode-common
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-vscode-extension
      ${node}/bin/npm --scripts-prepend-node-path=true install
      ${node}/bin/npm --scripts-prepend-node-path=true run build
    '')
    (pkgs.writeScriptBin "update-vscode-build-extension" ''
      #!/usr/bin/env bash
      set -e
      build-utopia-vscode-extension
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${pkgs.yarn}/bin/yarn run pull-utopia-extension
    '')
    (pkgs.writeScriptBin "build-vscode" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${pkgs.yarn}/bin/yarn
      ${pkgs.yarn}/bin/yarn run build
    '')
    (pkgs.writeScriptBin "build-vscode-with-extension" ''
      #!/usr/bin/env bash
      set -e
      build-utopia-vscode-extension
      build-vscode
    '')
  ];

  withBaseEditorScripts = lib.optionals includeEditorBuildSupport baseEditorScripts;

  serverBaseScripts = [
    (pkgs.writeScriptBin "rebuild-cabal" ''
      #!/usr/bin/env bash
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      ${pkgs.haskellPackages.hpack}/bin/hpack
    '')
    (pkgs.writeScriptBin "cabal-update" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      ${cabal}/bin/cabal new-update 'hackage.haskell.org,2021-06-09T12:43:34Z'
    '')
    (pkgs.writeScriptBin "test-server-inner" ''
      #!/usr/bin/env bash
      set -e
      rebuild-cabal
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      ${cabal}/bin/cabal new-test --disable-optimization --disable-profiling --disable-documentation --disable-library-coverage --disable-benchmarks utopia-web-test
    '')
    (pkgs.writeScriptBin "test-server" ''
      #!/usr/bin/env bash
      set -e
      cabal-update
      test-server-inner
    '')
    (pkgs.writeScriptBin "watch-tests" ''
      #!/usr/bin/env bash
      set -e
      cabal-update
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      ${pkgs.nodePackages.nodemon}/bin/nodemon -e hs,yaml --watch src --watch test --watch package.yaml --exec test-server-inner
    '')
    (pkgs.writeScriptBin "test-server-ci" ''
      #!/usr/bin/env bash
      set -e
      cabal-update
      ${pkgs.parallel}/bin/parallel --delay 10 --halt now,done=1 --line-buffer --tag ::: redis-server test-server-inner
    '')
  ];

  withServerBaseScripts = withBaseEditorScripts ++ (lib.optionals includeServerBuildSupport serverBaseScripts);

  editorRunScripts = [
    (pkgs.writeScriptBin "watch-tsc" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${node}/bin/npx --scripts-prepend-node-path=true tsc --watch && NODE_OPTIONS=--max_old_space_size=4096
    '')
    (pkgs.writeScriptBin "watch-editor-cowboy" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      RUN_COMPILER=true ${node}/bin/npm --scripts-prepend-node-path=true run move-fast-and-break-things
    '')
    (pkgs.writeScriptBin "watch-editor-no-compile" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      RUN_COMPILER=false ${node}/bin/npm --scripts-prepend-node-path=true run move-fast-and-break-things
    '')
    (pkgs.writeScriptBin "watch-editor-performance" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      RUN_COMPILER=true ${node}/bin/npm --scripts-prepend-node-path=true run performance-test
    '')
    (pkgs.writeScriptBin "watch-editor-cowboy-danger-hot" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      RUN_COMPILER=true ${node}/bin/npm --scripts-prepend-node-path=true run move-fast-and-break-things-hot
    '')
    (pkgs.writeScriptBin "watch-website" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/website-next
      ${node}/bin/npm --scripts-prepend-node-path=true install
      BROWSER=none ${node}/bin/npm --scripts-prepend-node-path=true run dev
    '')
  ];

  withEditorRunScripts = withServerBaseScripts ++ (lib.optionals includeRunLocallySupport editorRunScripts);

  serverRunScripts = [
    (pkgs.writeScriptBin "style-project" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server/src
      find . -name '*.hs' | xargs ${pkgs.haskellPackages.stylish-haskell}/bin/stylish-haskell -i
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server/test
      find . -name '*.hs' | xargs ${pkgs.haskellPackages.stylish-haskell}/bin/stylish-haskell -i
    '')
    (pkgs.writeScriptBin "run-server-inner" ''
      #!/usr/bin/env bash
      set -e
      rebuild-cabal
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      ${cabal}/bin/cabal new-run -j --disable-optimization --disable-profiling --disable-documentation --disable-library-coverage --disable-benchmarks utopia-web -- +RTS -N -c
    '')
    (pkgs.writeScriptBin "run-server" ''
      #!/usr/bin/env bash
      set -e
      cabal-update
      run-server-inner
    '')
    (pkgs.writeScriptBin "watch-server" ''
      #!/usr/bin/env bash
      set -e
      cabal-update
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      ${pkgs.nodePackages.nodemon}/bin/nodemon -e hs,yaml --watch src --watch package.yaml --exec run-server-inner
    '')
  ];

  withServerRunScripts = withEditorRunScripts ++ (lib.optionals includeRunLocallySupport serverRunScripts);

  vscodeDevScripts = [
    (pkgs.writeScriptBin "update-vscode-patch" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${pkgs.yarn}/bin/yarn
      ${pkgs.yarn}/bin/yarn run make-patch
    '')
    (pkgs.writeScriptBin "watch-utopia-vscode-common" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-vscode-common
      ${node}/bin/npm --scripts-prepend-node-path=true install
      ${node}/bin/npm --scripts-prepend-node-path=true run watch-dev
    '')
    (pkgs.writeScriptBin "watch-utopia-vscode-extension" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-vscode-extension
      ${node}/bin/npm --scripts-prepend-node-path=true install
      ${node}/bin/npm --scripts-prepend-node-path=true run watch-dev
    '')
    (pkgs.writeScriptBin "pull-extension" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${pkgs.yarn}/bin/yarn run pull-utopia-extension
    '')
    (pkgs.writeScriptBin "watch-vscode-build-extension-only" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${pkgs.nodePackages.nodemon}/bin/nodemon --watch ../utopia-vscode-extension/dist/browser/extension.js --exec pull-extension
    '')
    (pkgs.writeScriptBin "watch-vscode-dev" ''
      #!/usr/bin/env bash
      set -e
      ${pkgs.parallel}/bin/parallel --line-buffer --tag ::: watch-utopia-vscode-common watch-utopia-vscode-extension watch-vscode-build-extension-only
    '')
  ];

  # For the useful scripts in our dev environments
  customDevScripts = [
    (pkgs.writeScriptBin "start-website-server" ''
      #!/usr/bin/env bash
      set -e
      ${pkgs.parallel}/bin/parallel --tagstring '\033[30;3{=$_=++$::color%8=}m[{/}]' --line-buffer --tag ::: watch-server watch-website redis-server
    '')
    (pkgs.writeScriptBin "start" ''
      #!/usr/bin/env bash
      set -e
      install-editor
      ${pkgs.parallel}/bin/parallel --tagstring '\033[30;3{=$_=++$::color%8=}m[{/}]' --line-buffer --tag ::: watch-server watch-editor-cowboy watch-website redis-server
    '')
    (pkgs.writeScriptBin "start-performance" ''
      #!/usr/bin/env bash
      set -e
      install-editor
      ${pkgs.parallel}/bin/parallel --tagstring '\033[30;3{=$_=++$::color%8=}m[{/}]' --line-buffer --tag ::: watch-server watch-editor-performance watch-website redis-server
    '')
    (pkgs.writeScriptBin "start-hot-only-ui-work" ''
      #!/usr/bin/env bash
      set -e
      install-editor
      ${pkgs.parallel}/bin/parallel --tagstring '\033[30;3{=$_=++$::color%8=}m[{/}]' --line-buffer --tag ::: watch-server watch-editor-cowboy-danger-hot watch-website redis-server
    '')    
  ] ++ vscodeDevScripts;

  withCustomDevScripts = withServerRunScripts ++ (lib.optionals includeRunLocallySupport customDevScripts);

  # TODO Come back to these when trying to use nix to build a docker container - https://stackoverflow.com/questions/58421505/how-do-i-apply-a-nix-shell-config-in-a-docker-image
  releaseScripts = [
    (pkgs.writeScriptBin "build-editor" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${node}/bin/npm --scripts-prepend-node-path=true install --unsafe-perm
      ${node}/bin/npm --scripts-prepend-node-path=true run production
    '')
    # CRA for whatever reason will automatically fail on CI for any warnings, so we need to prefix with `CI=false`. Urgh. https://github.com/facebook/create-react-app/issues/3657
    (pkgs.writeScriptBin "build-website" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/website-next
      ${node}/bin/npm --scripts-prepend-node-path=true install
      CI=false ${node}/bin/npm --scripts-prepend-node-path=true run export
    '')
    (pkgs.writeScriptBin "build-server" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      ${cabal}/bin/cabal new-build utopia-web
      cp --verbose $(${pkgs.haskellPackages.cabal-plan}/bin/cabal-plan list-bin exe:utopia-web) .
    '')
    (pkgs.writeScriptBin "build-all" ''
      #!/usr/bin/env bash
      set -e
      build-editor
      build-website
      build-server
    '')
  ];

  scripts = withCustomDevScripts; # ++ (if needsRelease then releaseScripts else []);

  linuxOnlyPackages = lib.optionals stdenv.isLinux [ pkgs.xvfb_run pkgs.x11 pkgs.xorg.libxkbfile ];
  macOSOnlyPackages = lib.optionals stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);

  baseServerPackages = [
    cabal
    ghc
    pkgs.cabal2nix
    pkgs.haskellPackages.stylish-haskell
    pkgs.haskellPackages.hpack
    pkgs.postgresql
    pkgs.redis
  ];

  serverRunPackages = [
  ];

  releasePackages = [
    pkgs.heroku
  ];

  pythonAndPackages = pkgs.python37.withPackages(ps: with ps; [ pyusb tkinter pkgconfig ]);

  basePackages = [ node pkgs.yarn pkgs.libsecret pythonAndPackages pkgs.pkg-config ] ++ linuxOnlyPackages ++ macOSOnlyPackages;
  withServerBasePackages = basePackages ++ (lib.optionals includeServerBuildSupport baseServerPackages);
  withServerRunPackages = withServerBasePackages ++ (lib.optionals includeRunLocallySupport serverRunPackages);
  withReleasePackages = withServerRunPackages ++ (lib.optionals includeReleaseSupport releasePackages);
  packagesToUse = withReleasePackages;

in pkgs.mkShell {
  buildInputs = [
    (pkgs.stdenv.mkDerivation {
      name = "scripts";
      phases = "installPhase";
      installPhase = ''
        mkdir -p $out/bin
      '' + (builtins.concatStringsSep "" (builtins.map (script: ''
        for f in $(ls -d ${script}/bin/*); do ln -s $f $out/bin; done
      '') scripts));
    })
  ] ++ packagesToUse;
  # Makes the electron runner use this executable instead.
  ELECTRON_OVERRIDE_DIST_PATH = if stdenv.isLinux then "${pkgs.electron_6}/bin" else null;
}
