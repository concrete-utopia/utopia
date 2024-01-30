{ 
  compiler ? "ghc902",
  includeServerBuildSupport ? true,
  includeEditorBuildSupport ? true,
  includeRunLocallySupport ? true,
  includeReleaseSupport ? false,
  includeDatabaseSupport ? true
}:

let
  release = (import ./release.nix {});
  pkgs = release.pkgs;
  lib = pkgs.lib;
  node = pkgs.nodejs-16_x;
  postgres = pkgs.postgresql_13;
  stdenv = pkgs.stdenv;
  pnpm = node.pkgs.pnpm;
  yarn = pkgs.yarn;

  nodePackages = [
    node
    pnpm
    (yarn.override { nodejs = node; })
  ];

  cabal = pkgs.haskellPackages.cabal-install;
  # Slightly kludgy because the zlib Haskell package is a pain in the face.
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (hpkgs: with hpkgs; [
    zlib
  ]);

  baseEditorScripts = [
    (pkgs.writeScriptBin "install-utopia-api" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-api
      ${pnpm}/bin/pnpm install
      ${pnpm}/bin/pnpm run build
    '')
    (pkgs.writeScriptBin "install-editor" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)
      ${pnpm}/bin/pnpm install
      install-utopia-api
      update-vscode-build-extension
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm install
    '')
    (pkgs.writeScriptBin "install-editor-ci" ''
      #!/usr/bin/env bash
      set -e
      install-utopia-api
      build-utopia-vscode-common
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm install
    '')
    (pkgs.writeScriptBin "install-website" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/website-next
      ${pnpm}/bin/pnpm install
    '')
    (pkgs.writeScriptBin "install-website-editor-ci" ''
      #!/usr/bin/env bash
      set -e
      install-editor-ci
      install-website
    '')
    (pkgs.writeScriptBin "test-editor" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm test
    '')
    (pkgs.writeScriptBin "test-editor-watch" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm test-watch
    '')
    (pkgs.writeScriptBin "test-utopia-api" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-api
      ${pnpm}/bin/pnpm test
    '')
    (pkgs.writeScriptBin "test-website" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/website-next
      # ${pnpm}/bin/pnpm test
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
      ${pnpm}/bin/pnpm run check
    '')
    (pkgs.writeScriptBin "check-editor-ci" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run check-ci
    '')
    (pkgs.writeScriptBin "check-editor-code" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run check-code
    '')
    (pkgs.writeScriptBin "check-editor-jest" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run test-ci
    '')
    (pkgs.writeScriptBin "check-editor-karma" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run test-browser
    '')
    (pkgs.writeScriptBin "check-editor-karma-shard-1" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run test-browser-shard-1
    '')
    (pkgs.writeScriptBin "check-editor-karma-shard-2" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run test-browser-shard-2
    '')
    (pkgs.writeScriptBin "test-editor-browser" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run test-browser
    '')
    (pkgs.writeScriptBin "test-editor-browser-debug" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run test-browser-debug
    '')
    (pkgs.writeScriptBin "check-editor-all-ci" ''
      #!/usr/bin/env bash
      set -e
      install-editor-ci
      install-website
      check-editor-ci
      test-website
    '')
    (pkgs.writeScriptBin "check-editor-code-ci" ''
      #!/usr/bin/env bash
      set -e
      install-editor-ci
      install-website
      check-editor-code
      test-website
    '')
    (pkgs.writeScriptBin "check-editor-jest-ci" ''
      #!/usr/bin/env bash
      set -e
      install-editor-ci
      install-website
      check-editor-jest
    '')
    (pkgs.writeScriptBin "check-editor-karma-ci-shard-1" ''
      #!/usr/bin/env bash
      set -e
      install-editor-ci
      install-website
      check-editor-karma-shard-1
    '')
    (pkgs.writeScriptBin "check-editor-karma-ci-shard-2" ''
      #!/usr/bin/env bash
      set -e
      install-editor-ci
      install-website
      check-editor-karma-shard-2
    '')
    (pkgs.writeScriptBin "build-editor-staging-ci" ''
      #!/usr/bin/env bash
      set -e
      install-editor-ci
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/website-next
      ${pnpm}/bin/pnpm install
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run staging-print-json
    '')
    (pkgs.writeScriptBin "build-editor-branches-ci" ''
      #!/usr/bin/env bash
      set -e
      install-editor-ci
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/website-next
      ${pnpm}/bin/pnpm install
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run branches-print-json
    '')
    (pkgs.writeScriptBin "build-utopia-vscode-common" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-vscode-common
      ${pnpm}/bin/pnpm install
      ${pnpm}/bin/pnpm run build
    '')
    (pkgs.writeScriptBin "build-utopia-vscode-extension" ''
      #!/usr/bin/env bash
      set -e
      build-utopia-vscode-common
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-vscode-extension
      ${pnpm}/bin/pnpm install
      ${pnpm}/bin/pnpm run build
    '')
    (pkgs.writeScriptBin "update-vscode-build-extension" ''
      #!/usr/bin/env bash
      set -e
      build-utopia-vscode-extension
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      yarn
      yarn run pull-utopia-extension
    '')
    (pkgs.writeScriptBin "build-vscode" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      rm -rf ./dist ./node_modules
      yarn
      yarn run build
    '')
    (pkgs.writeScriptBin "build-vscode-with-extension" ''
      #!/usr/bin/env bash
      set -e
      build-utopia-vscode-extension
      build-vscode
    '')
  ];

  withBaseEditorScripts = lib.optionals includeEditorBuildSupport baseEditorScripts;

  puppeteerScripts = [
    (pkgs.writeScriptBin "build-puppeteer-tests" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/puppeteer-tests
      ${pnpm}/bin/pnpm install --unsafe-perm
      ${pnpm}/bin/pnpm run build
    '')
      (pkgs.writeScriptBin "run-puppeteer-test" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/puppeteer-tests
      ${pnpm}/bin/pnpm install --unsafe-perm
      ${pnpm}/bin/pnpm run performance-test
    '')
    (pkgs.writeScriptBin "run-comments-test" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/puppeteer-tests
      ${pnpm}/bin/pnpm install --unsafe-perm
      ${pnpm}/bin/pnpm run comments-test
    '')
    (pkgs.writeScriptBin "run-collaboration-test" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/puppeteer-tests
      ${pnpm}/bin/pnpm install --unsafe-perm
      ${pnpm}/bin/pnpm run collaboration-test
    '')
  ];

  withPuppeteerScripts = withBaseEditorScripts ++ (lib.optionals stdenv.isLinux puppeteerScripts);

  serverBaseScripts = [
    (pkgs.writeScriptBin "rebuild-cabal" ''
      #!/usr/bin/env bash
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/clientmodel/lib
      ${pkgs.haskellPackages.hpack}/bin/hpack
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      ${pkgs.haskellPackages.hpack}/bin/hpack
    '')
    (pkgs.writeScriptBin "cabal-update" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      ${cabal}/bin/cabal new-update 'hackage.haskell.org,2024-01-16T13:02:28Z'
    '')
    (pkgs.writeScriptBin "test-server-inner" ''
      #!/usr/bin/env bash
      set -e
      rebuild-cabal
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      nix-shell --run "${cabal}/bin/cabal build --disable-optimization --disable-profiling --disable-documentation --disable-library-coverage --disable-benchmarks utopia-web-test"
      cp --verbose $(${pkgs.haskellPackages.cabal-plan}/bin/cabal-plan list-bin test:utopia-web-test) .
      ./utopia-web-test +RTS -N -RTS "$@"
    '')
    (pkgs.writeScriptBin "test-server" ''
      #!/usr/bin/env bash
      set -e
      cabal-update
      test-server-inner "$@"
    '')
    (pkgs.writeScriptBin "watch-tests" ''
      #!/usr/bin/env bash
      set -e
      cabal-update
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      ${pkgs.nodePackages.nodemon}/bin/nodemon -e hs,yaml --watch src --watch test --watch package.yaml --exec test-server-inner "$@"
    '')
    (pkgs.writeScriptBin "test-server-ci" ''
      #!/usr/bin/env bash
      set -e
      cabal-update
      test-server-inner "$@"
    '')
  ];

  withServerBaseScripts = withPuppeteerScripts ++ (lib.optionals includeServerBuildSupport serverBaseScripts);

  editorRunScripts = [
    (pkgs.writeScriptBin "watch-tsc" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm run watch-tsc
    '')
    (pkgs.writeScriptBin "watch-editor-cowboy" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      RUN_COMPILER=true ${pnpm}/bin/pnpm run move-fast-and-break-things
    '')
    (pkgs.writeScriptBin "watch-editor-hmr" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      RUN_COMPILER=true ${pnpm}/bin/pnpm run dev-fast
    '')
    (pkgs.writeScriptBin "watch-editor-no-compile" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      RUN_COMPILER=false ${pnpm}/bin/pnpm run move-fast-and-break-things
    '')
    (pkgs.writeScriptBin "watch-editor-performance" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      RUN_COMPILER=true ${pnpm}/bin/pnpm run performance-test
    '')
    (pkgs.writeScriptBin "watch-editor-cowboy-danger-hot" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      RUN_COMPILER=true ${pnpm}/bin/pnpm run move-fast-and-break-things-hot
    '')
    (pkgs.writeScriptBin "watch-website" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/website-next
      ${pnpm}/bin/pnpm install
      BROWSER=none ${pnpm}/bin/pnpm run dev
    '')
  ];

  withEditorRunScripts = withServerBaseScripts ++ (lib.optionals includeRunLocallySupport editorRunScripts);

  databaseRunScripts = [
    (pkgs.writeScriptBin "create-db" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)
      PGLOCK_DIR="`pwd`/.pglock/"
      echo "Ignore previous line about database not existing." > pglog.txt
      ${postgres}/bin/createdb -e -h "$PGLOCK_DIR" utopia
    '')
    (pkgs.writeScriptBin "start-postgres-background" ''
      #!/usr/bin/env bash
      stop-postgres
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)
      PGLOCK_DIR="`pwd`/.pglock/"
      [ ! -d "utopia-db" ] && ${postgres}/bin/initdb -D utopia-db
      mkdir -p $PGLOCK_DIR
      ${postgres}/bin/pg_ctl -D utopia-db -l pglog.txt -o "--unix_socket_directories='$PGLOCK_DIR' -c log_statement=none" start
      ${postgres}/bin/psql -o /dev/null -h "$PGLOCK_DIR" -d utopia -tc "SELECT 1 FROM pg_database WHERE datname = 'utopia'" || create-db
    '')
    (pkgs.writeScriptBin "start-postgres" ''
      #!/usr/bin/env bash
      stop-postgres
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)
      start-postgres-background
      tail -f pglog.txt
    '')
    (pkgs.writeScriptBin "stop-postgres" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)
      PGLOCK_DIR="`pwd`/.pglock/"
      mkdir -p $PGLOCK_DIR
      ${postgres}/bin/pg_ctl -D utopia-db stop 
    '')
    (pkgs.writeScriptBin "run-psql" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)
      PGLOCK_DIR="`pwd`/.pglock/"
      mkdir -p $PGLOCK_DIR
      ${postgres}/bin/psql -h "$PGLOCK_DIR" -d utopia "$@"
    '')
  ];

  withDatabaseRunScripts = withEditorRunScripts ++ (lib.optionals (includeDatabaseSupport || includeRunLocallySupport) databaseRunScripts);

  serverRunScripts = [
    (pkgs.writeScriptBin "style-project" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server/src
      find . -name '*.hs' | xargs ${pkgs.haskellPackages.stylish-haskell}/bin/stylish-haskell -i
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server/test
      find . -name '*.hs' | xargs ${pkgs.haskellPackages.stylish-haskell}/bin/stylish-haskell -i
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/clientmodel/lib/src
      find . -name '*.hs' | xargs ${pkgs.haskellPackages.stylish-haskell}/bin/stylish-haskell -i
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/clientmodel/lib/test
      find . -name '*.hs' | xargs ${pkgs.haskellPackages.stylish-haskell}/bin/stylish-haskell -i
    '')
    (pkgs.writeScriptBin "run-server-inner" ''
      #!/usr/bin/env bash
      set -e
      rebuild-cabal
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/server
      pkill utopia-web || true
      nix-shell --run "${cabal}/bin/cabal new-run -j --disable-optimization --disable-profiling --disable-documentation --disable-library-coverage --disable-benchmarks utopia-web -- +RTS -N -c"
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
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/
      ${pkgs.nodePackages.nodemon}/bin/nodemon --delay 200ms -e hs,yaml --watch server/src --watch server/package.yaml --watch clientmodel/lib/src --watch clientmodel/lib/package.yaml --exec run-server-inner
    '')
  ];

  withServerRunScripts = withDatabaseRunScripts ++ (lib.optionals includeRunLocallySupport serverRunScripts);

  vscodeDevScripts = [
    (pkgs.writeScriptBin "update-vscode-patch" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${yarn}/bin/yarn
      ${yarn}/bin/yarn run make-patch
    '')
    (pkgs.writeScriptBin "watch-utopia-vscode-common" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-vscode-common
      ${pnpm}/bin/pnpm install
      ${pnpm}/bin/pnpm run watch-dev
    '')
    (pkgs.writeScriptBin "watch-utopia-vscode-extension" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/utopia-vscode-extension
      ${pnpm}/bin/pnpm install
      ${pnpm}/bin/pnpm run watch-dev
    '')
    (pkgs.writeScriptBin "pull-extension" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${yarn}/bin/yarn run pull-utopia-extension
    '')
    (pkgs.writeScriptBin "watch-vscode-build-extension-only" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/vscode-build
      ${pkgs.nodePackages.nodemon}/bin/nodemon --watch ../utopia-vscode-extension/dist/browser/extension.js --exec pull-extension
    '')
  ];

  # For the useful scripts in our dev environments
  customDevScripts = [
    (pkgs.writeScriptBin "check-tool-versions" ''
      #! /usr/bin/env nix-shell
      #! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [async process])" -i runhaskell

      import Control.Concurrent.Async
      import Control.Monad
      import Data.Foldable
      import Data.List
      import Data.Monoid
      import System.Exit
      import System.Process

      expectedToolVersions :: [(String, [String], [String])]
      expectedToolVersions =
        [ ("pnpm", ["--version"], ["7.14.2"])
        , ("yarn", ["--version"], ["1.22.19"])
        , ("ghc", ["--version"], ["The Glorious Glasgow Haskell Compilation System, version 9.0.2"])
        , ("cabal", ["--version"], ["cabal-install version 3.8.1.0", "compiled using version 3.8.1.0 of the Cabal library "])
        ]

      checkVersion :: (String, [String], [String]) -> IO All
      checkVersion (executable, arguments, expectedOutput) = do
        let commandToRun = unwords (executable : arguments)
        output <- readProcess "nix-shell" ["--run", commandToRun] ""
        let actualOutput = lines output
        let correctVersion = actualOutput == expectedOutput
        unless correctVersion $ do
          putStrLn ("Error when checking the version of " <> executable)
          putStrLn "Expected:"
          traverse_ putStrLn expectedOutput
          putStrLn "Received:"
          traverse_ putStrLn actualOutput
        pure $ All correctVersion

      main :: IO ()
      main = do 
        results <- mapConcurrently checkVersion expectedToolVersions
        let result = getAll $ mconcat results
        when result $ putStrLn "All tools are the correct version."
        if result then exitSuccess else exitFailure
    '')
    (pkgs.writeScriptBin "stop-dev" ''
      #!/usr/bin/env bash
      # Kill nodemon because it just seems to keep running.
      pkill nodemon
      pkill utopia-web
      stop-postgres
      tmux kill-session -t utopia-dev
    '')
    (pkgs.writeScriptBin "start-minimal" ''
      #!/usr/bin/env bash
      stop-dev
      set -e
      check-tool-versions
      tmux new-session -s utopia-dev \; \
        set -g default-terminal "xterm-256color" \; \
        set-option -g mouse on \; \
        new-window -n "Scratchpad" \; \
        new-window -n "Server" \; \
        send-keys -t :2 watch-server C-m \; \
        new-window -n "Editor TSC" \; \
        send-keys -t :3 watch-tsc C-m \; \
        new-window -n "Editor Vite" \; \
        send-keys -t :4 watch-editor-hmr C-m \; \
        new-window -n "Website" \; \
        send-keys -t :5 watch-website C-m \; \
        new-window -n "VSCode Common" \; \
        send-keys -t :6 watch-utopia-vscode-common C-m \; \
        new-window -n "VSCode Extension" \; \
        send-keys -t :7 watch-utopia-vscode-extension C-m \; \
        new-window -n "VSCode Pull Extension" \; \
        send-keys -t :8 watch-vscode-build-extension-only C-m \; \
        new-window -n "PostgreSQL" \; \
        send-keys -t :9 start-postgres C-m \; \
        select-window -t :1 \;
    '')
    (pkgs.writeScriptBin "start-full" ''
      #!/usr/bin/env bash
      stop-dev
      set -e
      if [ -z $GITHUB_TOKEN ]
      then
        echo "A GITHUB_TOKEN is required when running the full Utopia build. Please see the readme for instructions."
        exit 1
      else
        check-tool-versions
        build-vscode-with-extension
        install-editor
        start-minimal
      fi
    '')
    (pkgs.writeScriptBin "start-minimal-webpack" ''
      #!/usr/bin/env bash
      stop-dev
      set -e
      check-tool-versions
      tmux new-session -s utopia-dev \; \
        set -g default-terminal "xterm-256color" \; \
        set-option -g mouse on \; \
        new-window -n "Scratchpad" \; \
        new-window -n "Server" \; \
        send-keys -t :2 watch-server C-m \; \
        new-window -n "Editor TSC" \; \
        send-keys -t :3 watch-tsc C-m \; \
        new-window -n "Editor Webpack" \; \
        send-keys -t :4 watch-editor-cowboy C-m \; \
        new-window -n "Website" \; \
        send-keys -t :5 watch-website C-m \; \
        new-window -n "VSCode Common" \; \
        send-keys -t :6 watch-utopia-vscode-common C-m \; \
        new-window -n "VSCode Extension" \; \
        send-keys -t :7 watch-utopia-vscode-extension C-m \; \
        new-window -n "VSCode Pull Extension" \; \
        send-keys -t :8 watch-vscode-build-extension-only C-m \; \
        new-window -n "PostgreSQL" \; \
        send-keys -t :9 start-postgres C-m \; \
        select-window -t :1 \;
    '')
    (pkgs.writeScriptBin "start-full-webpack" ''
      #!/usr/bin/env bash
      stop-dev
      set -e
      check-tool-versions
      build-vscode-with-extension
      install-editor
      start-minimal-webpack
    '')
  ] ++ vscodeDevScripts;

  withCustomDevScripts = withServerRunScripts ++ (lib.optionals includeRunLocallySupport customDevScripts);

  # TODO Come back to these when trying to use nix to build a docker container - https://stackoverflow.com/questions/58421505/how-do-i-apply-a-nix-shell-config-in-a-docker-image
  releaseScripts = [
    (pkgs.writeScriptBin "build-editor" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/editor
      ${pnpm}/bin/pnpm install --unsafe-perm
      ${pnpm}/bin/pnpm run production
    '')
    # CRA for whatever reason will automatically fail on CI for any warnings, so we need to prefix with `CI=false`. Urgh. https://github.com/facebook/create-react-app/issues/3657
    (pkgs.writeScriptBin "build-website" ''
      #!/usr/bin/env bash
      set -e
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/website-next
      ${pnpm}/bin/pnpm install
      CI=false ${pnpm}/bin/pnpm run export
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

  linuxOnlyPackages = lib.optionals stdenv.isLinux [ pkgs.xvfb_run pkgs.xlibsWrapper pkgs.xorg.libxkbfile ];
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
    postgres
  ];

  serverRunPackages = [
  ];

  releasePackages = [
    pkgs.heroku
  ];

  pythonAndPackages = pkgs.python3.withPackages(ps: with ps; [ pyusb tkinter pkgconfig ]);

  basePackages = [ node pkgs.libsecret pythonAndPackages pkgs.pkg-config pkgs.tmux pkgs.git pkgs.wget ] ++ nodePackages ++ linuxOnlyPackages ++ macOSOnlyPackages;
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
  ELECTRON_OVERRIDE_DIST_PATH = if stdenv.isLinux then "${pkgs.electron}/bin" else null;
  # Required for webpack builds
  NODE_OPENSSL_OPTION = "--openssl-legacy-provider";
  # Required for node-gyp, apparently
  npm_config_force_process_config = true;
}
