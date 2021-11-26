let
  release = (import ../release.nix {});
  pkgs = release.pkgs;
  compiler = "ghc8107";
  utopia-server = pkgs.haskell.packages.${compiler}.callCabal2nix "utopia-server" ./. {};
  haskell = pkgs.haskell.lib;

  server-without-profile = haskell.disableLibraryProfiling (haskell.disableExecutableProfiling utopia-server);

  server-disabled-database-tests = haskell.appendConfigureFlag server-without-profile "--flags -enable-external-tests";

  server-tested = haskell.disableOptimization server-disabled-database-tests;

  server-shell = haskell.addBuildTools server-tested [pkgs.nodejs pkgs.yarn]; 

  server-dev = haskell.dontCheck server-tested;
in
  {
    server-dev = server-dev;
    server-shell = server-shell;
    server-tested = server-tested;
  }
