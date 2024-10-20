let
  release = (import ../release.nix {});
  pkgs = release.pkgs;
  recentPkgs = release.recentPkgs;
  # we set only the running Node to be 20.x, for project dependencies
  node = recentPkgs.nodejs_20;
  yarn = recentPkgs.yarn.override { nodejs = node; };
  compiler = "ghc902";
  utopia-server = pkgs.haskell.packages.${compiler}.callCabal2nix "utopia-server" ./. {};
  haskell = pkgs.haskell.lib;
  trivial = pkgs.lib.trivial;
  disableProfiling = pkg: haskell.disableLibraryProfiling (haskell.disableExecutableProfiling pkg);
  disableExternalTests = pkg: haskell.appendConfigureFlag pkg "--flags -enable-external-tests";
  withNodeTooling = pkg: haskell.addBuildTools pkg [node yarn];
  serverModifications = pkg: trivial.pipe pkg [disableProfiling disableExternalTests withNodeTooling];
  overriddenHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      opaleye = pkgs.haskell.lib.overrideCabal (super.callHackage "opaleye" "0.9.6.1" {}) {
        doCheck = false;
      };
      utopia-clientmodel = super.callCabal2nix "utopia-clientmodel" (../clientmodel/lib) {};
      utopia-web = serverModifications (super.callCabal2nix "utopia-web" (./.) {});
    };
  };
in
  overriddenHaskellPackages.utopia-web
