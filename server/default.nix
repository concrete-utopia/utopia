{}:

let
  release = (import ../release.nix {});
  nixpkgs = release.pkgs;
  inherit (nixpkgs) stdenv lib;
  compiler = "ghc902";
  utopia-server = nixpkgs.haskell.packages.${compiler}.callCabal2nix "utopia-server" ./. {};
  haskell = nixpkgs.haskell.lib;
  trivial = lib.trivial;
  disableProfiling = pkg: haskell.disableLibraryProfiling (haskell.disableExecutableProfiling pkg);
  disableExternalTests = pkg: haskell.appendConfigureFlag pkg "--flags -enable-external-tests";
  withNodeTooling = pkg: haskell.addBuildTools pkg [nixpkgs.nodejs nixpkgs.yarn];
  serverModifications = pkg: trivial.pipe pkg [disableProfiling disableExternalTests withNodeTooling];
  overriddenHaskellPackages = nixpkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      opaleye = serverModifications (nixpkgs.haskell.lib.overrideCabal (super.callHackage "opaleye" "0.9.6.1" {}) {
        doCheck = false;
      });
      serversession = serverModifications (super.callHackage "serversession" "1.0.2" {});
      utopia-clientmodel = serverModifications (super.callCabal2nix "utopia-clientmodel" (../clientmodel/lib) {});
      utopia-web = serverModifications (super.callCabal2nix "utopia-web" (./.) {});
    };
  };
  migrations = stdenv.mkDerivation {
    name = "utopia-server-migrations-1.0";
    src = ./migrations;
    installPhase = ''
      mkdir $out
      cp -R * $out/
    '';
  };
in
  {
    utopia-web = overriddenHaskellPackages.utopia-web;
    migrations = migrations;
  }
