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
  stdenv = pkgs.stdenv;
  shellCore = (import ./shell-core.nix) {
    inherit compiler includeServerBuildSupport includeEditorBuildSupport includeRunLocallySupport includeReleaseSupport includeDatabaseSupport;
  };
in pkgs.mkShell {
  buildInputs = shellCore; 
  # Makes the electron runner use this executable instead.
  ELECTRON_OVERRIDE_DIST_PATH = if stdenv.isLinux then "${pkgs.electron}/bin" else null;
  # Required for webpack builds
  NODE_OPENSSL_OPTION = "--openssl-legacy-provider";
  # Required for node-gyp, apparently
  npm_config_force_process_config = true;
}
