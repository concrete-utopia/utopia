let
  release = (import ./release.nix {});
  pkgs = release.pkgs;
  stdenv = pkgs.stdenv;

  haskellImage = pkgs.dockerTools.pullImage {
    imageName = "haskell";
    imageDigest = "sha256:2351d1ed56b95add00b8820d1aa7bc1a7f1a277989f7f4de41f01b065fbe5ee8";
    sha256 = "sha256-oH1mbIBAF/dRjB0pOj6tFLTvQihnlPzETijxJNCPwok=";
  };

  dockerBuild = pkgs.dockerTools.buildImage {
    name = "hello-docker";
    fromImage = haskellImage;
    fromImageTag = "9.0.2";
    copyToRoot = [
      ./dist
    ];
    config = {
      Env = [
        "APP_ENVIRONMENT=PRODUCTION"
        "PROXY_WEBPACK=FALSE"
        "WEBPACK_DLL_ENV=webpack-dll-dev" 
        "UTOPIA_SHA=$UTOPIA_SHA"
      ];
      Cmd = [ "run-server-production.sh" ];
    };
  };

in dockerBuild