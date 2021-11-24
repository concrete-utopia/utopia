#!/usr/bin/env bash
set -e

PROJECT_ROOT=`git rev-parse --show-toplevel`
DOCKER_DEV_ENV_DIR=$PROJECT_ROOT/docker-dev-env
DOCKER_CACHE_DIR=$DOCKER_DEV_ENV_DIR/.cache
CABAL_CACHE_DIR=$DOCKER_CACHE_DIR/.cabal
DOCKER_USER=`whoami`
DOCKER_UID=`id -u`

# Create the cache directories that Docker can use between sessions.
mkdir -p $CABAL_CACHE_DIR

# Copy the nix files into here so they can be used inside the Dockerfile.
cp $PROJECT_ROOT/shell.nix $DOCKER_DEV_ENV_DIR
cp $PROJECT_ROOT/release.nix $DOCKER_DEV_ENV_DIR

echo $DOCKER_DEV_ENV_DIR
echo $DOCKER_USER

# Build the Docker image.
docker build \
  --tag utopia-dev-env \
  --build-arg DOCKER_USER=$DOCKER_USER \
  --build-arg DOCKER_UID=$DOCKER_UID \
  $DOCKER_DEV_ENV_DIR

# Run a shell within the Docker image produced.
docker run \
  --interactive \
  --tty \
  --publish 8000:8000 \
  --volume $PROJECT_ROOT:/home/utopia/utopia-src \
  --volume $CABAL_CACHE_DIR:/root/.cabal \
  utopia-dev-env \
  nix-shell --run "sudo --preserve-env --user $DOCKER_USER bash"
