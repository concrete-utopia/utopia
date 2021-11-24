FROM nixos/nix

RUN set -ex && apk --no-cache add sudo chromium 

RUN nix-channel --add https://nixos.org/channels/nixos-21.05 nixpkgs
RUN nix-channel --update

# Attempt to get the nix dependencies of the shell cached into a Docker layer.
RUN mkdir -p /root/.nix-tmp
WORKDIR /root/.nix-tmp
ADD shell.nix /root/.nix-tmp/shell.nix
ADD release.nix /root/.nix-tmp/release.nix
RUN nix-shell --run "exit"

EXPOSE 8000

# Create a group and user.
ARG DOCKER_USER
ARG DOCKER_UID
RUN addgroup -S $DOCKER_USER && adduser -S $DOCKER_USER --uid $DOCKER_UID -G $DOCKER_USER

RUN chmod -R o+rwx /root/

RUN mkdir -p /root/.cabal/
VOLUME /root/.cabal/

RUN mkdir -p /home/utopia/utopia-src
VOLUME /home/utopia/utopia-src

# This is for jest.
ENV CHROME_BIN=chromium-browser

WORKDIR /home/utopia/utopia-src

