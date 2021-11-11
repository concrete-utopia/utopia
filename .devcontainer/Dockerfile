# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.205.1/containers/typescript-node/.devcontainer/base.Dockerfile

# [Choice] Node.js version (use -bullseye variants on local arm64/Apple Silicon): 16, 14, 12, 16-bullseye, 14-bullseye, 12-bullseye, 16-buster, 14-buster, 12-buster
ARG VARIANT="16-bullseye"
FROM mcr.microsoft.com/vscode/devcontainers/typescript-node:0-${VARIANT}

USER node

RUN sudo apt-get update && sudo apt-get install --no-install-recommends -y  locales curl xz-utils vim  ca-certificates direnv && sudo apt-get clean && sudo rm -rf /var/lib/apt/lists/* \
      && sudo mkdir -m 0755 /nix && sudo groupadd -r nixbld && sudo chown node /nix \
      && for n in $(seq 1 10); do sudo useradd -c "Nix build user $n" -d /var/empty -g nixbld -G nixbld -M -N -r -s "$(command -v nologin)" "nixbld$n"; done
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN set -o pipefail && curl -L https://nixos.org/nix/install | bash


# Fixes locale-related issues: https://gitlab.haskell.org/ghc/ghc/-/issues/8118
RUN locale-gen en_US.UTF-8
ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'


# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends direnv curl

# RUN whoami

# RUN curl -L https://nixos.org/nix/install | sh

# [Optional] Uncomment if you want to install an additional version of node using nvm
# ARG EXTRA_NODE_VERSION=10
# RUN su node -c "source /usr/local/share/nvm/nvm.sh && nvm install ${EXTRA_NODE_VERSION}"

# [Optional] Uncomment if you want to install more global node packages
# RUN su node -c "npm install -g <your-package-list -here>"
