FROM mcr.microsoft.com/vscode/devcontainers/base:hirsute

# RUN set -ex && apk --no-cache add sudo chromium
RUN apt-get update
RUN apt-get install --no-install-recommends -y nix-bin


# I commented this out && sudo chown $DOCKER_USER /nix
RUN sudo mkdir -m 0755 /nix && sudo mkdir -p /etc/nix && sudo groupadd -r nixbld \
      && for n in $(seq 1 10); do sudo useradd -c "Nix build user $n" -d /var/empty -g nixbld -G nixbld -M -N -r -s "$(command -v nologin)" "nixbld$n"; done

RUN echo 'filter-syscalls = false' >> /etc/nix/nix.conf
RUN nix-channel --add https://nixos.org/channels/nixos-21.05 nixpkgs
RUN nix-channel --update

# Attempt to get the nix dependencies of the shell cached into a Docker layer.
RUN mkdir -p /root/.nix-tmp
WORKDIR /root/.nix-tmp
ADD shell.nix /root/.nix-tmp/shell.nix
ADD release.nix /root/.nix-tmp/release.nix
RUN nix-shell --run "exit"

EXPOSE 8000

RUN chmod -R o+rwx /root/

RUN mkdir -p /root/.cabal/
VOLUME /root/.cabal/

# This is for jest.
ENV CHROME_BIN=chromium-browser

RUN apt-get install --no-install-recommends -y locales
ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'
RUN locale-gen en_US.UTF-8
RUN dpkg-reconfigure locales
# RUN update-locale LANG='en_US.UTF-8'

# install Direnv for the vscodeuser user
RUN apt-get install --no-install-recommends -y direnv
ADD .direnvrc /home/vscode/.direnvrc

RUN echo 'export TMPDIR=/tmp' > ~/.bashrc
RUN echo 'export TMPDIR=/tmp' > /home/$DOCKER_USER/.bashrc
