![](https://github.com/concrete-utopia/utopia/workflows/Build%20And%20Release%20On%20Push%20To%20Master/badge.svg) <!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-13-orange.svg?style=flat-square)](#contributors)

<!-- ALL-CONTRIBUTORS-BADGE:END -->

# Welcome to Utopia!

Utopia runs in the browser! To run it, you'll need to run the server (locally) and webpack.

## Prerequisites

- [NIX](http://nixos.org/nix)
- [nix-shell](https://nixos.org/download.html). Scroll down if you're on Catalina.
- Optionally: [direnv](https://direnv.net/docs/installation.html). Scroll down for setup instructions. If you don't have `direnv` installed, you'll need to run `nix-shell` before any of the `start` commands.

## Contributing and Bug Reporting

We welcome contributions. Utopia is a big project with a learning curve, but we're here to help. The easiest way is to file an issue, or reach out on [Discord](https://discord.gg/pD8SrEJ). Please read our [contributing](contributing.md) doc to get started

# Run the Editor

You can use a handful of commands to run the editor and server. Scroll down for more advanced options.

## Dev Mode: slower performance, full error messages, easier to debug

```
start
```

and in a second terminal, for typescript compile errors

```
watch-tsc
```

## Fast Mode: fast performance, minified react errors

Run everything in _fast_ mode, performance is fast, react will throw minified errors:

```
start-performance
```

and in a second terminal, for typescript compile errors

```
watch-tsc
```

## Run everything in _hot and brittle_ mode.

> it's the same as start but webpack is in hot mode (under the hood it runs npm run move-fast-and-break-things-hot instead of npm run move-fast-and-break-things). I've been using this for the last 4-5 days or so. hot mode still works if you are working on for example the inspector design. the canvas is pretty brittle to hot updates, every 4th update made my canvas turn white but for the purposes of the inspector tweaking it was fine

```
start-hot-only-ui-work
```

## Run each part separately

(each series of commands assumes that you're starting from the root directory of the project)

### Running the server:

Shell 1:

```
nix-shell
redis-server
```

Shell 2:

```
nix-shell
watch-server
```

If this ever fails with `truncated tar archive`, it's a cabal failure. The fix appears to be to delete `~/.cabal/packages`

### Running the packagers

Shell 3:

```
nix-shell
start-packager
```

### Running the editor in dev mode (slow but you see react errors)

Shell 4:

```
nix-shell
watch-tsc
```

Shell 5:

```
nix-shell
watch-editor-cowboy
```

### Running the editor in fast mode

Shell 4:

```
nix-shell
watch-tsc
```

Shell 5:

```
nix-shell
watch-editor-performance
```

Alternatively you can run `npm run webpack-production`. Occasionally you'll need to run `npm install` from the `/editor` directory before webpack.

### Running webpack with the website

Shell 6:

```
nix-shell
watch-website
```

# Troubleshooting

- If you have fsevents trouble, first nuke your node_modules

## Running on Catalina with Nix

Things seem to be changing every day but for now, our solution is taken from here:
https://github.com/NixOS/nix/issues/2925#issuecomment-539490866

```
sudo mkdir /System/Volumes/Data/opt/nix
sudo chown {your_user} /System/Volumes/Data/opt/nix
# Be careful as the space needs to be a tab, otherwise changes won't be picked up from synthetic.conf
sudo sh -c "echo 'nix	System/Volumes/Data/opt/nix' >> /System/Volumes/Data/private/etc/synthetic.conf"
```

^ make sure you replace the spaces with a tab character as directed above **or this will NOT work**.

Reboot, and then:

```
curl https://nixos.org/nix/install | sh
```

Add the `. /Users/{{USERNAME}}/.nix-profile/etc/profile.d/nix.sh` line to your shell's config file (e.g. ~/.zshrc, ~/.bashrc) as directed by the end of the nix install.

And while you're at it, this as well, which will supress the error that you're using a symlink instead of a real directory: `export NIX_IGNORE_SYMLINK_STORE=1`

### Running this without Nix

You'll need five things running concurrently:

```
editor/npm run webpack
editor/npx tsc --watch
Website/npm start
redis-server
server/cabal new-run --disable-optimization --disable-profiling --disable-documentation --disable-library-coverage --disable-benchmarks utopia-web -- +RTS -N

```

## Using direnv to make your life easier

Since a lot of this requires using `nix-shell` everywhere, you can just use [direnv](https://direnv.net/) to make that a lot simpler. Not only will this automatically use a nix shell whenever you `cd` into the project folder, but it also adds caching to vastly speed up the opening of that shell. You can install direnv by using brew:

```
brew install direnv
```

To actually run direnv you need to hook it into your shell by following the instructions [here](https://direnv.net/docs/hook.html).

Then to configure it, in your \$HOME directory add a file `.direnvrc` with the following contents (copied from https://github.com/direnv/direnv/wiki/Nix#using-a-global-use_nix-with-garbage-collection-prevention):

```
use_nix() {
  local path="$(nix-instantiate --find-file nixpkgs)"

  if [ -f "${path}/.version-suffix" ]; then
    local version="$(< $path/.version-suffix)"
  elif [ -f "${path}/.git" ]; then
    local version="$(< $(< ${path}/.git/HEAD))"
  fi

  local cache=".direnv/cache-${version:-unknown}"

  local update_drv=0
  if [[ ! -e "$cache" ]] || \
    [[ "$HOME/.direnvrc" -nt "$cache" ]] || \
    [[ .envrc -nt "$cache" ]] || \
    [[ default.nix -nt "$cache" ]] || \
    [[ shell.nix -nt "$cache" ]];
  then
    [ -d .direnv ] || mkdir .direnv
    nix-shell --show-trace --pure "$@" --run "\"$direnv\" dump bash" > "$cache"
    update_drv=1
  else
    log_status using cached derivation
  fi
  local term_backup=$TERM path_backup=$PATH
  if [ -n ${TMPDIR+x} ]; then
    local tmp_backup=$TMPDIR
  fi

  eval "$(< $cache)"
  export PATH=$PATH:$path_backup TERM=$term_backup TMPDIR=$tmp_backup
  if [ -n ${tmp_backup+x} ]; then
    export TMPDIR=${tmp_backup}
  else
    unset TMPDIR
  fi

  # `nix-shell --pure` sets invalid ssl certificate paths
  if [ "${SSL_CERT_FILE:-}" = /no-cert-file.crt ]; then
    unset SSL_CERT_FILE
  fi
  if [ "${NIX_SSL_CERT_FILE:-}" = /no-cert-file.crt ]; then
    unset NIX_SSL_CERT_FILE
  fi

  # This part is based on https://discourse.nixos.org/t/what-is-the-best-dev-workflow-around-nix-shell/418/4
  if [ "$out" ] && (( $update_drv )); then
    local drv_link=".direnv/drv"
    local drv="$(nix show-derivation $out | grep -E -o -m1 '/nix/store/.*.drv')"
    local stripped_pwd=${PWD/\//}
    local escaped_pwd=${stripped_pwd//-/--}
    local escaped_pwd=${escaped_pwd//\//-}
    ln -fs "$drv" "$drv_link"
    ln -fs "$PWD/$drv_link" "/nix/var/nix/gcroots/per-user/$LOGNAME/$escaped_pwd"
    log_status renewed cache and derivation link
  fi

  if [[ $# = 0 ]]; then
    watch_file default.nix
    watch_file shell.nix
  fi
}
```

And add a `.envrc` file to the root folder of the project with the following contents:

```
use nix
```

(This file is deliberately contained in the `.gitignore` because it is supposed to be personal to you - it allows you to add custom environment variables that will always be in scope whenever you're in this directory)

Please update your .bashrc (or .zsrc) to [hook it into the shell](https://direnv.net/docs/hook.html), for example for bash add this line:

```
eval "$(direnv hook bash)"
```

After this step open a new shell window and enter the utopia directory. Direnv should be activated as soon as you enter, you can use the `start` and `start-performance` scripts without manually running nix-shell.

## Unit Tests with Jest

One-off test run:

```
npm run test
```

Continuous mode:

```
npm run test-watch
```

On macOS, when trying to watch, you might run into an error message about number of open files. In that case, install watchman:

```
brew install watchman
```

## VSCode format on save

To enable format-on-save, you should install the VSCode plugin `esbenp.prettier-vscode`, and `dbaeumer.vscode-eslint` and then in your workspace settings, enable format on save, and tell prettier to use the eslint integration mode:

```
"editor.formatOnSave": true,
"prettier.eslintIntegration": true,
```

When VSCode asks you to pick a formatter, just pick Prettier. If it wouldn't ask you for any reason, you can store your preferences like so (for minimal impact put it in your workspace settings):

```
"[typescript]": {
  "editor.defaultFormatter": "esbenp.prettier-vscode"
},
"[typescriptreact]": {
  "editor.defaultFormatter": "esbenp.prettier-vscode"
},
"[javascript]": {
  "editor.defaultFormatter": "esbenp.prettier-vscode"
},
```

## Running the Server

To run the server, checkout the [readme](server/readme.md) in the `/server` directory
