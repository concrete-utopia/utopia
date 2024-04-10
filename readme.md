![](https://github.com/concrete-utopia/utopia/workflows/Build%20And%20Release%20On%20Push%20To%20Master/badge.svg) [![RelativeCI](https://badges.relative-ci.com/badges/CdA4FG8pCI7TO2sSFLyN?branch=master)](https://app.relative-ci.com/projects/CdA4FG8pCI7TO2sSFLyN) <!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-13-orange.svg?style=flat-square)](#contributors)

<!-- ALL-CONTRIBUTORS-BADGE:END -->

# Welcome to Utopia 🏝

Utopia is an integrated design and development environment for React. It uses React code as the source of truth, and lets you make real time changes to components by editing it and using a suite of design tools. It's early software, but you can [try it today](https://utopia.app/project), look at an [example project](https://utopia.app/p/36ae27be-welcome-to-utopia/), or read about it on our [blog](https://utopia.app/blog)!

[Try Utopia Now!](https://utopia.app/project)

![screenshot of utopia](https://user-images.githubusercontent.com/2226774/93580752-7b7b8e80-f9a0-11ea-8663-39683a53df2e.png)

[Start the editor](https://utopia.app/project)

# For contributors only: Installing Utopia on your machine

Please note: to use Utopia, visit [utopia.app](https://utopia.app/). Installing it locally is for feature development of Utopia itself: it's slower - sometimes very significantly so. It still needs connection to our servers. And you won't be able to edit projects in the file system on your machine if you install it locally.

To contribute to Utopia, you'll need to clone the repo, install the prerequisites, and then run the editor for the first time.

## Quick Start for contemporary MacOS
1. Clone the repo
2. Install Nix-Shell [nix-shell](https://nixos.org/download.html) `sh <(curl -L https://nixos.org/nix/install)`
3. Close terminal, start new terminal session, navigate to repo folder, run `nix-shell`. first run will take a while it will look like nothing will happen.
4. Generate and copy a Github Token [here](https://github.com/settings/tokens) (fine-grained or classic, longest expiration. No need to tick any of the permission checkboxes for classic)
5. First run and build: open terminal `GITHUB_TOKEN=<token from above> start-full`.
6. Wait - this step will take a while and start with (harmless) errors. Once you see the scratchpad, check the server tab - it'll take the longest (many minutes).
7. Load the editor at `http://localhost:8000/p`. 
8. Subsequent runs: `start-minimal` (this won't rebuild everything) except for server and infra-level changes
9. Editor hot-reloads and rebuilds as needed; refresh browser for asset changes
10. `stop-dev` in the Scratchpad stops the server.
11. Install [direnv](https://github.com/concrete-utopia/utopia#using-direnv-to-make-your-life-easier) to start nix-shell automatically `brew install direnv`

## Install the Prerequisites

- [nix-shell](https://nixos.org/download.html). If you are on macOS Catalina or later, you will be prompted to include an extra flag in the install script. If using Windows follow [this guide](https://nathan.gs/2019/04/12/nix-on-windows/). If you don't want to use nix, we have instructions [here](https://github.com/concrete-utopia/utopia#running-this-without-nix)
- [direnv](https://github.com/concrete-utopia/utopia#using-direnv-to-make-your-life-easier). If you don't have `direnv` installed, you'll need to run `nix-shell` before any of the `start` commands, and switching to nix will be a bit slower.

NB: If you're on Windows, you'll first need to set up the [Windows Subsystem for Linux (wsl)](https://docs.microsoft.com/en-us/windows/wsl/install-win10).

## You need to generate a `GITHUB_TOKEN`

A Github token is required to build VS Code, as there are some dependencies downloaded via the Github API which will be rate limited without one, causing the build to sporadically fail with a 403 error. To prevent that, generate a new Github token [here](https://github.com/settings/tokens) (if you have access to fine-grained tokens, use one of those, but if not then a "classic" token will be fine).

Give it the longest expiration time possible, readonly public access, and don't give it any permissions for anything else. Then copy that token somewhere so that you can use it in the next step.

## Run the Editor for the first time

The first time running the editor, run the following script:

```
GITHUB_TOKEN=<token from above> start-full
```

(You'll only need to do this once, and it will take quite some time to download and build various dependencies. After that, you can usually use `start-minimal`.)

## Working in the dev environment

Both of these scripts result in a tmux session with all of the various servers running and watching for changes. You can see all of the active sessions in the bar along the bottom, prefixed by the "window" number that they are running in. You should be able to click on each of those to switch to viewing that session, or if that doesn't work you can use the key combo `cmd`+`b` (macOS) or `ctrl`+`b` (Linux or Windows), followed by the number for that session. (see [here](https://github.com/tmux/tmux/wiki/Getting-Started#changing-the-current-window) for the relevant tmux docs)

To shut them down, in the "Scratchpad" tab of the session run the following command:

```
stop-dev
```

### Finally, loading the running application

Now the editor should load on [http://localhost:8000/p](http://localhost:8000/p), or [http://localhost:8000](http://localhost:8000/) when developing the website itself.

## Pull request bundle support.

When a series of environment variables are set (see `Branches.hs`), the editor supports the ability to get a bundle of editor code from S3 that was created from a PR, and load that instead of the code currently held locally. Which means that changes can be tested without spinning up multiple environments.

To use this if the URL currently is `https://utopia.pizza/p/e976df60-phase-rutabaga/`, the branch name would be added on in a query parameter like so: `https://utopia.pizza/p/e976df60-phase-rutabaga/?branch_name=my-test-branch`.

Limitations:

- Doesn't currently support Monaco because of the way that builds the workers in a special webpack plugin, so changes to the version of Monaco in that branch may fail in unusual ways.
- Anything that isn't editor code will not be changed by this, such as the website code or the server endpoints.

# Troubleshooting

## I'm on macOS and Nix has suddenly stopped working

Part of the nix installation will add a hook into `/etc/bashrc`, which can be wiped by a macOS update. There is an open bug ticket for that [here](https://github.com/NixOS/nix/issues/3616). If this has happened to you, you'll need to manually add that hook back in, or alternatively add it to your own `~/.zshrc` (where it won't be overwritten), copying and pasting the hook exactly as follows:

```
# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix
```

## fsevents

If you notice that 1 or more CPU cores are running 100% because of `node` processes, it is probably webpack-dev-server having trouble with `fsevents` on MacOS. To fix it, run `pnpm install fsevents` in the `utopia/editor` directory. see https://github.com/webpack/webpack/issues/701#issuecomment-216082726

## Installation error on `utopia-vscode-extensions` step

If you see an error to the effect of

```
Usage Error: The nearest package directory (/path/to/utopia/utopia-vscode-extension) doesn't seem to be part of the project declared in /path/to/utopia.

- If /path/to/utopia isn't intended to be a project, remove any yarn.lock and/or package.json file there.
- If /path/to/utopia is intended to be a project, it might be that you forgot to list utopia-vscode-extension in its workspace configuration.
- Finally, if /path/to/utopia is fine and you intend utopia-vscode-extension to be treated as a completely separate project (not even a workspace), create an empty yarn.lock file in it.
```

It could be caused by a `.yarnrc` or `.yarnrc.yml` file located in a parent directory. `.yarnrc` files are [used to configure yarn setting](https://yarnpkg.com/configuration/yarnrc) and will effect projects in all child directories below them. This can change the yarn version of utopia to one that will produce the above error. Simply removing the `.yarnrc` or `.yarnrc.yml` file will fix this error.

## Running this without Nix

We highly recommend using Nix to make life easier, but if you're having trouble with that or would prefer not to, then there is always the option to run each of the various scripts individually.

**You'll first need to build VS Code and install a few dependencies:**

```
cd [root-directory]/utopia-vscode-extension
pnpm install
pnpm run build

cd [root-directory]/vscode-build
yarn
yarn run build

cd [root-directory]/editor
pnpm install
```

**Then, to run the application, you'll need multiple things running concurrently...**
The server:

```
cd [root-directory]/server
hpack
cabal new-run -j --disable-optimization --disable-profiling --disable-documentation --disable-library-coverage --disable-benchmarks utopia-web -- +RTS -N -c"
```

PostgreSQL:

```
cd [root-directory]
PGLOCK_DIR="`pwd`/.pglock/"
mkdir -p $PGLOCK_DIR
pg_ctl -D utopia-db stop
initdb -D utopia-db
pg_ctl -D utopia-db -l pglog.txt -o "--unix_socket_directories='$PGLOCK_DIR' -c log_statement=none" start
psql -o /dev/null -h "$PGLOCK_DIR" -d utopia -tc "SELECT 1 FROM pg_database WHERE datname = 'utopia'" || create-db
tail -f pglog.txt
```

`tsc` to compile the editor:

```
cd [root-directory]/editor
pnpm tsc --watch && NODE_OPTIONS=--max_old_space_size=4096
```

Vite to run the editor:

```
cd [root-directory]/editor
pnpm run dev-fast
```

The utopia-vscode-common module:

```
cd [root-directory]/utopia-vscode-common
pnpm install
pnpm run watch-dev
```

The utopia-vscode-extension module:

```
cd [root-directory]/utopia-vscode-extension
pnpm install
pnpm run watch-dev
```

A script for pulling changes to the above extension into VS Code:

```
cd [root-directory]/vscode-build
yarn run pull-utopia-extension
```

The website project (if developing that)

```
cd [root-directory]/website-next
pnpm install
BROWSER=none pnpm run dev
```

## Using direnv to make your life easier

Since a lot of this requires using `nix-shell` everywhere, you can just use [direnv](https://direnv.net/) to make that a lot simpler. Not only will this automatically use a nix shell whenever you `cd` into the project folder, but it also adds caching to vastly speed up the opening of that shell. You can install direnv by using [brew](https://brew.sh/):

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
    local drv="$(nix --experimental-features nix-command show-derivation $out | grep -E -o -m1 '/nix/store/.*.drv')"
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

Please update your .zshrc (or .bashrc) to [hook it into the shell](https://direnv.net/docs/hook.html), for example for zsh add this line:

```
eval "$(direnv hook zsh)"
```

After this step open a new shell window and enter the utopia directory. Direnv should be activated as soon as you enter, you can use the `start` and `start-performance` scripts without manually running nix-shell.

## Unit Tests with Jest

Use the following nix scripts:

```
test-editor
```

Continuous mode:

```
test-editor-watch
```

On macOS, when trying to watch, you might run into an error message about number of open files. In that case, install watchman:

```
brew install watchman
```

## Browser tests with Karma

_Tip: If you want to focus on a specific test suite you can change `describe(...` to `describe.only(...` and if you want to run a specific test case, you can do the same with `it.only(...` ._

Use the following nix scripts:

One-off run with a headless Chrome (same as on the CI):

```
test-editor-browser
```

Debugging (bring your own Chrome):

```
test-editor-browser-debug
```

In debugging mode, Karma will be run in watch mode, and will not use a managed browser. Once the Karma webpack server is ready, it will print something like this to the terminal:

```
01 03 2022 11:20:17.412:WARN [karma]: No captured browser, open http://localhost:9876/
```

Use your own Chrome browser to navigate to the printed http address. The website will have a Debug button, or you can just append `/debug.html` to the url like so: `http://localhost:9876/debug.html` (your port may vary).

In debug mode, you can see all console logs in Chrome's own DevTools, you can use `debugger;` statements, etc.
Behind the scenes, Karma runs a webpack dev server in watch mode, which means if you change the source code, webpack will recompile and karma will attempt to re-run the tests. If the tests do not rerun for some reason, just navigate to `http://localhost:9876/debug.html` again.

(I recommend opening DevTools before navigating to `/debug.html` to make sure you don't miss `debugger;` statements and see the console output even if a test hangs the browser or enters an infinite loop.)

### Browser tests failing to run with `Error: spawn Unknown system error -86`

This is most likely because the browser tests are trying to run a headless version of Chrome built for Intel chips on an Mac with an ARM chip. If that's the case, [Rosetta 2](https://support.apple.com/en-gb/HT211861) should solve your problem:

```
softwareupdate --install-rosetta
```

## VSCode linting, formatting with prettier etc

To enable format-on-save, you should install the VSCode plugin `esbenp.prettier-vscode`, and `dbaeumer.vscode-eslint` and then in your workspace settings, enable format on save, and tell prettier to use the eslint integration mode:

```
  "eslint.workingDirectories": ["./editor", "./utopia-api"],
  "editor.formatOnSave": true,
  "prettier.useEditorConfig": false,
  "prettier.requireConfig": true,
  "[typescript]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  },
  "[javascript]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  },
  "[typescriptreact]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  },
  "[json]": {
    "editor.defaultFormatter": "esbenp.prettier-vscode"
  }

```

Select prettier as the default formatter in your settings; VSCode may prompt you to do so. The last four line items, starting with `[typescript]` reflect this. You should restart VSCode after this.

# Deploying

All pushes to the `master` branch will immediately trigger [this](.github/workflows/master-pushes.yml) workflow that runs the tests and then triggers a deploy to our Staging environment.

To deploy to the Production environment, somebody needs to manually trigger our [`tag-release.yml`](.github/workflows/tag-release.yml) workflow, giving either a specific commit hash or branch name (defaulting to `master`), and optionally a tag name (the default behaviour is to increment the patch version). This can be triggered via the "Run Workflow" button [here](https://github.com/concrete-utopia/utopia/actions?query=workflow%3A%22Tag+and+Release%22)  
**Note**: in the "Use workflow from" dropdown you have to select "Branch: master" - this is specifying which workflow to run, not which branch to cut the release from.
