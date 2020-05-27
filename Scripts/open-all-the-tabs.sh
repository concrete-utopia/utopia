#!/usr/bin/env bash

PROJECT_DIR=$(git rev-parse --show-toplevel)
nix-shell --command install-editor
kitty @ set-window-title Momentum
kitty @ new-window --match title:Momentum --new-tab --title "Editor TSC" --cwd $PROJECT_DIR nix-shell --command watch-tsc
kitty @ new-window --match title:Momentum --new-tab --title "Editor Webpack" --cwd $PROJECT_DIR nix-shell --command watch-editor-cowboy
kitty @ new-window --match title:Momentum --new-tab --title "Website" --cwd $PROJECT_DIR nix-shell --command watch-website
kitty @ new-window --match title:Momentum --new-tab --title "WP Packager" --cwd $PROJECT_DIR nix-shell --command watch-webpack-packager
kitty @ new-window --match title:Momentum --new-tab --title "WP DLL" --cwd $PROJECT_DIR nix-shell --command watch-webpack-dll
kitty @ new-window --match title:Momentum --new-tab --title "Mongo" --cwd $PROJECT_DIR nix-shell --command run-mongodb
kitty @ new-window --match title:Momentum --new-tab --title "Redis" --cwd $PROJECT_DIR nix-shell --command redis-server
kitty @ new-window --match title:Momentum --new-tab --title "Server" --cwd $PROJECT_DIR nix-shell --command watch-server
