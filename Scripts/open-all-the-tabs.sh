#!/usr/bin/env bash

PROJECT_DIR=$(git rev-parse --show-toplevel)
nix-shell --command install-editor
nix-shell --command build-vscode
kitty @ set-window-title Momentum
kitty @ new-window --match title:Momentum --new-tab --title "Editor TSC" --cwd $PROJECT_DIR direnv exec . nix-shell --run watch-tsc
kitty @ new-window --match title:Momentum --new-tab --title "Editor Webpack" --cwd $PROJECT_DIR direnv exec . nix-shell --run watch-editor-cowboy
kitty @ new-window --match title:Momentum --new-tab --title "Website" --cwd $PROJECT_DIR direnv exec . nix-shell --run watch-website
kitty @ new-window --match title:Momentum --new-tab --title "Server" --cwd $PROJECT_DIR direnv exec . nix-shell --run watch-server
kitty @ new-window --match title:Momentum --new-tab --title "VS Code" --cwd $PROJECT_DIR direnv exec . nix-shell --run watch-vscode-dev
