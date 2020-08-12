#!/usr/bin/env bash

# TODO: Figure this out.
env="macos"

outputs=(
  "${HOME}/.config/nvim/init.vim"
  "${HOME}/.config/nvim/coc-settings.json"
  "${HOME}/.config/nixpkgs/home.nix"
  "${HOME}/.ghci"
  "${HOME}/.reddup.yml"
  "/etc/nixos/configuration.nix"
  "${HOME}/config/fish/functions/clip.fish"
  "${HOME}/.config/kitty/kitty.conf"
)

inputs=(
    "./config/nvim/init.vim"
    "./config/nvim/coc-settings.json"
    "./home-manager/$env/home.nix"
    "./home/ghci"
    "./home/reddup-$env.yaml"
    "./nix/$env/configuration.nix"
    "~/.config/fish/functions/clip.fish"
    "~/.config/kitty/kitty.conf"
)

should_check() {
    # All systems
    if [ "$1" -lt "5" ]; then
        return $1
    elif [ "$env" = "nixos" ]; then
        return $1
    else
        return 100
    fi
}

check_file() {
    should_check $1
    if [ "$?" -lt 99 ]; then
        if [ -L "${outputs[$1]}" ]; then
            return 1
        else
            return 2
        fi
    else
        return 3
    fi
}

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     env=nixos;;
    Darwin*)    env=macos;;
    *)          env="UNKNOWN:${unameOut}"
esac
echo "Running checks for ${env}..."

for i in "${!inputs[@]}"
do
    check_file $i
    case "$?" in
        "1")
            echo "[√] ${inputs[$i]}"
            ;;
        "2")
            ln -s "${inputs[$i]}" ${outputs[$i]}
            echo "[˜] Created symlink for ${inputs[$i]} at ${outputs[$i]}."
            ;;
        "3")
            echo "[≈] Skipping ${inputs[$i]}..."
    esac
done

