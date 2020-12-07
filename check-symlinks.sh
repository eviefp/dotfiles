#!/usr/bin/env bash
set -u
set -o pipefail

env="macos"
machine=""

outputs=(
  "${HOME}/.config/nvim/init.vim"
  "${HOME}/.config/nvim/coc-settings.json"
  "${HOME}/.config/nixpkgs/home.nix"
  "${HOME}/.ghci"
  "${HOME}/.reddup.yml"
  "${HOME}/.config/nixpkgs/home.nix"
  "${HOME}/.config/fish/functions/clip.fish"
  "${HOME}/.config/kitty/kitty.conf"
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

success() {
    echo -e "\e[32m[√]\e[0m $1"
}

fail() {
    echo -e "\e[31m[x]\e[0m $1"
}

info() {
    echo -e "\e[33m[≈]\e[0m $1"
}

setEnv() {
    unameOut="$(uname -s)"
    case "${unameOut}" in
        Linux*)     env=nixos;;
        Darwin*)    env=macos;;
        *)          env="UNKNOWN:${unameOut}"
    esac
}

setMachine() {
    hostOut="$(hostname)"
    case "${hostOut}" in
        "vm")                machine=vm;;
        "laptop")            machine=laptop;;
        "APM3LC02CD0J3MD6M") machine=macos;;
        *)                   machine="UNKNOWN:${hostOut}"
    esac
}

checkAll() {
    for i in "${!inputs[@]}"
    do
        check_file $i
        case "$?" in
            "1")
                success "${inputs[$i]}"
                ;;
            "2")
                ln -s "$(pwd)/${inputs[$i]}" "${outputs[$i]}" 2> /dev/null
                if [ "$?" -eq 0 ]; then
                    success "Created symlink for ${inputs[$i]} at ${outputs[$i]}."
                else
                    fail "Failed to create symlink for ${inputs[$i]} at ${outputs[$i]}."
                fi
                ;;
            "3")
                info "Skipping ${inputs[$i]}..."
        esac
    done
    if [ "$env" = "nixos" ]; then
        if [ -L "/etc/nixos/configuration.nix" ]; then
            success "/etc/nixos.configuration.nix"
        else
            fail "Please setup configuration.nix symlink!"
        fi

        if [ -L "${HOME}/.config/nixpkgs/home.nix" ]; then
            success "./home-manager/$machine/home.nix"
        else
            ln -s "./home-manager/$machine/home.nix" "${HOME}/.config/nixpkgs/home.nix" 2> /dev/null
            success "Created symlink for ./home-manager/$machine/home.nix at ${HOME}/.config/nixpkgs/home.nix"
        fi
    fi

}

main() {
    echo ""
    setEnv
    setMachine

    inputs=(
        "config/nvim/init.vim"
        "config/nvim/coc-settings.json"
        "home-manager/$machine/home.nix"
        "home/ghci"
        "home/reddup-$machine.yaml"
        "home-manager/$machine/home.nix"
        "config/fish/functions/clip.fish"
        "config/kitty/kitty.conf"
    )
    echo -e "\e[35m${env}\e[0m/\e[36m${machine}\e[0m detected..."
    checkAll
}

main
