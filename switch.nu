#!/usr/bin/env nu

let host = (hostname)

print "Building new config..."
nix build .#nixosConfigurations.($host).config.system.build.toplevel --show-trace

# nix store diff-closures /run/current-system ./result
nvd diff /run/current-system ./result


print -n 'Switch to new config? (y/_) '
let result = (input listen --types [key])

print ''

if $result.code == 'y' {
  print 'Updating...'
  sudo nixos-rebuild switch --flake .#($host)
} else {
  print 'Not updating.'
}

# nix copy -L .#nixosConfigurations.($host).config.system.build.toplevel --no-check-sigs --to ssh-ng://evie@($host)
