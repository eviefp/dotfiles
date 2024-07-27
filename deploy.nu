#!/usr/bin/env nu


def run [host: string, ...cmd: string] {
  let command = ($cmd | str join ' ')
  if $host == (hostname) {
    exec ($command)
  } else {
    ssh $host -- $"cd code/dotfiles; ($command)"
  }
}

def buildHere [derivation: string, destination: string] {
  nix build ($derivation)
  nix copy -L ($derivation) --no-check-sigs --to ssh-ng://($destination)
}

def updateRemote [host: string] {
  if $host != (hostname) {
    run $host git fetch
    run $host git pull
  }
}

def compareRemotely [host: string, derivation: string] {
  updateRemote $host
  run $host nix build ($derivation)
  run $host nvd diff /run/current-system ./result
}

def runUpdate [host: string] {
  print ''
  print -n 'Switch to new config? (y/_) '
  let result = (input listen --types [key])

  print ''

  if $result.code == 'y' {
    print 'Updating...'
    run $host sudo nixos-rebuild switch "--flake" .#($host)
  } else {
    print 'Not updating.'
  }
}

def main [inputHost: string] {
  let host = if $inputHost == null { hostname } else { $inputHost }
  let derivation = $".#nixosConfigurations.($host).config.system.build.toplevel"
  let destination = $"evie@($host)"

  buildHere $derivation $destination
  compareRemotely $host $derivation
  runUpdate $host
}
