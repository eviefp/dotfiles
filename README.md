# My dotfiles

```sh
sudo --preserve-env nixos-rebuild switch
nix-shell '<home-manager>' -A install
```

## Just show me your vim/emacs config

Vim: config/nvim/init.vim
Emacs: home-manager/emacs.nix


NOTE that the emacs config will need some tweaking, since it's designed
to run under `nix`.

## Structure

I am in the process of cleaning this up. Meanwhile:

The `shell.nix` exists such that I can run the commands at the top of this
file using the pinned `nixpkgs`/`home-manager` versions.

- `config` has mostly raw config files; some are no longer used
- `dev-shell` is some idea I am trying out for having a single dev shell per language, or at least a starting point; extremely early/unfleshed
- `home-manager` is the home-manager config
  - `common.nix` is the starting point/base
  - `emacs.nix` and `init.el` might be interesting if you want to setup your emacs
  - `email.nix` is WIP, does not do smtp/sendmail yet
  - `carbon` is my laptop
  - `thelxinoe` is my main desktop
  - `desktop` is my old/defunct desktop, should clean up
  - `vm` is the old streaming VM from 2020
  - `macos` is my old work laptop
- `nix` is the `niv` pins
- `system` is the system-wide `configuration.nix` for my devices
  - `common.nix` is the starting point/base
