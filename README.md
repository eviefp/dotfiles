# My system configuration

This is where all nix configuration lies. I try to keep it as neat as
possible. Please let me know if you see anything dubious or that can
be improved.

## How it works

I symlink `/etc/nixos/configuration.nix` to the appropriate config
file under `system`, for example `./system/thelxinoe/configuration.nix`.

After that, I open up a nix shell (or make sure lorri/direnv are setup
properly for this directory), and run

```sh
sudo --preserve-env nixos-rebuild switch
```

For `home-manager`, I symlink ~/.config/nixpkgs/home.nix to, for example,
`home-manager/thelxinoe/home.nix` and run (from the same nix shell) as
above:

```sh
nix-shell '<home-manager>' -A install
```

As long as you just update the config file without the pins, you can
just use `home-manager switch`. If you want to update everything, then
you need to run

```sh
niv update
```

Followed by the same commands as above.

## Just show me your vim/emacs config

Vim: config/nvim/init.vim
Emacs: config/init.el


NOTE that the emacs config might need some tweaking, since it's designed
to run under `nix`.

## Structure

I am in the process of cleaning this up. Meanwhile:

The `shell.nix` exists such that I can run the commands at the top of this
file using the pinned `nixpkgs`/`home-manager` versions.

- `config` has mostly raw config files; some are no longer used
- `dev-shell` is some idea I am trying out for having a single dev shell per language, or at least a starting point; extremely early/unfleshed
- `home-manager` is the home-manager config
  - `modules` is where all the gory details are
  - `aiode` is my laptop
  - `fractal` is my home server/nas
  - `thelxinoe` is my main desktop
- `nix` is the `niv` pins
- `system` is the system config
  - `modules` is where all the gory details are
  - `aiode` is my laptop
  - `fractal` is my home server/nas
  - `thelxinoe` is my main desktop

## TODO

### system

### home-manager

### other
