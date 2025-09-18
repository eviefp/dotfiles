# My system configuration

Please let me know if you see anything dubious or that can be improved.

## Basic setup

This is a flake-based system configuration for my NixOS devices, with
system-wide settings being setup through NixOS modules and personal/user
settings being configured through the NixOS home-manager module.

The directory structure is:

- `config` are random configuration files that are used in nix modules
- `hosts` contain the nixos/darwin/home-manager entry modules
- `lib` has a couple of nix helpers
- `modules` holds `home-manager` and `nixos` modules
- `packages` holds a few self-sourced packages, overrides, and scripts
- `scripts` holds a few trivial nushell scripts, an awk script, and a playground
  for haskell scripts
- `secrets` holds the sops secrets

## Initial setup

You will probably want to pick one of my devices as an example:

- `thelxinoe` is my desktop
- `arche` is a desktop with an nvidia gfx (they're finnicky!)
- `apate` is my work macbook pro (hey, not my choice)
- `janus` and `aiode` are my laptops
- `fractal` is my headless home server
- `jellyfin` is a hetzner VM

```bash
sudo nixos-rebuild switch --flake .#newname
```

If you want to update the flake pins, you can run `nix flake update`.

## Neovim/Emacs/Helix

Easy access to configs:

- [neovim.nix](./modules/home-manager/editors/neovim.nix)
- [emacs.nix](./modules/home-manager/editors/emacs.nix) and [init.el](./config/init.el)
- [helix.nix](./modules/home-manager/editors/helix.nix)

## Sops stuff

For age keys

```bash
nix-shell -p ssh-to-age
cat public_ed25519 | ssh-to-age # use this as the public key in the sops file
cat private_ed25519 | ssh-to-age -private-key # put this in ~/.config/sops/age/keys.txt
```
