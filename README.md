# My system configuration
Please let me know if you see anything dubious or that can be improved.
## How it works

On new systems, start with `home-manager`.

Start with opening a nix shell here:

```sh
nix-shell
```

Make sure your `NIX_PATH` is set before continuing.

For `home-manager`, I symlink ~/.config/home-manager/home.nix to, for example,
`home-manager/thelxinoe/home.nix` and run:
```sh
nix-shell '<home-manager>' -A install
```

Restart the shell; your `NIX_PATH` should now be setup such that everything
"just works"L

```sh
sudo --preserve-env nixos-rebuild switch
```

As long as you just update the config file without the pins, you can
just use `home-manager switch`. If you want to update everything, then
you need to run

```sh
niv update
```

Followed by the same commands as above.

## Updating System Version

1. change the `nixpkgs` pin
2. update ./system/modules/boot.nix `system.stateVersion` accordingly
3. switch to the new config

## Just show me your vim/emacs config

Vim: config/nvim/lua/*.lua (*)
Emacs: config/init.el

(*) My `init.lua` file is generated by nix as a require to all files under that directory. See `neovim.nix` for details.

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
- [ ] rofi config
  - [ ] use same colors as eww
  - [ ] transparent background
- [ ] eww
  - [ ] statusbar
    - [ ] tray?
	- [ ] not sure I love the icons
	- [ ] turn on tv?
	- [ ] clicking on cpu/memory/etc maybe opens more info?
  - [ ] another window that's some sort of overview?
- [ ] swaync config
  - [ ] I did nothing to configure it so far
- [ ] hyprland
  - [ ] styling/appearance
  - [ ] workspace rules
  - [ ] dwindle layout
  - [ ] animations
  - [ ] master layout
  - [ ] xmonad-like binds
    - [ ] main window button
    - [ ] maybe default do a 70/30 split or something?
    - [ ] which other bindings am I missing?
  - [ ] screen sharing
- [ ] qutebrowser eating mouse
- [ ] keyboard stuff, kmonad maybe?
- [ ] screen locking and/or dpms
- [ ] screenshot
- [ ] clipboard https://wiki.hyprland.org/Useful-Utilities/Clipboard-Managers/


- awesome wayland https://github.com/natpen/awesome-wayland
- other https://wiki.hyprland.org/Useful-Utilities/Other/
