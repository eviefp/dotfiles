{ lib, config, ... }:
let
  cfg = config.evie.wayland;
in
{
  imports = [
    ./wayland/hyprland.nix
    ./wayland/hyprpaper.nix
    ./wayland/swaync.nix
    ./wayland/eww.nix
    ./wayland/screenshot.nix
    ./wayland/rofi.nix
  ];
}
