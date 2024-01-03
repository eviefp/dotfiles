{ lib, config, pkgs, hyprpaper, ... }:
let
  cfg = config.evie.wayland.hyprpaper;
in
{
  config = {
    home = {
      file.".config/wallpaper" = {
        source = ../../../config/wallpapers;
        recursive = true;
      };

      file.".config/hypr/hyprpaper.conf".text = ''
        preload = ~/.config/wallpaper/elsa.png
        preload = ~/.config/wallpaper/evey.png
        preload = ~/.config/wallpaper/hq.png
        preload = ~/.config/wallpaper/mulan.png

        wallpaper = DP-1,~/.config/wallpaper/elsa.png
        wallpaper = DP-2,~/.config/wallpaper/evey.png
        wallpaper = DP-3,~/.config/wallpaper/hq.png
        wallpaper = HDMI-A-2,~/.config/wallpaper/mulan.png
      '';

      packages = [
        hyprpaper.packages.${pkgs.system}.hyprpaper
      ];
    };
  };
}
