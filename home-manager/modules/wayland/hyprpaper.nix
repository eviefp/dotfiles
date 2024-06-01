{ lib, config, pkgs, hyprpaper, ... }:
let
  cfg = config.evie.wayland;
  files = [ "evey.png" "elsa.png" "hq.png" "mulan.png" ];
  preload = lib.lists.foldr (w: acc: "${acc}\n preload = ~/.config/wallpaper/${w}") "" files;

  mkWallpaper = p: "wallpaper = ${p.fst.name}, ~/.config/wallpaper/${p.snd}";
  wallpaper = lib.lists.foldr (p: conf: "${conf}\n${mkWallpaper p}") "" (lib.lists.zipLists cfg.monitors files);
in
{
  config = {
    home = {
      file.".config/wallpaper" = {
        source = ../../../config/wallpapers;
        recursive = true;
      };

      file.".config/hypr/hyprpaper.conf".text = ''
        ${preload}
        ${wallpaper}
      '';

      packages = [
        hyprpaper.packages.${pkgs.system}.hyprpaper
      ];
    };
  };
}

