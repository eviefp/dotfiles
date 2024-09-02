{ dotfiles, lib, config, pkgs, osConfig, ... }:
let
  cfg = config.evie.wayland;
  files = [ "nomi.jpg" "rev.jpg" "c.jpg" "evey.png" ];
  preload = lib.lists.foldr (w: acc: "${acc}\n preload = ~/.config/wallpaper/${w}") "" files;

  mkWallpaper = p: "wallpaper = ${p.fst.name}, ~/.config/wallpaper/${p.snd}";
  wallpaper = lib.lists.foldr (p: conf: "${conf}\n${mkWallpaper p}") "" (lib.lists.zipLists cfg.monitors files);
in
{
  config = lib.mkIf (lib.elem "hyprland" osConfig.evie.wayland.compositors) {
    home = {
      file.".config/wallpaper" = {
        source = ../../../../config/wallpapers;
        recursive = true;
      };

      file.".config/hypr/hyprpaper.conf".text = ''
        ${preload}
        ${wallpaper}
      '';

      packages = [
        dotfiles.hyprpaper.packages.${pkgs.system}.hyprpaper
      ];
    };
  };
}

