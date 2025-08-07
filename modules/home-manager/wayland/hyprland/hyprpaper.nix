{ dotfiles, lib, config, pkgs, ... }:
let
  cfg = config.evie.wayland.hyprpaper;
  wayland = config.evie.wayland;
  files = lib.lists.drop cfg.wallpaperSkip (lib.lists.flatten (lib.lists.replicate 10 [ "jinx-dory.jpg" "tg.jpg" "girl-portrait.jpg" "c.jpg" "nomi.jpg" ]));
  preload = lib.lists.foldr (w: acc: "${acc}\n preload = ~/.config/wallpaper/${w}") "" files;

  mkWallpaper = p: "wallpaper = ${p.fst.name}, ~/.config/wallpaper/${p.snd}";
  wallpaper = lib.lists.foldr (p: conf: "${conf}\n${mkWallpaper p}") "" (lib.lists.zipLists wayland.monitors files);
in
{
  options.evie.wayland.hyprpaper = {
    enable = lib.mkEnableOption "hyprpaper defaults";

    wallpaperSkip = lib.mkOption {
      type = lib.types.int;
      default = 0;
    };

  };

  config = lib.mkIf cfg.enable {
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

