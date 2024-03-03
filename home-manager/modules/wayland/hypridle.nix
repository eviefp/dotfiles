{ lib, config, pkgs, hypridle, ... }:
let
  cfg = config.evie.wayland;
  files = [ "elsa.png" "evey.png" "hq.png" "mulan.png" ];
  preload = lib.lists.foldr (w: acc: "${acc}\n preload = ~/.config/wallpaper/${w}") "" files;

  mkWallpaper = p: "wallpaper = ${p.fst.name}, ~/.config/wallpaper/${p.snd}";
  wallpaper = lib.lists.foldr (p: conf: "${conf}\n${mkWallpaper p}") "" (lib.lists.zipLists cfg.monitors files);
in
{
  config = lib.mkIf cfg.enable {
    home = {
      file.".config/hypr/hypridle.conf".text = ''
        general {
        }

        # listener {
        #   timeout = 300
        #   on-timeout = hyprlock
        # }

        listener {
          timeout = 600
          on-timeout = hyprctl dispatch dpms off
          on-resume = hyprctl dispatch dpms on
        }

        # listener {
        #   timeout = 3600
        #   on-timeout = systemctl suspend
        # }
      '';

      packages = [
        hypridle.packages.${pkgs.system}.hypridle
      ];
    };
  };
}
