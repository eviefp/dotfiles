{ dotfiles, lib, osConfig, pkgs, ... }:
{
  config = lib.mkIf (lib.elem "hyprland" osConfig.evie.wayland.compositors) {
    home = {
      file.".config/hypr/hypridle.conf".text = ''
        general {
        }

        listener {
          timeout = 600
          on-timeout = hyprctl dispatch dpms off
          on-resume = hyprctl dispatch dpms on
        }

        listener {
         timeout = 1200
         on-timeout = hyprlock
        }
      '';

      packages = [
        dotfiles.hypridle.packages.${pkgs.system}.hypridle
      ];
    };
  };
}
