{ dotfiles, lib, osConfig, pkgs, ... }:
{
  config = lib.mkIf (osConfig.evie.wayland.compositor == "hyprland") {
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
