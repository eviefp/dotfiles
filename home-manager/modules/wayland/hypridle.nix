{ pkgs, hypridle, ... }:
{
  config = {
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
         timeout = 900
         on-timeout = hyprlock
        }

        listener {
          timeout = 20000
          on-timeout = systemctl suspend
        }
      '';

      packages = [
        hypridle.packages.${pkgs.system}.hypridle
      ];
    };
  };
}
