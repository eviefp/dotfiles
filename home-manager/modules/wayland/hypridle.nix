{ lib, config, pkgs, hypridle, ... }:
let
  cfg = config.evie.wayland;
in
{
  config = lib.mkIf cfg.enable {
    home = {
      file.".config/hypr/hypridle.conf".text = ''
        general {
        }

        # listener {
        #  timeout = 300
        #  on-timeout = hyprlock
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
