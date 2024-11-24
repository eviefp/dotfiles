{ dotfiles, config, lib, pkgs, ... }:
let
  cfg = config.evie.wayland.hypridle;
in
{
  options.evie.wayland.hypridle = {
    enable = lib.mkEnableOption "hypridle defaults";
  };

  config = lib.mkIf cfg.enable {
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
