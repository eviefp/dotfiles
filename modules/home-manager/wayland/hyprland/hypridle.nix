{ dotfiles, config, lib, pkgs, ... }:
let
  cfg = config.evie.wayland.hypridle;
in
{
  options.evie.wayland.hypridle = {
    enable = lib.mkEnableOption "hypridle defaults";
    lock = lib.mkEnableOption "hyprlock on idle?";
  };

  config = lib.mkIf cfg.enable {
    home = {
      file.".config/hypr/hypridle.conf".text =
        lib.concatStringsSep "\n"
          [
            ''
              listener {
                timeout = 600
                on-timeout = hyprctl dispatch dpms off
                on-resume = hyprctl dispatch dpms on
              }
            ''
            (if cfg.lock then ''
              listener {
               timeout = 1200
               on-timeout = hyprlock
              }
            '' else "")
          ];

      packages = [
        dotfiles.hypridle.packages.${pkgs.system}.hypridle
      ];
    };
  };
}


