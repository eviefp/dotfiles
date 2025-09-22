{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.evie.wayland.swaync;
in {
  options.evie.wayland.swaync = {
    enable = lib.mkEnableOption "swaync defaults";
  };
  config = lib.mkIf cfg.enable {
    home = {
      file.".config/swaync" = {
        source = ../../../config/swaync;
        recursive = true;
      };

      packages = [
        pkgs.swaynotificationcenter
      ];
    };
  };
}
