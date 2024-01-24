{ lib, config, pkgs, ... }:
let
  cfg = config.evie.wayland;
in
{
  imports = [ ];

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
