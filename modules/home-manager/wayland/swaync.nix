{ pkgs, ... }:
{
  config = {
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
