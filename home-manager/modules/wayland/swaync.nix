{ lib, config, pkgs, ... }:
let
in
{
  imports = [ ];

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
