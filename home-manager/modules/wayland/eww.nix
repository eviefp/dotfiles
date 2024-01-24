{ lib, config, pkgs, ... }:
let
  cfg = config.evie.wayland;
in
{
  imports = [ ];

  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.csvkit
      pkgs.socat
      pkgs.wlr-randr
    ];

    programs.eww = {
      enable = true;
      package = pkgs.eww-wayland;
      configDir = ./../../../config/eww;
    };
  };
}
