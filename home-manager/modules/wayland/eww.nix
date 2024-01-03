{ lib, config, pkgs, ... }:
let
in
{
  imports = [ ];

  config = {

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
