/**
**************************************************************************
* Moonlander
*************************************************************************
*/
{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.hardware.moonlander;
in {
  options.evie.hardware.moonlander = {
    enable = lib.mkEnableOption "moonlander";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.keymapp
      pkgs.wally-cli
    ];

    hardware.keyboard.zsa.enable = true;
  };
}
