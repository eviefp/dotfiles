/****************************************************************************
  * aerospace module
  ************************************************************************ */
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.macos.aerospace;
in
{
  options.evie.macos.aerospace = {
    enable = lib.mkEnableOption "aerospace defaults";

  };

  config = lib.mkIf cfg.enable {
    programs.aerospace = {
      enable = true;

      launchd = {
        enable = true;
        keepAlive = true;
      };

      userSettings = {

        mode.map.binding = {
          alt-enter = "exec-and-forget open -n ${lib.getExe pkgs.kitty}";
        };
      };
    };
  };
}
