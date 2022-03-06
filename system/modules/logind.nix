/****************************************************************************
  * Logind module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.logind;

in
{
  imports = [ ];

  options.evie.logind = {
    enable = lib.options.mkEnableOption "Enable XServer.";
  };

  config = lib.mkIf cfg.enable {
    services.logind = {
      lidSwitch = "suspend";
      lidSwitchExternalPower = "suspend";
    };
  };
}
