/**
**************************************************************************
* Logind module
*
*************************************************************************
*/
{
  lib,
  config,
  ...
}: let
  cfg = config.evie.logind;
in {
  imports = [];

  options.evie.logind = {
    enable = lib.options.mkEnableOption "Enable logind.";
  };

  config = lib.mkIf cfg.enable {
    services.logind = {
      lidSwitch = "suspend";
      lidSwitchExternalPower = "suspend";
    };
  };
}
