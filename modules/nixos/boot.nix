/**
**************************************************************************
* Boot module
*
* All my systems are setup as UEFI.
*************************************************************************
*/
{
  lib,
  config,
  ...
}: let
  cfg = config.evie.boot;
in {
  options.evie.boot = {
    enable = lib.mkEnableOption "boot defaults";
  };

  config.boot = lib.mkIf cfg.enable {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };
}
