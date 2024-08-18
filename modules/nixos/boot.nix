/****************************************************************************
  * Boot module
  *
  * All my systems are setup as UEFI.
  **************************************************************************/
{ lib, ... }:
{
  config.boot = lib.mkMerge [
    {
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };
    }
  ];
}
