/****************************************************************************
  * Boot module
  *
  * All my systems are setup as UEFI. The home server device is having issues
  * with modeset, so I have to set the 'nomodeset' kernel param to get it to
  * boot.
  **************************************************************************/
{ lib, config, ... }:
let cfg = config.evie.boot;
in
{
  imports = [ ];

  options.evie.boot = {
    enableHeadless = lib.options.mkEnableOption "Enable headless mode.";
  };

  config = lib.mkMerge [
    {
      boot.loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };

      # Randomly decided the NixOS version should be here.
      system.stateVersion = "23.11";
    }
    (lib.mkIf cfg.enableHeadless { boot.kernelParams = [ "nomodeset" ]; })
  ];
}
