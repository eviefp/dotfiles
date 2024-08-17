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
  options.evie.boot = {
    enableHeadless = lib.options.mkEnableOption "Enable headless mode.";
  };

  config.boot = lib.mkMerge [
    {
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };
    }
    # TODO: move this to the server's hardware config and remove setting
    (lib.mkIf cfg.enableHeadless { kernelParams = [ "nomodeset" ]; })
  ];
}
