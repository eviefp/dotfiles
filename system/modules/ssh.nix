/****************************************************************************
  * SSH module
  *
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
    (lib.mkIf cfg.enableHeadless { boot.kernelParams = [ "nomodeset" ]; })
  ];
}
