/****************************************************************************
  * Bluetooth
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.hardware.bluetooth;
in
{
  options.evie.hardware.bluetooth = {
    enable = lib.mkEnableOption "bluetooth";
  };

  config = lib.mkIf cfg.enable {
    hardware = {
      # steam/controllers
      xone.enable = false;
      xpadneo.enable = true;
      steam-hardware.enable = true;

      # pair with blueman
      bluetooth = {
        enable = true;
        settings = {
          General = {
            ControllerMode = "dual";
            # Privacy = "???";
          };
          Policy = {
            AutoEnable = true;
          };
        };
      };

    };

    services.blueman.enable = true;
  };
}
