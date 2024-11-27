/****************************************************************************
  * Pipewire
  **************************************************************************/
{ lib, config, ... }:
let
  cfg = config.evie.hardware.pipewire;
in
{
  options.evie.hardware.pipewire = {
    enable = lib.mkEnableOption "pipewire";
  };

  config = lib.mkIf cfg.enable {
    hardware.pulseaudio.enable = false;
    services. pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse = {
        enable = true;
      };
      wireplumber = {
        enable = true;
      };
    };
  };
}

