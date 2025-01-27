/****************************************************************************
  * Pipewire
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.hardware.pipewire;
in
{
  options.evie.hardware.pipewire = {
    enable = lib.mkEnableOption "pipewire";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.pulseaudio
    ];
    services.pulseaudio.enable = false;
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

