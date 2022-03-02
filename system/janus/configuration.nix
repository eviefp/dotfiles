/****************************************************************************
  * Aiode system configuration
  *
  **************************************************************************/

{ config, pkgs, ... }:
let common = import ../common.nix { inherit config pkgs; };
in
{
  imports = [
    ./hardware-configuration.nix
    ../modules/boot.nix
    ../modules/network.nix
    ../modules/locale.nix
    ../modules/packages.nix
    ../modules/services.nix
    ../modules/xserver.nix
    ../modules/users.nix
  ];

  evie.network = {
    hostName = "janus";
    interface = "enp0s20f0u3";
    wifi = {
      enable = true;
      interface = "wlo1";
    };
  };

  evie.packages = {
    enableGPG = true;
    enableDconf = true;
  };

  evie.services.xcompose = true;

  evie.xserver = {
    enable = true;
    useBluetooth = true;
    enableHiDPI = false;
  };
}

