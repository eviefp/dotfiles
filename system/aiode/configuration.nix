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
    hostName = "aiode";
    interface = "enp0s31f6";
    wifi = {
      enable = true;
      interface = "wlp2s0";
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
    enableHiDPI = true;
  };
}

