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
    ../modules/logind.nix
    ../modules/packages.nix
    ../modules/services.nix
    ../modules/xserver.nix
    ../modules/users.nix
  ];

  evie.network = {
    hostName = "janus";
    interface = "";
    wifi = {
      enable = true;
      interface = "";
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

  evie.logind.enable = true;
}

