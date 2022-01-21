{ config, pkgs, ... }:
let
  # common = import ../system-gnome.nix { inherit config pkgs; };
  common = import ../common.nix { inherit config pkgs; };
in {
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
    hostName = "thelxinoe";
    interface = "enp4s0";
  };

  evie.packages = {
    enableGPG = true;
    enableDconf = true;
  };

  evie.services.xcompose = true;

  evie.xserver = {
    enable = true;
    useNVidia = true;
    useBluetooth = true;
  };
}

