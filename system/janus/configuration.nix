/****************************************************************************
  * Janus system configuration
  *
  **************************************************************************/

{ config, pkgs, ... }:
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

  # this might not work?
  nixpkgs.config = {
    packageOverrides = pkgs: {
      vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
    };
  };

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
    extra = [ pkgs.libva pkgs.libva-utils ];
  };

  evie.services.xcompose = true;

  evie.xserver = {
    enable = true;
    useBluetooth = true;
  };

  # evie.logind.enable = true;

  environment.variables = {
    VPAU_DRIVER = "va_gl";
  };
}

