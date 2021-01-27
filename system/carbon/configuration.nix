# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  common = import ../common.nix { inherit config pkgs; };
in
  {
    imports =
      [ # Include the results of the hardware scan.
        ./hardware-configuration.nix
      ];

    boot.loader = common.boot.loader;

    networking = common.networking;
    networking.hostName = "aiode"; # Define your hostname.
    networking.networkmanager.enable = true;
    networking.wireless.networks = import ./networks.nix;
    networking.interfaces.enp0s31f6.useDHCP = true;
    networking.interfaces.wlp2s0.useDHCP = true;
  
    i18n = common.il8n;
    console = common.console;
    time = common.time;
    nixpkgs = common.nixpkgs;
    environment = common.environment;
    fonts = common.fonts;
    programs = common.programs;
  
    services = common.services;
  
    sound = common.services;

    hardware = common.hardware;

    hardware.pulseaudio = {
      enable = true;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
      package = pkgs.pulseaudioFull;
    };
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;

    hardware.video.hidpi.enable = true;
  
    # Enable the X11 windowing system.
    services.xserver.videoDrivers = [ "intel" ];

    nix = common.nix;

    users = common.users;

    security = common.security;
  
    system = common.system;
  
  }

