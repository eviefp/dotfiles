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

    networking = common.networking // {
      hostName = "aiode"; # Define your hostname.
      networkmanager.enable = true;
      wireless.networks = import ./networks.nix;
      interfaces.enp0s31f6.useDHCP = true;
      interfaces.wlp2s0.useDHCP = true;
    };
  
    i18n = common.i18n;
    console = common.console;
    time = common.time;
    nixpkgs = common.nixpkgs;
    environment = common.environment;
    fonts = common.fonts;
    programs = common.programs;
  
    services = common.services // {
      blueman.enable = true;
      xserver.videoDrivers = [ "intel" ];
    };
  
    sound = common.sound;

    hardware = common.hardware // {
      pulseaudio = {
        enable = true;
        extraModules = [ pkgs.pulseaudio-modules-bt ];
        package = pkgs.pulseaudioFull;
      };
      bluetooth.enable = true;
      video.hidpi.enable = true;
    };

    nix = common.nix;

    users = common.users;

    security = common.security;
  
    system = common.system;
  
  }

