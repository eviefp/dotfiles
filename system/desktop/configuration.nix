{ config, pkgs, ... }:
let
  common = import ../common.nix { inherit config pkgs; };
in
  {
    imports =
      [ # Include the results of the hardware scan.
        ./hardware-configuration.nix
      ];
  
    # Use the systemd-boot EFI boot loader.
    boot = common.boot;
  
    networking = common.networking // {
      hostName = "arche";
      interfaces.enp0s31f6.useDHCP = true;
    };
  
    i18n = common.i18n;
    console = common.console;
    time = common.time;
    nixpkgs = common.nixpkgs;
    environment = common.environment;
    fonts = common.fonts;
    programs = common.programs;
    services = common.services;
    sound = common.sound;
    hardware = common.hardware;
    nix = common.nix;
    users = common.users;
    security = common.security;
    virtualisation = common.virtualisation;
    system = common.system;
  }

