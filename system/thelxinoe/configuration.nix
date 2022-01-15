{ config, pkgs, ... }:
let
  # common = import ../system-gnome.nix { inherit config pkgs; };
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
      hostName = "thelxinoe";
      interfaces.enp4s0.useDHCP = true;
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
    };
    sound = common.sound;
    hardware = common.hardware // {
      pulseaudio = common.hardware.pulseaudio // {
        extraModules = [ pkgs.pulseaudio-modules-bt ];
      };
      bluetooth.enable = true;
    };
    nix = common.nix;
    users = common.users;
    security = common.security;
    virtualisation = common.virtualisation;
    system = common.system;
  }

