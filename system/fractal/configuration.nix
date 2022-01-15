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
    boot.kernelParams = [ "nomodeset" ];

    networking = common.networking // {
      hostName = "fractal";
      interfaces.eno1.useDHCP = true;
    };

    i18n = common.i18n;
    console = common.console;
    time = common.time;
    nixpkgs = common.nixpkgs;

    environment.systemPackages = with pkgs; [
      vim
      wget
      git
    ];

    fonts = common.fonts;

    services.lorri.enable = true;
    services.openssh.enable = true;
    services.printing = {
      enable = true;
      drivers = [ pkgs.hplip pkgs.gutenprint ];
    };
    services.nextcloud = {
        enable = true;
        hostName = "fractal";
        config = {
          dbtype = "pgsql";
          dbuser = "nextcloud";
          dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
          dbname = "nextcloud";
          adminpassFile = "/home/evie/root";
          adminuser = "root";
        };
    };
    services.postgresql = {
        enable = true;
        ensureDatabases = [ "nextcloud" ];
        ensureUsers = [
        { name = "nextcloud";
          ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
        }
        ];
    };

    systemd.services."nextcloud-setup" = {
        requires = ["postgresql.service"];
        after = ["postgresql.service"];
    };

    sound.enable = true;
    hardware.pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };

    nix = common.nix;
    users = common.users;
    security = common.security;
    virtualisation.docker = common.virtualisation;
    system = common.system;
}
