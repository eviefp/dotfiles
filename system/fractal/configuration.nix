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

    services = {
      lorri.enable = true;

      openssh.enable = true;

      printing = {
        enable = true;
        drivers = [ pkgs.hplip pkgs.gutenprint ];
      };

      nextcloud = {
        enable = true;
        hostName = "fractal.eevie.ro";
        https = true;
        home = "/mnt/raid/nextcloud";
        maxUploadSize = "512G";
        config = {
          dbtype = "pgsql";
          dbuser = "nextcloud";
          dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
          dbname = "nextcloud";
          adminpassFile = "/mnt/raid/nextcloud/pass";
          adminuser = "admin";
        };
      };

      postgresql = {
        enable = true;
        ensureDatabases = [ "nextcloud" ];
        ensureUsers = [
        { name = "nextcloud";
          ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
        }
        ];
      };

      nginx = {
        enable = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;

        virtualHosts = {
          "fractal" = {
            forceSSL = false;
          };
          "fractal.eevie.ro" = {
            forceSSL = true;
            sslCertificate = "/mnt/raid/fractal.eevie.ro.crt";
            sslCertificateKey = "/mnt/raid/fractal.eevie.ro.key";
          };
        };
      };

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

    virtualisation = common.virtualisation;
    system = common.system;
}
