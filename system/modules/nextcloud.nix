/*******************************************************************************
 * Nextcloud module
 *
 * Everything is hardcoded because I have a single server which hosts this. No
 * need to generalise!
 ******************************************************************************/
{ lib, config, ... }:
let
  cfg = config.evie.nextcloud;
in {
  imports = [];

  options.evie.nextcloud = {
    enable = lib.options.mkEnableOption "Enable NextCloud mode.";
  };

  config = {
    fileSystems."/mnt/raid" = {
      device = "/dev/md126";
      label = "raid";
      depends = "/";
      mountPoint = "/mnt/raid";
    };

    services = {
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
  };
}
