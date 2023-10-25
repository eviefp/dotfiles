/****************************************************************************
  * Nextcloud module
  *
  * Everything is hardcoded because I have a single server which hosts this. No
  * need to generalise!
  **************************************************************************/
{ lib, config, ... }:
{
  imports = [ ];

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
          dbhost =
            "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
          dbname = "nextcloud_aio";
          adminpassFile = "/mnt/raid/nextcloud/pass";
          adminuser = "admin";
        };
      };

      postgresql = {
        enable = true;
        ensureDatabases = [ "nextcloud_aio" ];
        ensureUsers = [{
          name = "nextcloud";
          ensurePermissions."DATABASE nextcloud_aio" = "ALL PRIVILEGES";
        }];
      };

      nginx = {
        enable = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;
        logError = "stderr debug";

        virtualHosts = {
          "fractal" = {
            forceSSL = false;
            locations = {
              "/wiki" = {
                root = "/mnt/raid";
                index = "impulse.html";
              };
            };
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
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
    };
  };
}
