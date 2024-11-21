/****************************************************************************
  * Nextcloud module
  *
  * Everything is hardcoded because I have a single server which hosts this. No
  * need to generalise!
  * TODO: will I reuse this?
  **************************************************************************/
{ lib, config, ... }:
let
  cfg = config.evie.boot;
in
{
  imports = [ ];

  options.evie.nextcloud = {
    enable = lib.options.mkEnableOption "Enable NextCloud service.";
  };

  config = lib.mkIf cfg.enable {
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
        database.createLocally = true;
        config = {
          dbtype = "pgsql";
          adminpassFile = "/mnt/raid/nextcloud/pass";
        };
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
