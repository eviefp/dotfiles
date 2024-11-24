/****************************************************************************
  * Calendar module
  *
  * Set up calendar accounts.
  * TODO: manually run 'vdirsyncer discover <name>' for each account.
  **************************************************************************/
{ lib, config, osConfig, ... }:
let
  cfg = config.evie.system.calendar;
in
{
  options.evie.system.calendar = {
    enable = lib.mkEnableOption "calendar defaults";
  };

  config = lib.mkIf cfg.enable {
    accounts.calendar = {
      basePath = "${config.xdg.dataHome}/calendars";
      accounts = {

        gmailPrimary = {
          khal = {
            enable = true;
            # addresses = [ "alexa.eviest@gmail.com" "alexaeviest@gmail.com" ];
            color = "light green";
            type = "discover";
          };
          remote.type = "google_calendar";
          vdirsyncer = {
            enable = true;
            metadata = [ "color" ];
            tokenFile = "/home/evie/.local/share/vdirsyncer/token";
            clientIdCommand = [ "cat" "${osConfig.sops.secrets.gmailCalendarClientId.path}" ];
            clientSecretCommand = [ "cat" "${osConfig.sops.secrets.gmailCalendarClientSecret.path}" ];
            collections = [ "from a" "from b" ];
            conflictResolution = "remote wins";
          };
        };
        garnix = {
          khal = {
            enable = true;
            # addresses = [ "alexa.eviest@gmail.com" "alexaeviest@gmail.com" ];
            color = "light green";
            type = "discover";
          };
          remote.type = "google_calendar";
          vdirsyncer = {
            enable = true;
            metadata = [ "color" ];
            tokenFile = "/home/evie/.local/share/vdirsyncer/garnix-token";
            clientIdCommand = [ "cat" "${osConfig.sops.secrets.gmailCalendarClientId.path}" ];
            clientSecretCommand = [ "cat" "${osConfig.sops.secrets.gmailCalendarClientSecret.path}" ];
            collections = [ "from a" "from b" ];
            conflictResolution = "remote wins";
          };
        };
      };
    };

    programs.khal = {
      enable = true;
      locale = {
        dateformat = "%d/%m/%Y";
        longdateformat = "'%A, %d %B %Y'";

        datetimeformat = "'%d/%m/%Y %H:%M'";
        longdatetimeformat = "'%A, %d %B %Y, %H:%M'";

        timeformat = "%H:%M";

        firstweekday = 1;

      };
      # format: "%A, %d %B, %H:%M"
    };

    programs.vdirsyncer = {
      enable = true;
    };

    services.vdirsyncer = {
      enable = true;
      frequency = "*:0/5";
    };
  };
}
