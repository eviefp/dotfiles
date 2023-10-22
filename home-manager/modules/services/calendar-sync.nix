/****************************************************************************
  * Calendar Sync Service
  **************************************************************************/
{ builtins, lib, config, pkgs, ... }:
let
  cfg = config.evie.services.calendar-sync;

  calendars = (import ./calendar-sync-secrets.nix).calendars;

  mkScript = calendar: import ./calendar-sync-script.nix {
    inherit pkgs;
    resholve = pkgs.resholve;
    url = calendar.url;
    org = calendar.org;
  };

  mkService = calendar: {
    Unit = {
      Description = "Calendar Sync ${calendar.name}";
    };

    Install = { WantedBy = [ "default.target" ]; };

    Service = {
      Type = "simple";
      ExecStart = "${mkScript calendar}/bin/calendar-sync-script";
      Restart = "always";
      RestartSec = 600;
    };
  };

  cal-services = lib.forEach calendars (cal: {
    name = "calendar-sync-${cal.name}";
    value = mkService cal;
  });

  services = lib.listToAttrs cal-services;
in
{
  imports = [ ];

  options.evie.services.calendar-sync = {
    enable = lib.options.mkEnableOption "Enable calendar sync";
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services = services;
  };
}

