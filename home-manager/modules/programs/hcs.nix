/****************************************************************************
  * hcs module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  sources = import ../../../nix/sources.nix;
  hcs = import sources.hcs { pkgs = pkgs; };
  cfg = config.evie.programs.hcs;
in
{
  imports = [ ];

  options.evie.programs.hcs = {
    enable = lib.options.mkEnableOption "Enable hcs";
    service = lib.options.mkEnableOption "Enable hcs service";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      hcs
    ];

    systemd.user.services.hcsHasura = lib.mkIf cfg.service {
      Unit = {
        Description = "HCS-hasura";
      };

      Service = {
        ExecStart = "${hcs}/bin/hcs import hasura";
      };
    };

    systemd.user.timers.hcsHasura = lib.mkIf cfg.service {
      Unit = {
        Description = "HCS-hasura-timer";
      };

      Timer = {
        OnCalendar = "hourly";
        Unit = "hcsHasura.service";
      };

      Install = { WantedBy = [ "timers.target" ]; };
    };

    systemd.user.services.hcsEvie = lib.mkIf cfg.service {
      Unit = {
        Description = "HCS-evie";
      };

      Service = {
        ExecStart = "${hcs}/bin/hcs import evie";
      };
    };

    systemd.user.timers.hcsEvie = lib.mkIf cfg.service {
      Unit = {
        Description = "HCS-evie-timer";
      };

      Timer = {
        OnCalendar = "hourly";
        Unit = "hcsEvie.service";
      };

      Install = { WantedBy = [ "timers.target" ]; };
    };
  };
}
