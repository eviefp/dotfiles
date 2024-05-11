/****************************************************************************
  * ect module
 
  **************************************************************************/
{ lib, config, pkgs, ect, ... }:
let
  cfg = config.evie.programs.ect;
  calendars = (import ../services/calendar-sync-secrets.nix).calendars;
  yamlFormat = pkgs.formats.yaml { };

  ectConfig = {
    calendars = calendars;
    notification = {
      exec = "notify-send {title}";
      threads = 10;
      enable = true;
    };
    export = {
      enable = true;
      outputPath = "/home/evie/code/personal-org/cal-sync/local.ics";
      httpPort = 31234;
    };
  };

  ectPackage = ect.packages.${pkgs.system}.default;
in
{
  imports = [ ];

  options.evie.programs.ect = {
    enable = lib.options.mkEnableOption "Enable ect";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      ect.packages.${pkgs.system}.default
      pkgs.libnotify
    ];

    xdg.configFile."ect/ect.yaml".source = yamlFormat.generate "ect-config" ectConfig;

    systemd.user.services.ect = {
      Unit = {
        Description = "Evie's Calendar Tool";
      };

      Install = { WantedBy = [ "default.target" ]; };

      Service = {
        Type = "exec";

        ExecStart = "${ectPackage}/bin/ect --server";

        Restart = "always";

        RestartSec = 3;
      };
    };
  };
}
