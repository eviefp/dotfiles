/****************************************************************************
  * ect module
  *
  **************************************************************************/
{ lib, config, pkgs, ect, ... }:
let
  cfg = config.evie.programs.ect;
  calendars = (import ../services/calendar-sync-secrets.nix).calendars;
  yamlFormat = pkgs.formats.yaml { };

  ectConfig = {
    calendars = lib.flatten [ (lib.forEach calendars (cal: cal.org)) [ "/home/evie/code/personal-org/cal-sync/test.org" ] ];
    notification = {
      exec = "notify-send {title}";
      threads = 10;
    };
  };
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
  };
}
