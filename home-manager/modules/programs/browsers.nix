/****************************************************************************
  * programs/browsers module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.browsers;
in
{
  imports = [ ];

  options.evie.programs.browsers = {
    enable = lib.options.mkEnableOption "Enable browsers";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.chromium
      pkgs.firefox
    ];

    home.file = {
      ".config/tridactyl/tridactylrc".source = ../../../config/tridactyl;
      ".mozilla/native-messaging-hosts/passff.json".source =
        "${pkgs.passff-host}/share/passff-host/passff.json";
      ".mozilla/native-messaging-hosts/tridactyl.json".source =
        "${pkgs.tridactyl-native}/lib/mozilla/native-messaging-hosts/tridactyl.json";
      ".mozilla/native-messaging-hosts/firenvim.json".source = ../../../config/firenvim.json;
    };
    programs = {
      browserpass = {
        enable = true;
        browsers = [ "firefox" ];
      };
    };
  };
}
