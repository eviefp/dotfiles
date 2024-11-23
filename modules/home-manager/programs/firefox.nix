/****************************************************************************
  * programs/firefox module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.firefox;
in
{
  options.evie.programs.firefox = {
    enable = lib.mkEnableOption "firefox defaults";
  };

  config = lib.mkIf cfg.enable {
    home.file = {
      ".config/tridactyl/tridactylrc".text = ''
        autocmd DocStart ^http(s?)://www.reddit.com js tri.excmds.urlmodify("-t", "www", "old")

        set allowautofocus false
        set searchengine duckduckgo

        set hintfiltermode vimperator-reflow
        set hintchars fdsqjklmrezauiopwxcvghtybn

        blacklistadd twitter.com
        blacklistadd gmail.com
      '';
      ".mozilla/native-messaging-hosts/passff.json".source =
        "${pkgs.passff-host}/share/passff-host/passff.json";
      ".mozilla/native-messaging-hosts/firenvim.json".text = ''
        { "name": "firenvim", "description": "Turn your browser into a Neovim GUI.", "path": "/home/evie/.local/share/firenvim/firenvim", "type": "stdio", "allowed_extensions": ["firenvim@lacamb.re"]}
      '';
    };

    programs = {
      firefox = {
        enable = false;
        package = pkgs.firefox.override {
          cfg = {
            enableTridactylNative = true;
          };
        };
        policies = {
          AllowFileSelctionDialogs = true;
          AppAutoUpdate = false;
          DefaultDownloadDirectory = "~/Downloads";
          DisableAppUpdate = true;
          DisableFirefoxStudies = true;
          DisablePocket = true;
          DisableSetDesktopBackground = true;
          DisableTelemetry = true;
          DisableBookmarksToolbar = true;
          DisplayMenuBar = false;
          DontCheckDefaultBrowser = true;
          SearchBar = "unified";
          ShowHomeButton = false;
        };
      };
    };
  };
}
