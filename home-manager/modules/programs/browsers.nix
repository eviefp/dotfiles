/****************************************************************************
  * programs/browsers module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.browsers;
in
{
  imports = [ ];

  options.evie.programs.browsers = {
    enable = lib.options.mkEnableOption "Enable browsers";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      # pkgs.chromium
    ];

    home.file = {
      # ".config/tridactyl/tridactylrc".source = ../../../config/tridactyl;
      # ".mozilla/native-messaging-hosts/passff.json".source =
      #   "${pkgs.passff-host}/share/passff-host/passff.json";
      # ".mozilla/native-messaging-hosts/firenvim.json".source = ../../../config/firenvim.json;
    };
    programs = {
      browserpass = {
        enable = true;
        browsers = [ "firefox" ];
      };
      firefox = {
        enable = false;
        package = pkgs.firefox.override {
          cfg = {
            enableTridactylNative = true;
          };
        };
        # nativeMessagingHosts = []; TODO
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
      qutebrowser = {
        enable = true;
        package = pkgs.qutebrowser.override {
          enableWideVine = true;
        };
        keyBindings = {
          normal = {
            "<Ctrl-d>" = "tab-close";
            "<Ctrl-r>" = "reload";
          };
        };
        extraConfig = ''
          config.unbind('d')
          config.unbind('r')
        '';
        settings = {
          editor.command = [ "emacsclient" "-q" "-u" "-c" "{}" ];
          auto_save.session = true;
          colors = {
            webpage = {
              darkmode = {
                enabled = false;
              };
              preferred_color_scheme = "dark";
            };
          };
          content = {
            autoplay = false;
            pdfjs = true;
          };
          downloads = {
            location = {
              directory = "/home/evie/Downloads";
              prompt = false;
            };
            position = "bottom";
            remove_finished = 10000;
          };
          tabs = {
            position = "right";
            show = "multiple";
            last_close = "close";
            select_on_remove = "last-used";
            # padding = ''{"bottom": 3, "left": 5, "right": 5, "top": 0}'';
          };
          session = {
            default_name = "evie";
            lazy_restore = true;
          };
          # spellcheck.languages = [ "en-US" "en-GB" "ro-RO" ];
          statusbar = {
            position = "top";
          };
          window = {
            transparent = false;
            hide_decoration = false;
          };
        };
      };
    };
  };
}
