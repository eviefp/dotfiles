/****************************************************************************
  * programs/browsers module
  *
  **************************************************************************/
{ pkgs, config, ... }:
{
  imports = [ ];

  config = {
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
            "<Ctrl-right>" = "set tabs.show switching";
            "<Ctrl-left>" = "set tabs.show multiple";
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
              directory = "/home/${config.evie.system.user}/Downloads";
              prompt = false;
            };
            position = "bottom";
            remove_finished = 10000;
          };
          tabs = {
            position = "right";
            show = "switching";
            last_close = "close";
            select_on_remove = "last-used";
          };
          session = {
            default_name = "evie";
            lazy_restore = true;
          };
          # spellcheck.languages = [ "en-US" "en-GB" "ro-RO" ];
          statusbar = {
            position = "bottom";
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
