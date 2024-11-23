/****************************************************************************
  * programs/qutebrowser module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.qutebrowser;
in
{
  options.evie.programs.qutebrowser = {
    enable = lib.mkEnableOption "qutebrowser defaults";
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables.BROWSER = "qutebrowser";

    programs.qutebrowser = {
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
        editor.command = [ "kitty" "-e" "nvim" "{}" ];
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
}
