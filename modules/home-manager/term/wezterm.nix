/**
**************************************************************************
* term/wezterm module
*
* https://wezfurlong.org/wezterm
* Does NOT work on wayland :(
*************************************************************************
*/
{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.term.wezterm;
in {
  options.evie.term.wezterm = {
    enable = lib.mkEnableOption "wezterm defaults";
  };

  config = lib.mkIf cfg.enable {
    programs.wezterm = {
      enable = true;
      package = pkgs.wezterm;
      colorSchemes = {
        dark = {
          foreground = "#f975df";
          background = "#000000";
          cursor_bg = "#822bd8";
          cursor_border = "#822bd8";
          cursor_fg = "#f975df";
          selection_bg = "#33467C";
          selection_fg = "#c0caf5";
          compose_cursor = "#f7768e";

          ansi = ["#15161E" "#f7768e" "#9ece6a" "#e0af68" "#7aa2f7" "#bb9af7" "#7dcfff" "#a9b1d6"];
          brights = ["#414868" "#f7768e" "#9ece6a" "#e0af68" "#7aa2f7" "#bb9af7" "#7dcfff" "#c0caf5"];
        };
        light = {
          # todo
          foreground = "#f975df";
          background = "#000000";
          cursor_bg = "#822bd8";
          cursor_border = "#822bd8";
          cursor_fg = "#f975df";
          selection_bg = "#33467C";
          selection_fg = "#c0caf5";
          compose_cursor = "#f7768e";

          ansi = ["#15161E" "#f7768e" "#9ece6a" "#e0af68" "#7aa2f7" "#bb9af7" "#7dcfff" "#a9b1d6"];
          brights = ["#414868" "#f7768e" "#9ece6a" "#e0af68" "#7aa2f7" "#bb9af7" "#7dcfff" "#c0caf5"];
        };
      };
      extraConfig = ''
        local wezterm = require 'wezterm'
        local wezterm = require 'local'

        return {
          adjust_window_size_when_changing_font_size = false,
          allow_win32_input_mode = false,
          audible_bell = "Disabled",
          prefer_egl = true,

          automatically_reload_config = true,

          animation_fps = 60,

          window_background_gradient = {
            orientation = { Linear = { angle = 45.0 } },
            colors = {
              '#350320',
              '#340335',
              '#250335',
              '#1c0335',
              '#060335',
            },
            interpolation = 'Linear',
            blend = 'Rgb',
            noise = 4,
            segment_size = 128,
            segment_smoothness = 1.0,
          },

          default_prog = {"${pkgs.nushellFull}/bin/nu", "--login", "--interactive"},
          enable_wayland = true,
          -- launch_menu = {},
          font = wezterm.font('Hasklug Nerd Font Mono', { weight = "Regular", stretch = "Normal", italic = false }),
          check_for_updates = false,
          font_size = 10,
          -- color_scheme = 'Molokai',
          enable_tab_bar = false,
          front_end = "OpenGL",
          window_padding = {
            left = 5,
            top = 5,
            right = 5,
            bottom = 5,
          },
          window_background_opacity = 0.8,
          text_background_opacity = 1.0,
          color_scheme = "dark",
        }
      '';
    };
  };
}
