/****************************************************************************
  * programs/wezterm module
  *
  * https://wezfurlong.org/wezterm
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.wezterm;
in
{
  imports = [ ];

  options.evie.programs.wezterm = {
    enable = lib.options.mkEnableOption "Enable wezterm";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.wezterm
    ];
    home.file.".config/wezterm/wezterm.lua".text = ''
      local wezterm = require 'wezterm'

      return {
        default_prog = {"${pkgs.fish}/bin/fish"},
        -- launch_menu = {},
        font = wezterm.font('Hasklug Nerd Font Mono', { weight = "Regular", stretch = "Normal", italic = false }),
        check_for_updates = false,
        font_size = 10,
        -- color_scheme = 'Molokai',
        enable_tab_bar = false,
        window_background_opacity = 0.8,
        text_background_opacity = 1.0,
        colors = {
          foreground = "#c0caf5",
          background = "#220426",
          cursor_bg = "#be14fc",
          cursor_border = "#be14fc",
          cursor_fg = "#6d2887",
          selection_bg = "#33467C",
          selection_fg = "#c0caf5",

          ansi = { "#15161E", "#f7768e", "#9ece6a", "#e0af68", "#7aa2f7", "#bb9af7", "#7dcfff", "#a9b1d6" },
          brights = { "#414868", "#f7768e", "#9ece6a", "#e0af68", "#7aa2f7", "#bb9af7", "#7dcfff", "#c0caf5" },
        },
      }
    '';
  };
}
