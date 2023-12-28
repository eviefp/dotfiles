/****************************************************************************
  * programs/kitty module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.kitty;
in
{
  imports = [ ];

  options.evie.programs.kitty = {
    enable = lib.options.mkEnableOption "Enable kitty";
  };

  config = lib.mkIf cfg.enable
    {
      programs.kitty = {
        enable = true;
        settings = {
          scrollback_lines = 10000;
          repaint_delay = 4;

          font_family = "Hasklug Nerd Font Mono";
          # bold_font = "Hasklug Bold Nerd Font Complete Mono";
          # italic_font = "Hasklug Italic Nerd Font Complete Mono";
          # bold_italic_font = "Hasklug Bold Italic Nerd Font Complete Mono";
          disable_ligatures = "never";
          font_size = "10.0";

          background_opacity = "0.7";
          dynamic_background_opacity = "yes";
          background_tint = "0.5";

          cursor = "#d62dc9";
          cursor_shape = "block";
          cursor_blink_interval = 0;

          background = "#111111";
          foreground = "#c934f3";
          window_padding_width = 5;

          linux_display_server = "wayland";
        };
        shellIntegration = {
          enableFishIntegration = true;
          enableBashIntegration = true;
          mode = "no-cursor";
        };
      };
    };
}
