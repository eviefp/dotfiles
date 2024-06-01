/****************************************************************************
  * programs/kitty module
  *
  **************************************************************************/
{ ... }:
{
  imports = [ ];

  config = {
    programs.kitty = {
      enable = true;
      settings = {
        shell = "nu --login --interactive";

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

        enable_audio_bell = "no";

        copy_on_select = "clipboard";
        strip_trailing_spaces = "smart";
        mouse_map = "middle release ungrabbed paste_from_clipboard";

        linux_display_server = "wayland";

      };
      keybindings = {
        "ctrl+space" = "";
      };
      shellIntegration = {
        enableFishIntegration = true;
        enableBashIntegration = true;
        mode = "no-cursor";
      };
    };
  };
}
