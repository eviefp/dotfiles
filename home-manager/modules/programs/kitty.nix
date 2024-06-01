/****************************************************************************
  * programs/kitty module
  *
  **************************************************************************/
{ ... }:
{
  imports = [ ];

  config = {
    home.file = {
      ".config/kitty/light.conf".text = ''
        background_opacity 0.7
        dynamic_background_opacity yes
        background_tint 0.5

        cursor #721045

        background #ffffff
        foreground #721045

      '';
      ".config/kitty/dark.conf".text = ''
        background_opacity 0.7
        dynamic_background_opacity yes
        background_tint 0.5

        cursor #d62dc9

        background #111111
        foreground #c934f3

      '';
    };
    programs.kitty = {
      enable = true;
      settings = {
        shell = "nu --login --interactive";

        scrollback_lines = 10000;
        repaint_delay = 4;

        font_size = "10.0";
        font_family = "Hasklug Nerd Font Mono";
        disable_ligatures = "never";

        cursor_shape = "block";
        cursor_blink_interval = 0;

        window_padding_width = 5;

        enable_audio_bell = "no";

        copy_on_select = "clipboard";
        strip_trailing_spaces = "smart";
        mouse_map = "middle release ungrabbed paste_from_clipboard";

        linux_display_server = "wayland";

        include = "theme.conf";

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
