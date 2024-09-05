/****************************************************************************
  * programs/kitty module
  *
  **************************************************************************/
{ ... }:
{
  config = {
    home.file = {
      ".config/kitty/light.conf".text = ''
        font_size 12.0
        background_opacity 0.7
        dynamic_background_opacity yes
        background_tint 1.0

        cursor #f975df

        background #ffffff
        foreground #822bd8

        color0 #15161E
        color8 #414868

        color1 #f7768e
        color9 #f7768e

        color2 #9ece6a
        color10 #9ece6a

        color3 #e0af68
        color11 #e0af68

        color4 #7aa2f7
        color12 #7aa2f7

        color5 #bb9af7
        color13 #bb9af7

        color6 #7dcfff
        color14 #7dcfff

        color7 #a9b1d6
        color15 #c0caf5
      '';
      ".config/kitty/dark.conf".text = ''
        font_size 10.0
        background_opacity 0.6
        dynamic_background_opacity yes
        background_tint 0.5

        cursor #822bd8

        background #000000
        foreground #f975df

        color0 #15161E
        color8 #414868

        color1 #f7768e
        color9 #f7768e

        color2 #9ece6a
        color10 #9ece6a

        color3 #e0af68
        color11 #e0af68

        color4 #7aa2f7
        color12 #7aa2f7

        color5 #bb9af7
        color13 #bb9af7

        color6 #7dcfff
        color14 #7dcfff

        color7 #a9b1d6
        color15 #c0caf5
      '';
    };
    programs.kitty = {
      enable = true;
      settings = {
        shell = "nu --login --interactive";

        scrollback_lines = 10000;
        repaint_delay = 4;

        font_family = "Hasklug Nerd Font Mono";
        disable_ligatures = "never";

        cursor_shape = "block";
        cursor_blink_interval = "0";
        cursor_stop-blinking_after = "0";

        window_padding_width = 5;

        enable_audio_bell = "no";

        copy_on_select = "clipboard";
        strip_trailing_spaces = "smart";
        mouse_map = "middle release ungrabbed paste_from_clipboard";

        linux_display_server = "wayland";

        tab_bar_stule = "powerline";
        tab_powerline_style = "round";

        include = "theme.conf";

      };
      keybindings = {
        "ctrl+space" = "";
        "ctrl+z" = "";
      };
      shellIntegration = {
        enableFishIntegration = true;
        enableBashIntegration = true;
        mode = "no-cursor";
      };
    };
  };
}
