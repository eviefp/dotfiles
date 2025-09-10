/****************************************************************************
  * term/kitty module
  *
  **************************************************************************/
{ lib, config, ... }:
let
  cfg = config.evie.term.kitty;
in
{
  options.evie.term.kitty = {
    enable = lib.mkEnableOption "kitty defaults";
  };

  config = lib.mkIf cfg.enable {
    home.file = {
      ".config/kitty/light.conf".text = /*toml*/ ''
        font_size 12.0
        background_opacity 0.7
        dynamic_background_opacity yes
        background_tint 1.0

        # cyberdream theme for kitty
        background            #ffffff
        foreground            #16181a
        cursor                #16181a
        cursor_text_color     #ffffff
        selection_background  #acacac
        color0                #ffffff
        color8                #acacac
        color1                #d11500
        color9                #d11500
        color2                #008b0c
        color10               #008b0c
        color3                #997b00
        color11               #997b00
        color4                #0057d1
        color12               #0057d1
        color5                #a018ff
        color13               #a018ff
        color6                #008c99
        color14               #008c99
        color7                #16181a
        color15               #16181a
        selection_foreground  #16181a
        active_tab_foreground #000000
        active_tab_background #d17c00
        inactive_tab_foreground #16181a
        inactive_tab_background #ffffff
      '';
      ".config/kitty/dark.conf".text = /*toml*/ ''
        font_size 10.0
        background_opacity 0.8
        dynamic_background_opacity yes
        background_tint 0.8

        # cyberdream theme for kitty
        background            #000000
        foreground            #ffffff
        cursor                #ffffff
        cursor_text_color     #16181a
        selection_background  #3c4048
        color0                #16181a
        color8                #3c4048
        color1                #ff6e5e
        color9                #ff6e5e
        color2                #5eff6c
        color10               #5eff6c
        color3                #f1ff5e
        color11               #f1ff5e
        color4                #5ea1ff
        color12               #5ea1ff
        color5                #bd5eff
        color13               #bd5eff
        color6                #5ef1ff
        color14               #5ef1ff
        color7                #ffffff
        color15               #ffffff
        selection_foreground  #ffffff
        active_tab_foreground #000000
        active_tab_background #ffbd5e
        inactive_tab_foreground #ffffff
        inactive_tab_background #16181a
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
