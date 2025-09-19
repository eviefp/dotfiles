/****************************************************************************
  * term/kitty module
  *
  **************************************************************************/
{ lib, pkgs, config, theme, ... }:
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

        background            ${theme.light.bg}
        foreground            ${theme.light.fg}
        cursor                ${theme.light.fg}
        cursor_text_color     ${theme.light.bg}
        selection_background  ${theme.light.bg_highlight}
        color0                ${theme.light.bg}
        color8                ${theme.light.bg_highlight}
        color1                ${theme.light.red}
        color9                ${theme.light.red}
        color2                ${theme.light.green}
        color10               ${theme.light.green}
        color3                ${theme.light.yellow}
        color11               ${theme.light.yellow}
        color4                ${theme.light.blue}
        color12               ${theme.light.blue}
        color5                ${theme.light.purple}
        color13               ${theme.light.purple}
        color6                ${theme.light.cyan}
        color14               ${theme.light.cyan}
        color7                ${theme.light.fg}
        color15               ${theme.light.fg}
        selection_foreground  ${theme.light.fg}
        active_tab_foreground #000000
        active_tab_background ${theme.light.orange}
        inactive_tab_foreground ${theme.light.fg}
        inactive_tab_background ${theme.light.bg}
      '';
      ".config/kitty/dark.conf".text = /*toml*/ ''
        font_size 10.0
        background_opacity 0.8
        dynamic_background_opacity yes
        background_tint 0.8

        # cyberdream theme for kitty
        background            #000000
        foreground            ${theme.dark.fg}
        cursor                ${theme.dark.fg}
        cursor_text_color     ${theme.dark.bg}
        selection_background  ${theme.dark.bg_highlight}
        color0                ${theme.dark.bg}
        color8                ${theme.dark.bg_highlight}
        color1                ${theme.dark.red}
        color9                ${theme.dark.red}
        color2                ${theme.dark.green}
        color10               ${theme.dark.green}
        color3                ${theme.dark.yellow}
        color11               ${theme.dark.yellow}
        color4                ${theme.dark.blue}
        color12               ${theme.dark.blue}
        color5                ${theme.dark.purple}
        color13               ${theme.dark.purple}
        color6                ${theme.dark.cyan}
        color14               ${theme.dark.cyan}
        color7                ${theme.dark.fg}
        color15               ${theme.dark.fg}
        selection_foreground  ${theme.dark.fg}
        active_tab_foreground #000000
        active_tab_background ${theme.dark.orange}
        inactive_tab_foreground ${theme.dark.fg}
        inactive_tab_background ${theme.dark.bg}
      '';
    };
    programs.kitty = {
      enable = true;
      settings = {
        shell = "${lib.getExe pkgs.nushell} --login --interactive";

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
