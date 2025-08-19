# wofi is really nice and simple, but is unmaintained so maybe not
{ config, lib, ... }:
let
  cfg = config.evie.wayland.wofi;
in
{
  options.evie.wayland.wofi = {
    enable = lib.mkEnableOption "wofi defaults";
  };
  config = lib.mkIf cfg.enable {
    programs.wofi = {
      enable = true;

      # show / mode found in wofi.7
      # run - apps in $PATH
      # drun - desktop apps
      # dmenu
      settings = {
        allow_markup = true;
        term = "kitty";
        hide_scroll = false;
        matching = "fuzzy";
        insensitive = true;
        gtk_dark = true;
      };

      style = ''
        '';
    };
  };
}
