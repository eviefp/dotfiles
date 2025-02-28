/****************************************************************************
  * GUI module
  *
  * GUI programs such as browsers, multimedia, etc.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.gui;
in
{
  options.evie.programs.gui = {
    enable = lib.mkEnableOption "gui defaults";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      # Multimedia
      pkgs.light
      pkgs.fdk_aac
      pkgs.paprefs # multi audio sink setup
      pkgs.pwvucontrol
      pkgs.sonusmix
      pkgs.coppwr
      pkgs.steam
      pkgs.instawow
      pkgs.transmission_4-gtk
      pkgs.xournalpp
      pkgs.libreoffice

      pkgs.gimp

      # X-server related
      pkgs.pass
      pkgs.pinentry-all
      pkgs.xdg-utils

      pkgs.audacity
    ];

    home.file = {
      ".config/fish/functions/ssh.fish".source = ../../../config/fish/functions/ssh.fish;
      ".config/fish/functions/ed.fish".source = ../../../config/fish/functions/ed.fish;
      ".XCompose".source = ../../../config/XCompose;
    };

    programs = {
      feh = {
        enable = true;
        keybindings = {
          zoom_in = [ "plus" "J" ];
          zoom_out = [ "minus" "K" ];
          zoom_default = "0";
          scroll_left = "h";
          scroll_right = "l";
          scroll_up = "k";
          scroll_down = "j";
          quit = "q";
          next_img = "Right";
          prev_img = "Left";
        };
      };

      mpv = {
        enable = true;
        config = {
          audio-channels = "stereo";
        };
      };
    };

  };
}
