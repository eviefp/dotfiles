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
      pkgs.transmission_4-gtk
      pkgs.xournal
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
      mpv = {
        enable = true;
      };
    };

  };
}
