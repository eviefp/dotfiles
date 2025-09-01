/****************************************************************************
  * GUI module
  *
  * GUI programs such as browsers, multimedia, etc.
  **************************************************************************/
{ dotfiles, lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.gui;
  scrcpy = dotfiles.self.lib.nuShellScript {
    name = "scrcpy";
    text = ''
      scrcpy --video-codec=h265 -m1920 --max-fps=60 -K --legacy-paste
    '';
    runtimeInputs = [ pkgs.scrcpy ];
  };
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
      pkgs.prismlauncher
      # pkgs.instawow
      pkgs.transmission_3-gtk
      pkgs.xournalpp
      pkgs.libreoffice

      pkgs.gimp

      # X-server related
      pkgs.pass
      pkgs.pinentry-all
      pkgs.xdg-utils

      pkgs.audacity

      # phone mirroring
      scrcpy
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
