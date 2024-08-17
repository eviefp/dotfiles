/****************************************************************************
  * GUI module
  *
  * GUI programs such as browsers, multimedia, etc.
  **************************************************************************/
{ dotfiles, pkgs, ... }:
{
  imports = with dotfiles.self.homeManagerModules.programs; [
    browsers
    kitty
    chat
  ];

  config = {
    home.packages = [
      # Multimedia
      pkgs.light
      pkgs.fdk_aac
      pkgs.paprefs # multi audio sink setup
      pkgs.pavucontrol
      pkgs.steam
      pkgs.transmission_4-gtk
      pkgs.xournal
      pkgs.libreoffice

      pkgs.gimp

      # X-server related
      pkgs.pass
      pkgs.pinentry #TODO: should be removed I think
      pkgs.xdg_utils

      pkgs.audacity
    ];

    home.file = {
      ".config/fish/functions/ssh.fish".source =
        ../../config/fish/functions/ssh.fish;

      ".config/fish/functions/ed.fish".source =
        ../../config/fish/functions/ed.fish;

      ".XCompose".source = ../../config/XCompose;
    };

    programs = {
      mpv = {
        enable = true;
      };
    };

  };
}
