/****************************************************************************
  * programs/chat module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  imports = [ ];

  config = {
    home.packages = [
      pkgs.discord
      pkgs.slack
      pkgs.signal-desktop
      pkgs.element-desktop-wayland
      pkgs.chatterino2
    ];
  };
}
