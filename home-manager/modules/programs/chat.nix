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
      pkgs.chatterino2
    ];
  };
}
