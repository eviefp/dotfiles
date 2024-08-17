/****************************************************************************
  * Fonts module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  config = {
    home.packages = [
      pkgs.comic-mono
      pkgs.fira-code
      pkgs.font-awesome
      pkgs.monaspace
      pkgs.nerdfonts
    ];

    fonts.fontconfig.enable = true;
  };
}
