/**
**************************************************************************
* Fonts module
*
*************************************************************************
*/
{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.system.fonts;
in {
  options.evie.system.fonts = {
    enable = lib.mkEnableOption "fonts defaults";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.comic-mono
      pkgs.fira-code
      pkgs.font-awesome
      pkgs.monaspace
    ];

    fonts.fontconfig.enable = true;
  };
}
