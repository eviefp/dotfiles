/****************************************************************************
  * Fonts module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.fonts;
in
{
  imports = [ ];

  options.evie.fonts = {
    enable = lib.options.mkEnableOption "Enable fonts";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.fira-code
      pkgs.nerdfonts
    ];

    fonts.fontconfig.enable = true;
  };
}
