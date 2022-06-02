/****************************************************************************
  * programs/bower module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.bower;
in
{
  imports = [ ];

  options.evie.programs.bower = {
    enable = lib.options.mkEnableOption "Enable bower";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.notmuch-bower
      pkgs.ncurses
      pkgs.lynx
      pkgs.gpgme
      pkgs.file
    ];
  };
}
