/****************************************************************************
  * term/ranger module
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.term.ranger;
in
{
  options.evie.term.ranger = {
    enable = lib.mkEnableOption "ranger defaults";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.ranger
      pkgs.w3m # for displaying images
    ];
    home.file.".config/ranger/rc.conf".source = ../../../config/ranger/rc.conf;
  };
}
