/****************************************************************************
  * programs/chat module
  *
  **************************************************************************/
{ lib, config, pkgs, unstable, ... }:
let
  cfg = config.evie.programs.chat;
in
{
  imports = [ ];

  options.evie.programs.chat = {
    enable = lib.options.mkEnableOption "Enable chat programs";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.discord
      pkgs.slack
      pkgs.signal-desktop
      unstable.chatterino2
    ];
  };
}
