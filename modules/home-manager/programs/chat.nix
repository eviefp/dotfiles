/****************************************************************************
  * programs/chat module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.chat;
in
{
  options.evie.programs.chat = {
    enable = lib.mkEnableOption "chat defaults";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.discord
      pkgs.signal-desktop
      pkgs.element-desktop-wayland
      pkgs.chatterino2
    ];
  };
}
