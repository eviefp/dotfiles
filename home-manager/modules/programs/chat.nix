/****************************************************************************
  * programs/chat module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.chat;
  sources = import ../../../nix/sources.nix;
  unstable = import sources.unstable { };
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
