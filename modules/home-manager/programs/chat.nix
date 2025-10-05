/**
**************************************************************************
* programs/chat module
*
*************************************************************************
*/
{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.programs.chat;
  teams = pkgs.writeShellApplication {
    name = "teams";
    runtimeInputs = [pkgs.google-chrome];
    text = ''
      #!/usr/bin/env bash

      google-chrome-stable --app="https://teams.microsoft.com/v2/"
    '';
  };
in {
  options.evie.programs.chat = {
    enable = lib.mkEnableOption "chat defaults";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.discord
      pkgs.signal-desktop
      pkgs.element-desktop
      pkgs.chatterino2
      teams
    ];
  };
}
