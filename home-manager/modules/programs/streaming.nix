/****************************************************************************
  * programs/streaming module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.streaming;
in
{
  imports = [ ];

  options.evie.programs.streaming = {
    enable = lib.options.mkEnableOption "Enable streaming";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.ffmpeg-full pkgs.chatterino2 pkgs.kdenlive ];

    programs = {
      obs-studio = {
        enable = true;
        plugins = [ ];
      };
    };
  };
}
