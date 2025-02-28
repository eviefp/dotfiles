/****************************************************************************
  * programs/streaming module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.streaming;
in
{
  options.evie.programs.streaming = {
    enable = lib.mkEnableOption "qutebrowser defaults";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.davinci-resolve pkgs.ffmpeg-full pkgs.kdePackages.kdenlive ];
    programs = {
      obs-studio = {
        enable = true;
        plugins = [ ];
      };
    };
  };
}
