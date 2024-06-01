/****************************************************************************
  * programs/streaming module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  imports = [ ];

  config = {
    home.packages = [ pkgs.ffmpeg-full pkgs.kdenlive ];

    programs = {
      obs-studio = {
        enable = true;
        plugins = [ ];
      };
    };
  };
}
