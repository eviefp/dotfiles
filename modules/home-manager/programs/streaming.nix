/****************************************************************************
  * programs/streaming module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  config = {
    home.packages = [ pkgs.davinci-resolve pkgs.ffmpeg-full pkgs.kdenlive ];

    programs = {
      obs-studio = {
        enable = true;
        plugins = [ ];
      };
    };
  };
}
