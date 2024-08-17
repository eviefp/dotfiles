/****************************************************************************
  * programs/streaming module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  config = {
    # TODO: try davinci 
    home.packages = [ pkgs.ffmpeg-full pkgs.kdenlive ];

    programs = {
      obs-studio = {
        enable = true;
        plugins = [ ];
      };
    };
  };
}
