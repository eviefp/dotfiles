/****************************************************************************
  * programs/shell/ranger module
  *
  * https://github.com/ranger/ranger
  *
  * Setup and install 'ranger'.
  **************************************************************************/
{ pkgs, ... }:
{
  config = {
    home.packages = [
      pkgs.ranger
      pkgs.w3m # for displaying images
    ];
    home.file.".config/ranger/rc.conf".source = ../../../../config/ranger/rc.conf;
  };
}
