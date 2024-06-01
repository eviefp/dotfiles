/****************************************************************************
  * programs/bower module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  imports = [ ];

  config = {
    home.packages = [
      pkgs.notmuch-bower
      pkgs.ncurses
      pkgs.lynx
      pkgs.gpgme
      pkgs.file
    ];
  };
}
