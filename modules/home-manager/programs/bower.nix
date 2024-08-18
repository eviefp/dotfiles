/****************************************************************************
  * programs/bower module
  *
  **************************************************************************/
{ pkgs, ... }:
{
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
