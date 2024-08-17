/****************************************************************************
  * programs/bower module
  *
  * TODO: am I really using this?
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
