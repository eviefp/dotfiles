/****************************************************************************
  * programs/dev/tools module
  *
  * Enable packages I use for generic development:
  *   - 'gnumake' for Makefiles and such
  *   - 'httpie' for simple http requests
  *   - 'sqlite' for stuff like org roam
  **************************************************************************/
{ pkgs, ... }:
{
  config = {
    home.packages = [
      pkgs.gnumake
      pkgs.httpie
      pkgs.sqlite
    ];
  };
}
