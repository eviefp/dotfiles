/**
**************************************************************************
* dev/tools module
*
* Enable packages I use for generic development:
*   - 'gnumake' for Makefiles and such
*   - 'httpie' for simple http requests
*   - 'sqlite' for stuff like org roam
*************************************************************************
*/
{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.dev.tools;
in {
  options.evie.dev.tools = {
    enable = lib.mkEnableOption "tools defaults";
  };
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.gnumake
      pkgs.httpie
      pkgs.sqlite
    ];
  };
}
