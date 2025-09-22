/**
**************************************************************************
* dev/lua module
*
*************************************************************************
*/
{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.dev.lua;
in {
  options.evie.dev.lua = {
    enable = lib.mkEnableOption "lua defaults";
  };
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.lua5_3
      pkgs.lua53Packages.lua-cjson
    ];
  };
}
