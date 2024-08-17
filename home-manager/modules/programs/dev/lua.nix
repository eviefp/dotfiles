/****************************************************************************
  * programs/dev/lua module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  config = {
    home.packages = [
      pkgs.lua5_3
      pkgs.lua53Packages.lua-lsp
      pkgs.lua53Packages.luacheck
      pkgs.lua53Packages.lua-cjson
    ];
  };
}
