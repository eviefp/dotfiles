/****************************************************************************
  * programs/dev/lua module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.dev.lua;
in
{
  imports = [ ];

  options.evie.programs.dev.lua = {
    enable = lib.options.mkEnableOption "Enable lua";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.lua5_3
      pkgs.lua53Packages.lua-lsp
      pkgs.lua53Packages.luacheck
      pkgs.lua53Packages.lua-cjson
    ];
  };
}
