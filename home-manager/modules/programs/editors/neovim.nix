/****************************************************************************
  * Neovim module
  *
  * Neovim package, plugins, and init file.
  **************************************************************************/
{ lib, config, pkgs, nix-neovim, ... }:
let
  cfg = config.evie.programs.editors.neovim;
  vimUtils = pkgs.vimUtils;

in
{
  imports = [ ];

  options.evie.programs.editors.neovim = {
    enable = lib.options.mkEnableOption "Enable neovim.";
  };

  config = (lib.mkIf cfg.enable {
    home.file = {
      ".config/nvim/lua" = {
        source = ../../../../config/nvim/lua;
        recursive = true;
      };
    };

    home.packages = [
      pkgs.fd
      pkgs.gcc
      (nix-neovim.neovim-with-packages pkgs)
    ];

  });
}
