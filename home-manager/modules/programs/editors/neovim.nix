/****************************************************************************
  * Neovim module
  *
  * Neovim package, plugins, and init file.
  **************************************************************************/
{ pkgs, nix-neovim, ... }:
{
  imports = [ ];

  config = {
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

  };
}
