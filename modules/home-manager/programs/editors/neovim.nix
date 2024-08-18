/****************************************************************************
  * Neovim module
  *
  * Neovim package, plugins, and init file.
  **************************************************************************/
{ dotfiles, pkgs, ... }:
{
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
      dotfiles.nix-neovim.packages.${pkgs.system}.neovim-with-packages
    ];

  };
}
