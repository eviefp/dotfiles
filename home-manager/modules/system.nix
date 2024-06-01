/****************************************************************************
  * system module
  *
  * This exists so I don't have to symlink each system's config to '/etc/nixos'.
  ************************************************************************ */
{ lib, ... }:
{
  imports = [ ];

  options.evie.system = {
    host = lib.mkOption {
      type = lib.types.str;
      description = "System hostname (should match config directory).";
    };
    dotfiles = lib.mkOption {
      type = lib.types.str;
      description = "Repository base path";
      default = "/home/evie/code/dotfiles";
    };
  };

  config = {
    home.stateVersion = "24.05";
    home.username = "evie";
    home.homeDirectory = "/home/evie";
  };
}
