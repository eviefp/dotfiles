/****************************************************************************
  * system module
  *
  * This exists so I don't have to symlink each system's config to '/etc/nixos'.
  ************************************************************************ */
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.system;
in
{
  imports = [ ];

  options.evie.system = {
    enable = lib.options.mkEnableOption "Enable nixos-configuration setup.";
    host = lib.mkOption {
      type = lib.types.str;
      description = "System hostname (should match config directory).";
    };
    dotfiles = lib.mkOption {
      type = lib.types.str;
      description = "Repository base path";
    };
  };

  config = lib.mkIf cfg.enable {
    home.stateVersion = "24.05";
    home.username = "evie";
    home.homeDirectory = "/home/evie";
  };
}
