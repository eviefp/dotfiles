/****************************************************************************
  * system module
  *
  * This exists so I don't have to symlink each system's config to '/etc/nixos'.
  ************************************************************************ */
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.system;
  sources = import ../../nix/sources.nix;
  home-manager = import sources.home-manager { };
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
    home.sessionVariables = {
      NIX_PATH =
        "nixpkgs=${sources.nixpkgs}:home-manager=${sources.home-manager}:nixos-config=${cfg.dotfiles}/system/${cfg.host}/configuration.nix";
    };
  };
}
