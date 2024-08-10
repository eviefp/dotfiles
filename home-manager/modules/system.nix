/****************************************************************************
  * system module
  *
  * This exists so I don't have to symlink each system's config to '/etc/nixos'.
  ************************************************************************ */
{ lib, config, ... }:
let
  cfg = config.evie.system;
in
{
  imports = [ ];

  options.evie.system = {
    host = lib.mkOption {
      type = lib.types.str;
      description = "System hostname (should match config directory).";
    };

    user = lib.mkOption {
      type = lib.types.str;
      description = "username";
      default = "evie";
    };
  };

  config = {
    home.stateVersion = "24.11";
    home.username = cfg.user;
    home.homeDirectory = "/home/${cfg.user}";
  };
}
