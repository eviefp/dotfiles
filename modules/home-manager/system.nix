/****************************************************************************
  * system module
  *
  * This exists so I don't have to symlink each system's config to '/etc/nixos'.
  ************************************************************************ */
{ lib, config, osConfig, ... }:
let
  cfg = config.evie.system;
in
{
  options.evie.system = {
    user = lib.mkOption {
      type = lib.types.str;
      description = "username";
      default = "evie";
    };
  };

  config = {
    home.username = cfg.user;
    home.homeDirectory = "/home/${cfg.user}";
    home.stateVersion = osConfig.system.stateVersion;
  };
}
