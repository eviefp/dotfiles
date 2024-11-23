/****************************************************************************
  * home-manager module
  ************************************************************************ */
{ lib, config, osConfig, ... }:
let
  cfg = config.evie.system.home-manager;
in
{
  options.evie.system = {
    enable = lib.mkEnableOption "home-manager defaults";

    user = lib.mkOption {
      type = lib.types.str;
      description = "username";
      default = "evie";
    };
  };

  config = lib.mkIf cfg.enable {
    home.username = cfg.user;
    home.homeDirectory = "/home/${cfg.user}";
    home.stateVersion = osConfig.system.stateVersion;
  };
}
