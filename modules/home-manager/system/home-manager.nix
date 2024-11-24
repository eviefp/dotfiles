/****************************************************************************
  * home-manager module
  ************************************************************************ */
{ lib, config, osConfig, dotfiles, ... }:
let
  cfg = config.evie.system.home-manager;
in
{
  options.evie.system.home-manager = {
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

    # TODO: I don't think this is needed?
    # home.sessionVariables = {
    #   NIX_PATH = "nixpkgs=${dotfiles.nixpkgs}";
    # };
  };
}
