/****************************************************************************
  * hcs module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  sources = import ../../../nix/sources.nix;
  hcs = import sources.hcs { pkgs = pkgs; };
  cfg = config.evie.programs.hcs;
in
{
  imports = [ ];

  options.evie.programs.hcs = {
    enable = lib.options.mkEnableOption "Enable hcs";
    # These are secrets which should really not be in the nix store.
    # configPath = lib.mkOption {
    #   type = lib.types.path;
    #   description = "Path to 'hcs.toml'.";
    # };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      hcs
    ];
    # home.file.".config/hcs/hcs.toml".path = cfg.configPath;
  };
}
