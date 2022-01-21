/****************************************************************************
  * programs/shell module
  *
  * Enable packages I use for shell-related things:
  *   - 'killall' to easily kill processes
  *   - 'ripgrep' for navigation
  *   - 'wget'
  *   - 'unzip'
  *   - 'zip'
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.shell;
in
{
  imports = [ ];

  options.evie.programs.shell = {
    enable = lib.options.mkEnableOption "Enable shell tools.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.killall
      pkgs.ripgrep
      pkgs.wget
      pkgs.unzip
      pkgs.zip
    ];
  };
}
