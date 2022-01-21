/****************************************************************************
  * programs/shell/ranger module
  *
  * https://github.com/ranger/ranger
  *
  * Setup and install 'ranger'.
  *
  * TODO: expose easy config attribute.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.shell.ranger;
in
{
  imports = [ ];

  options.evie.programs.shell.ranger = {
    enable = lib.options.mkEnableOption "Enable ranger";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.ranger
      pkgs.w3m # for displaying images
    ];
    home.file.".config/ranger/rc.conf".source = ../../../../config/ranger/rc.conf;
  };
}
