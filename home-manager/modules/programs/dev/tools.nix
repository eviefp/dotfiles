/****************************************************************************
  * programs/dev/tools module
  *
  * Enable packages I use for generic development:
  *   - 'gnumake' for Makefiles and such
  *   - 'httpie' for simple http requests
  *   - 'sqlite' for stuff like org roam
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs.dev.tools;
in
{
  imports = [ ];

  options.evie.programs.dev.tools = {
    enable = lib.options.mkEnableOption "Enable dev tools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.gnumake
      pkgs.httpie
      pkgs.sqlite
    ];
  };
}
