{ lib, config, pkgs, dotfiles, ... }:
let
  cfg = config.evie.term.scripts;
in
{
  options.evie.term.scripts = {
    enable = lib.mkEnableOption "scripts";
  };

  config.evie.term.scripts = { };
}
