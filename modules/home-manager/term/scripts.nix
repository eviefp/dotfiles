{ lib, config, ... }:
{
  options.evie.term.scripts = {
    enable = lib.mkEnableOption "scripts";
  };

  config.evie.term.scripts = { };
}
