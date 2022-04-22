/****************************************************************************
  * neuron module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  sources = import ../../../nix/sources.nix;
  neuron = import sources.neuron;
  cfg = config.evie.programs.neuron;
in
{
  imports = [ ];

  options.evie.programs.neuron = {
    enable = lib.options.mkEnableOption "Enable neuron";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      neuron.default
    ];
  };
}
