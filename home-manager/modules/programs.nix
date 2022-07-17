/****************************************************************************
  * Programs module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs;
  sources = import ../../nix/sources.nix;
  unstable = import sources.unstable { };
in
{
  imports = [
    ./programs/bower.nix
    ./programs/chat.nix
    ./programs/dev/haskell.nix
    ./programs/dev/lua.nix
    ./programs/dev/nix.nix
    ./programs/dev/provers.nix
    ./programs/dev/tools.nix
    ./programs/editors/vscode.nix
    ./programs/hcs.nix
    ./programs/hydroxide.nix
    ./programs/neuron.nix
    ./programs/shell.nix
    ./programs/shell/ranger.nix
    ./programs/streaming.nix
    ./programs/text.nix
    ./programs/wezterm.nix
    ./programs/kitty.nix
  ];

  options.evie.programs = {
    enable = lib.options.mkEnableOption "Enable generic packages.";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      nixpkgs.config.allowUnfree = true;
    })
    (
      {
        home.packages = [ unstable.zoom-us ];
      }
    )
  ];
}
