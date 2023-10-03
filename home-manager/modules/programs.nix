/****************************************************************************
  * Programs module
  *
  **************************************************************************/
{ lib, config, pkgs, unstable, ... }:
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
    ./programs/editors/helix.nix
    # ./programs/hydroxide.nix
    ./programs/kitty.nix
    ./programs/neuron.nix
    # ./programs/purebred.nix
    ./programs/shell.nix
    ./programs/shell/ranger.nix
    ./programs/streaming.nix
    ./programs/text.nix
    ./programs/wezterm.nix
  ];

  options.evie.programs = {
    enable = lib.options.mkEnableOption "Enable generic packages.";
  };

  config = lib.mkMerge [
    (
      {
        home.packages = [ unstable.zoom-us pkgs.light ];
      }
    )
  ];
}
