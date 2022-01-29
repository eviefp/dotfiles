/****************************************************************************
  * Programs module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.programs;
in
{
  imports = [
    ./programs/chat.nix
    ./programs/dev/haskell.nix
    ./programs/dev/lua.nix
    ./programs/dev/nix.nix
    ./programs/dev/provers.nix
    ./programs/dev/tools.nix
    ./programs/editors/vscode.nix
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
    (lib.mkIf cfg.enable {
      nixpkgs.config.allowUnfree = true;
      # home.packages = [
      #   pkgs.graphviz
      #   pkgs.nodejs

      #   pkgs.gnome3.zenity
      #   pkgs.lua
      #   pkgs.lua51Packages.luabitop
      #   pkgs.networkmanagerapplet
      # ];
    })
  ];
}
