/****************************************************************************
  * Thelxinoe home-manager
  *
  **************************************************************************/
{ nix-path, ... }:
{
  imports = [
    ./programs/editors/emacs.nix
    ./programs/editors/neovim.nix
    ./programs/editors/helix.nix
    ./programs/shell.nix
    ./programs/shell/ranger.nix
    ./programs/text.nix
    ./fonts.nix
    ./system.nix
    ./sops.nix
  ];

  evie.system.dotfiles = "/home/evie/code/dotfiles";

  home.sessionVariables = {
    EDITOR = "ed";
    BROWSER = "qutebrowser";
    NIX_PATH = nix-path;
    OOO_FORCE_DESKTOP = "gnome";
  };
}

