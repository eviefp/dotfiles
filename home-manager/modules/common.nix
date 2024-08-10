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
    ./programs/term.nix
    ./programs/shell/ranger.nix
    ./programs/text.nix
    ./fonts.nix
    ./system.nix
  ];

  home.sessionVariables = {
    EDITOR = "hx";
    BROWSER = "qutebrowser";
    NIX_PATH = nix-path;
    OOO_FORCE_DESKTOP = "gnome";
  };
}

