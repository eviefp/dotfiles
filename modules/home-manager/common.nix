/****************************************************************************
  * Common home-manager module
  *
  * Adds the modules/settings that all of my systems use.
  **************************************************************************/
{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeManagerModules; [
    fonts
    system

    editors.emacs
    editors.neovim
    editors.helix

    programs.term
    programs.ranger
    programs.text
  ];

  home.sessionVariables = {
    NIX_PATH = "nixpkgs=${dotfiles.nixpkgs}";
  };
}

