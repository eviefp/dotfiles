/****************************************************************************
  * Programs module
  *
  **************************************************************************/
{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeManagerModules.programs.dev; [
    haskell
    lua
    nix
    provers
    tools
  ];
}
