/****************************************************************************
  * Programs module
  *
  **************************************************************************/
{ dotfiles, ... }:
{
  imports = with dotfiles.self.homeManagerModules.programs.devModules; [
    haskell
    lua
    nix
    provers
    tools
  ];
}
