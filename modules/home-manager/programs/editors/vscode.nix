/****************************************************************************
  * programs/editors/vscode module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  imports = [ ];

  config = {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      extensions = with pkgs.vscode-extensions; [
        vscode-icons-team.vscode-icons
        redhat.vscode-yaml
        redhat.vscode-xml
        donjayamanne.githistory
        editorconfig.editorconfig
        yzhang.markdown-all-in-one
        mhutchie.git-graph
        vscodevim.vim
        jnoortheen.nix-ide
        haskell.haskell
      ];
      mutableExtensionsDir = false;
      keybindings = [ ];
      userSettings = { };
    };
  };
}
