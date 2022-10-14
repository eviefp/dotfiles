/****************************************************************************
  * programs/editors/vscode module
  *
  **************************************************************************/
{ lib, config, ... }:
let cfg = config.evie.programs.editors.vscode;
in
{
  imports = [ ];

  options.evie.programs.editors.vscode = {
    enable = lib.options.mkEnableOption "Enable vscode";
  };

  config = lib.mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      extensions = [ ];
      keybindings = [ ];
      userSettings = { };
    };
  };
}
