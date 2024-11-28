{ lib, config, pkgs, dotfiles, ... }:
let
  cfg = config.evie.term.scripts;
  nuShellScript = dotfiles.self.packages.${pkgs.system}.nuShellScript;
  scrcpy = nuShellScript {
    name = "scrcpy";
    text = ''
      scrcpy --video-codec=h265 -m1920 --max-fps=60 -K
    '';
    runtimeInputs = [ pkgs.scrcpy ];
  };
in
{
  options.evie.term.scripts = {
    enable = lib.mkEnableOption "scripts";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      scrcpy
    ];
  };
}
