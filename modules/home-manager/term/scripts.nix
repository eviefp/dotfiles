{ lib, config, pkgs, dotfiles, ... }:
let
  cfg = config.evie.term.scripts;
  scrcpy = dotfiles.self.lib.nuShellScript {
    name = "scrcpy";
    text = ''
      scrcpy --video-codec=h265 -m1920 --max-fps=60 -K --legacy-paste
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
