{ lib, config, pkgs, ... }:
let
  cfg = config.evie.term.scripts;
  nuShellScript = pkgs.callPackage ../../../lib/nuShellScript.nix { };
  scrcpy = nuShellScript
    {
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
