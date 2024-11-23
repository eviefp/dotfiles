{ pkgs, ... }:
let
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
  home.packages = [
    scrcpy
  ];
}
