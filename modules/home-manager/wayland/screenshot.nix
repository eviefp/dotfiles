{ config, lib, pkgs, dotfiles, ... }:
let
  cfg = config.evie.wayland.screenshot;
  screenshot = dotfiles.self.lib.nuShellScript {
    name = "screenshot";
    text = ''
      grimblast copy area
    '';
    runtimeInputs = [
      pkgs.grimblast
    ];
  };
in
{
  options.evie.wayland.screenshot = {
    enable = lib.mkEnableOption "screenshot utils";
    package = lib.mkOption {
      type = lib.types.package;
      default = screenshot;
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = [
      screenshot
    ];
  };
}
