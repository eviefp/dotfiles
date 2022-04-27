/****************************************************************************
  * Hydroxide module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.hydroxide;
  package = pkgs.callPackage ./hydroxide-package.nix { };
in
{
  imports = [ ];

  options.evie.programs.hydroxide = {
    enable = lib.options.mkEnableOption "Enable hydroxide";
    service = lib.options.mkEnableOption "Enable hydroxide service";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      package
    ];

    systemd.user.services.hydroxide = lib.mkIf cfg.service {
      Unit = {
        Description = "Hydroxide Protonmail Bridge";
      };

      Install = { WantedBy = [ "default.target" ]; };

      Service = {
        Type = "forking";

        ExecStart = "${package}/bin/hydroxide serve";

        Restart = "always";

        RestartSec = 3;
      };
    };
  };
}
