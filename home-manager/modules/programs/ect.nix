/****************************************************************************
  * ect module
  **************************************************************************/
{ lib, config, pkgs, ect, ... }:
let
  cfg = config.evie.programs.ect;
  ectPackage = ect.packages.${pkgs.system}.default;
  ectWrapped = pkgs.writeShellScriptBin "ect" ''
    ${ectPackage}/bin/ect "$@" --config ${config.sops.secrets.ect_yaml.path}
  '';
in
{
  imports = [ ];

  options.evie.programs.ect = {
    enable = lib.options.mkEnableOption "Enable ect";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      ectWrapped
      pkgs.libnotify
    ];

    systemd.user.services.ect = {
      Unit = {
        Description = "Evie's Calendar Tool";
      };

      Install = { WantedBy = [ "default.target" ]; };

      Service = {
        Type = "exec";

        ExecStart = "${ectWrapped}/bin/ect server";

        Restart = "always";

        RestartSec = 3;
      };
    };
  };
}
