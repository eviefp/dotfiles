/****************************************************************************
  * ect module
  **************************************************************************/
{ config, pkgs, ect, ... }:
let
  ectPackage = ect.packages.${pkgs.system}.default;
  ectWrapped = pkgs.writeShellScriptBin "ect" ''
    ${ectPackage}/bin/ect "$@" --config ${config.sops.secrets.ect_yaml.path}
  '';
in
{
  imports = [ ];

  config = {
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
