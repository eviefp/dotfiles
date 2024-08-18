/****************************************************************************
  * ect module
  **************************************************************************/
{ dotfiles, config, pkgs, ... }:
let
  ectPackage = dotfiles.ect.packages.${pkgs.system}.default;
  ectWrapped = pkgs.writeShellScriptBin "ect" ''
    ${ectPackage}/bin/ect "$@" --config ${config.sops.secrets.ect_yaml.path}
  '';
in
{
  config = {
    home.packages = [
      ectWrapped
      pkgs.libnotify
    ];

    systemd.user.services.ect = {
      Unit = {
        Description = "Evie's Calendar Tool";
        After = [ "sops-nix.service" ];
      };

      Install = { WantedBy = [ "hyprland-session.target" ]; };

      Service = {
        Type = "exec";

        ExecStart = "${ectWrapped}/bin/ect server";

        Restart = "always";

        RestartSec = 3;
      };
    };
  };
}
