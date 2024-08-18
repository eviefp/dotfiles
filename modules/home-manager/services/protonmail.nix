/****************************************************************************
  * Protonmail bridge
  *
  * Needs config/setup first.
  **************************************************************************/
{ lib, config, ... }:
let
  cfg = config.evie.services.protonmail;
  sources = import ../../../nix/sources.nix;
  unstable = import sources.unstable { };
  package = unstable.protonmail-bridge;
in
{
  imports = [ ];

  options.evie.services.protonmail = {
    enable = lib.options.mkEnableOption "Enable protonmail";
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.protonmail = {
      Unit = {
        Description = "Protonmail Bridge";
      };

      Install = { WantedBy = [ "default.target" ]; };

      Service = {
        Type = "forking";

        ExecStart = "${package}/bin/protonmail-bridge --noninteractive -l debug";

        Restart = "always";

        RestartSec = 3;
      };
    };
  };
}
