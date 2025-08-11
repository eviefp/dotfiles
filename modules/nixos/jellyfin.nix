{ config, lib, ... }:
let
  cfg = config.evie.jellyfin;
in
{
  imports = [ ];

  options.evie.jellyfin = {
    enable = lib.options.mkEnableOption "Enable jellyfin server.";
  };

  config = lib.mkIf cfg.enable {
    services.jellyfin = {
      enable = true;
      openFirewall = true;
    };
  };
}
