{ config, lib, ... }:
let
  cfg = config.evie.openvpn-client;
in
{
  options.evie.openvpn-client = {
    enable = lib.mkEnableOption "openvpn defaults";
  };

  config = lib.mkIf cfg.enable {
    services.openvpn.servers.jelyfin = {
      config = config.evie.openvpn-server.client-config;
    };
  };
}

