{ config, lib, ... }:
let
  cfg = config.evie.openvpn-server;
  vpn-device = "tun0";
  port = 1194;
  domain = "jellyfin.evie.ro";
  client-key = config.sops.secrets.thelxinoe-openvpn-key.path;
  client-config = ''
    dev tun
    remote "${domain}"
    ifconfig 10.8.0.2 10.8.0.1
    port ${toString port}

    cipher AES-256-CBC
    auth-nocache

    comp-lzo
    keepalive 10 60
    resolv-retry infinite
    nobind
    persist-key
    persist-tun
    secret ${client-key}
  '';
in
{
  options.evie.openvpn-server = {
    enable = lib.mkEnableOption "openvpn defaults";
    client-config = lib.mkOption {
      type = lib.types.str;
    };
  };


  config = lib.mkMerge
    [
      (lib.mkIf cfg.enable {
        networking.nat = {
          enable = true;
          externalInterface = "enp1s0";
          internalInterfaces = [ vpn-device ];
        };
        networking.firewall.allowedUDPPorts = [ port ];
        services.openvpn.servers.thelxinoe.config = ''
          dev ${vpn-device}
          proto udp
          ifconfig 10.8.0.1 10.8.0.2
          secret ${client-key}
          port ${toString port}

          cipher AES-256-CBC
          auth-nocache

          comp-lzo
          keepalive 10 60
          ping-timer-rem
          persist-tun
          persist-key
        '';

        # todo: do I really need this?
        environment.etc."openvpn/thelxinoe-client.ovpn" = {
          text = client-config;
          mode = "600";
        };
      })
      {
        evie.openvpn-server.client-config = client-config;
      }
    ];
}


