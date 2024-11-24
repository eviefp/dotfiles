/****************************************************************************
  * Network module
  *
  * Set the hostName and interfaces to DHCP. Can also setup additional firewall
  * exceptions and enable WiFi for my laptop.
  *
  * All my systems are normally on the same network and I'm too lazy to setup
  * proper DNS, so I use a hosts file to name them.
  **************************************************************************/
{ lib, config, ... }:
let
  cfg = config.evie.network;
in
{
  options.evie.network = {
    enable = lib.mkEnableOption "network";
    hostName = lib.mkOption {
      type = lib.types.str;
      description = "The hostname for the device.";
    };

    enableWifi = lib.mkEnableOption "wifi";

    extraPorts = lib.mkOption {
      type = lib.types.listOf lib.types.port;
      default = [ ];
      description = "Extra ports to open.";
    };
  };

  config.networking = lib.mkMerge [
    {
      hostName = cfg.hostName;
      useDHCP = true;
      firewall.allowedTCPPorts = lib.lists.unique
        (builtins.concatLists [ [ 22 80 443 1143 8080 ] cfg.extraPorts ]);
      hosts = {
        "192.168.1.1" = [ "router" ];
        "192.168.1.15" = [ "bridge" ];
        "192.168.10.177" = [ "thelxinoe" ];
        "192.168.10.166" = [ "janus" ];
        "192.168.10.206" = [ "fractal" ];
        "192.168.10.1" = [ "router2" ];
        "192.168.10.25" = [ "aiode" ];
        "192.168.10.67" = [ "arche" ];
      };
    }
    (lib.mkIf cfg.enableWifi {
      wireless = {
        enable = true;
        iwd.enable = true;
      };
    })
  ];
}
