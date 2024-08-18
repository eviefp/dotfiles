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
let cfg = config.evie.network;
in
{
  options.evie.network = {
    hostName = lib.mkOption {
      type = lib.types.str;
      description = "The hostname for the device.";
    };

    interface = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Ethernet device name.";
    };

    extraPorts = lib.mkOption {
      type = lib.types.listOf lib.types.port;
      default = [ ];
      description = "Extra ports to open.";
    };

    wifi = {
      enable = lib.options.mkEnableOption "Enable WiFi.";

      interface = lib.mkOption {
        type = lib.types.str;
        description = "WiFi device name.";
        default = "";
      };
    };

  };

  config.networking = lib.mkMerge [
    {
      hostName = cfg.hostName;
      useDHCP = false;
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
    (lib.mkIf (cfg.interface != "")
      { interfaces."${cfg.interface}".useDHCP = true; }
    )
    (lib.mkIf cfg.wifi.enable (lib.mkMerge [
      {
        networkmanager = {
          enable = true;
          wifi.powersave = false;
        };
      }
      (lib.mkIf (cfg.wifi.interface != "") {
        interfaces."${cfg.wifi.interface}".useDHCP = false;
      })
    ]))
  ];
}
