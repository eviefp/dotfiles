/****************************************************************************
  * Network module
  *
  * Set the hostName and interfaces to DHCP. Can also setup additional firewall
  * exceptions and enable WiFi for my laptop.
  *
  * All my systems are normally on the same network and I'm too lazy to setup
  * proper DNS, so I use a hosts file to name them.
  *
  * TODO:
  * I tried to bundle all interfaces together in a list of strings, and using
  *
  * mkMerge (forEach interface (i: { networking.interfaces."${i}".useDHCP = true; } )
  *
  * ... but that errors out with something about infinite recursion.
  **************************************************************************/
{ lib, config, ... }:
let cfg = config.evie.network;
in
{
  imports = [ ];

  options.evie.network = {
    hostName = lib.mkOption {
      type = lib.types.str;
      description = "The hostname for the device.";
    };

    interface = lib.mkOption {
      type = lib.types.str;
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
      };
    };

  };

  config = lib.mkMerge [
    {
      programs.ssh = {
        knownHosts = {
          thelxinoe.publicKey = "AAAAB3NzaC1yc2EAAAADAQABAAACAQDNUm2WLlRueJwT8XUL04+7PZV9P+SyzqpU2EOSppmkCeAI0yXpK/pU1PyDot3QYdDiE/8ZaeywLf+Nje9X+HhUeZJ6b9m1M135162Z3pwp6GlCVJgQovEOWsorzoL5u6vx4fa8HLaEixfQM2/G8FadZDsDlI700bzoztGZeB0KPL7gL9og4UiWaw3mMHvOruZHAPRnw2U3UfJ10OwBcTkD+IV3pcxxNZqeaaufgI1R1w/jLtkGqeX4EtlPpl2PEJtZFy9iWxEbYu65RULadhzc2UoRl5WVfDxYKMe62b9slnatTpQ8GK6Zn0H+YOHwzU40qC4o8bChdXlqvv3te1ZATRcmGIdX4KKYtYSC+mEsxZQKsHOBWm2u636Zr/kNLJH5GOeAqpoebab9CS/1G9n7kU7EILapHzVWLeMT7/dPX3JLvbI7354Ou8gsP1maAl7hjhaw2cvuRmHI6dKLeQHre5V3t/rtNYQitjgjLF9fw3FWYxvDqrQegRidO4+oGXW4BwqfnphNFYdZenJR5Aa6Ww7P4wY7GMFIo7+eLAmpoScmURUWc+LW2kFVS0KpKnSizjM8HBTw+LYp9CNx/WzqUisOt+C+attGiLPRo42vPxEXObUR/xq+JYnORBHTKfttYalw2MObfNsfY6cM83FK/n2HGJNYOowAt0YWdQWyOQ==";
          fractal.publicKey = "AAAAB3NzaC1yc2EAAAADAQABAAACAQC/3r/y2J2XfBbJr6unFzzelcVnErzpC+ZFnOb3kx3E1zjNkH9PPIwJkkQ7JIOTa1m7WT8FzDLVWXHzohnWLuXB+i5rnlgZbKssSlm5e6TQXOS3eK1nGCxiO0p9Gv+sUibNZO2hHzDqnX7f9CtIGLG2R3gxE1SR9c8+ecJ8ee+foASMgCH3tCkZdmNnm86QgDpSM9BS35aipTI5Z8maCMUh5a0+CDkWnsvE6cVVZ3RoeFtM+c3GN2aCcuHz0RxkN6R/jGU+DGeaKiAfp35G0eIOY6mbihngRgZ7UNAdGL3SZXuC4zXtaBfUhx/NmtIRbuDeFE6v1mWszrdTKB5TQj2DfhsOjTb8xFL4PgbEwgvq2pbInGJ5zJrA40O+LbUpyCz6Q+l4aD0cmgiF31jAhWDtbHCqvfXoA1jjStjhyOhcq9lrpug9ppig5C0LsE1Pw4Zip5HhKMD6TXRxer83AiLEmcRaFLgS0qtgDZFFN+YJx114LTrhgTLyz8syDygvAS+I8GxpxU41w2WTxRIXHlcGWRqDey5QGCGu2GqcYOCIjH006YF71iYyGNMjx0yANKIii5zacL71rQce7zuqumVVFvF2lRYMDH6MLJUbAAMFKmEGZPKn31TKJucNzF2qjvx2fEIL9pXK5T7Pd83u4bN4E1MNi0v6yTN9p+lXCaBZiQ==";
          carbon.publicKey = "AAAAB3NzaC1yc2EAAAADAQABAAACAQDGgmxhOsUY3+SPB4aebkBe+bvDHatoQ1F5Ha7wpbHt0zFKholCZpgNOA+udfSW7cZig6Om1uvNeSszL8iyZThLbboAEPm+PHg3qNJYHyWUM1y84yuDGXSZvqMkpR6AuTu4fCZhbZCGoeTo44iRxmWNo9GU+8t7niojtgpIcQ14l7A6Oth/5bAkNTrw9h85n+ikqyYBAn0Zy/+NkepFjsbf9Ljj0qqKWRbqzL8QszKhTzdLXd01hwvNXQtHTUDbW3HkySGnoWKpVTod5xww2C8w9gBhoABrsKFYdgVsk3u+ap0HmBOIdAfpm1qicd3O2zMqVs7lG2/GDd6/SMcqbxc1hwC69YIk7Ltk3PJnP/VOgrI07AFVKxD/KIzfNVxzKNec/f51rEdzC854PYydxommUOC0fqEhYFlQoTZH55aSa0ZQ1orOCQt28UWYesG8VKHH86e/jeoRCzXriWXcm/wFeBenFBSMlSNrgOK0UUObTYe9eWJfV4M+xLhkLJxtmLmB3WvJ7/PEbAgGNdjAFwgCRei1jH34OVQ3fm06MJQGrDJrQkK4QYmaoiXA4saHtD3cC9uID5HI37w0r7gkEeofbJppq4FI2rRgySrmGfYpNtn5yb8w7fpwwYhKAef2QeivPhWbJiN6uH0PaMbhQLQ9T5bjIJAZXlimAGmfBB7FvQ==";
        };
      };
      networking = {
        hostName = cfg.hostName;
        useDHCP = false;
        firewall.allowedTCPPorts = lib.lists.unique
          (builtins.concatLists [ [ 22 80 443 1143 8080 ] cfg.extraPorts ]);
        hosts = {
          "192.168.1.1" = [ "router" ];
          "192.168.1.15" = [ "bridge" ];
          # peroxide self-signs the certificate with the C/N 'nixos'
          # should probably PR a change there
          "192.168.10.177" = [ "thelxinoe" "nixos" ];
          "192.168.10.166" = [ "janus" ];
          "192.168.10.206" = [ "fractal" ];
          "192.168.10.1" = [ "router2" ];
          "192.168.10.25" = [ "aiode" ];
        };
      };
    }
    (lib.mkIf (cfg.interface != "")
      { networking.interfaces."${cfg.interface}".useDHCP = true; }
    )
    (lib.mkIf cfg.wifi.enable (lib.mkMerge [
      {
        networking = {
          networkmanager = {
            enable = true;
            wifi.powersave = false;
          };
        };
      }
      (lib.mkIf (cfg.wifi.interface != "") {
        networking.interfaces."${cfg.wifi.interface}".useDHCP = false;
      })
    ]))
  ];
}
