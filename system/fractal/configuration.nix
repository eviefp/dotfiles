/****************************************************************************
  * Fractal system configuration
  *
  **************************************************************************/
{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../modules/boot.nix
    ../modules/network.nix
    ../modules/locale.nix
    ../modules/packages.nix
    ../modules/services.nix
    ../modules/users.nix
    ../modules/nextcloud.nix
  ];

  evie.boot.enableHeadless = true;

  evie.network = {
    hostName = "fractal";
    interface = "eno1";
    extraPorts = [ 1025 1143 ];
  };

  networking.firewall.allowedUDPPorts = [ 31234 ];

  evie.packages = { extra = [ pkgs.git pkgs.wget ]; };

  evie.nextcloud.enable = true;

  services.iperf3 = {
    enable = true;
    bind = "192.168.10.206";
    port = 31234;
    openFirewall = true;
  };

}
