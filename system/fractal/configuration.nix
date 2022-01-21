{ config, pkgs, ... }:
let common = import ../common.nix { inherit config pkgs; };
in {
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

  evie.packages = { extra = [ pkgs.git pkgs.wget ]; };

  evie.nextcloud.enable = true;

}
