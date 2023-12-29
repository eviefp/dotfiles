/****************************************************************************
  * Thelxinoe system configuration
  *
  **************************************************************************/
{ ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../modules/boot.nix
    ../modules/network.nix
    ../modules/locale.nix
    ../modules/packages.nix
    ../modules/services.nix
    ../modules/xserver.nix
    ../modules/users.nix
    ../modules/kmonad.nix
  ];

  evie.kmonad.enable = true;

  evie.network = {
    hostName = "thelxinoe";
    interface = "enp4s0";
    extraPorts = [ 31337 ];
  };

  evie.packages = {
    enableGPG = true;
    enableDconf = true;
  };

  evie.services.xcompose = false;

  evie.xserver = {
    enable = true;
    useNVidia = true;
    useBluetooth = true;
  };

  services = {
    peroxide = {
      enable = true;
      settings = {
        ServerAddress = "nixos";
      };
    };
  };

  environment.variables = {
    WLR_NO_HARDWARE_CURSOR = "1";
    WLR_NO_HARDWARE_CURSORS = "1";
    LIBVA_DRIVER_NAME = "nvidia";
    XDG_SESSION_TYPE = "wayland";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
  };
}

