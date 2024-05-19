/****************************************************************************
  * Thelxinoe system configuration
  *
  **************************************************************************/
{ ... }:
{
  imports = [
    ./hardware-configuration.nix
    ../modules/nix-settings.nix
    ../modules/boot.nix
    ../modules/network.nix
    ../modules/locale.nix
    ../modules/packages.nix
    ../modules/services.nix
    ../modules/xserver.nix
    ../modules/users.nix
  ];



  evie.network = {
    hostName = "thelxinoe";
    interface = "enp4s0";
    extraPorts = [ 31234 ];
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
    WLR_DRM_NO_ATOMIC = "1";
    LIBVA_DRIVER_NAME = "nvidia";
    XDG_SESSION_TYPE = "wayland";
    GBM_BACKEND = "nvidia-drm";
    NVD_BACKEND = "direct";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __GLX_VRR_ALLOWED = "1";
  };
}

