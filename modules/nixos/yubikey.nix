/****************************************************************************
  * Yubikey module
  **************************************************************************/
{ config, pkgs, ... }:
{
  config = {
    environment.systemPackages = [ pkgs.yubikey-personalization ];

    services = {
      pcscd.enable = true;
      udev.packages = [ pkgs.yubikey-personalization ];
    };

    security.pam = {
      u2f = {
        enable = true;
        # settings = {
        interactive = true;
        control = "sufficient";
        cue = true;
        origin = "pam://yubi";
        authFile = config.sops.secrets.yubiAuthFile.path;
        # };
      };
      services = {
        login.u2fAuth = true;
        sudo.u2fAuth = true;
        sddm.u2fAuth = true;
        sddm.nodelay = true;
        hyprlock = { };
      };
    };
  };
}
