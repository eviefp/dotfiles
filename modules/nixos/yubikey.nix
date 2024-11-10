/****************************************************************************
  * Yubikey module
  **************************************************************************/
{ config, pkgs, ... }:
{
  config = {
    environment.systemPackages = [ pkgs.yubikey-personalization pkgs.yubikey-manager ];

    services = {
      pcscd.enable = true;
      udev.packages = [ pkgs.yubikey-personalization ];
      dbus.packages = [ pkgs.gcr ];
    };

    security.pam = {
      u2f = {
        enable = true;
        settings = {
          interactive = true;
          control = "sufficient";
          cue = true;
          origin = "pam://yubi";
          authFile = config.sops.secrets.yubiAuthFile.path;
        };
      };
      services = {
        login.u2fAuth = true;
        sudo.u2fAuth = true;
        sddm.u2fAuth = true;
        sddm.nodelay = true;
        hyprlock.u2fAuth = true;
      };
    };
  };
}
