/****************************************************************************
  * Yubikey module
  **************************************************************************/
{ pkgs, config, ... }:
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
          cue = true;
          control = "sufficient";
          pinverification = 0;
          userverification = 0;
          debug = true;
          origin = "pam://yubi";
          authfile = config.sops.secrets.yubiAuthFile.path;
        };
      };

      services = {
        login.u2fAuth = true;
        sudo.u2fAuth = true;
        sddm.u2fAuth = true;
        sddm-greeter.u2fAuth = true;
        hyprlock.u2fAuth = true;
      };
    };
  };
}
