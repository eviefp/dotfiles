/**
**************************************************************************
* Yubikey module
*************************************************************************
*/
{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.evie.yubikey;
in {
  options.evie.yubikey = {
    enable = lib.mkEnableOption "yubikey defaults";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [pkgs.yubikey-personalization pkgs.yubikey-manager];

    services = {
      pcscd.enable = true;
      udev.packages = [pkgs.yubikey-personalization];
      dbus.packages = [pkgs.gcr];
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
