/****************************************************************************
  * Yubikey module
  **************************************************************************/
{ pkgs, ... }:
let
  key = "evie:l5rCPM1n8ApINtpaomR6TUPO+kgmV3DrOvwuJ27b7pnRQV35UFJ8P9VL61PZIZxZS1vft8r+f70Og4H20YFt4w==,UTsPZfb7kufe/r43I51K61lpqkcvOhStOU5l3QgntBDnyedHf97JY3KqNivAePsElQGV8tdWChiUNj8I+ZQlrA==,es256,+presence";
in
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
          authfile = pkgs.writeText "u2f_mapping" key; # config.sops.secrets.yubiAuthFile.path;
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
