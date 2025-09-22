/**
**************************************************************************
* Users module
*
* Nothing much to say here. Just my default user, along with the groups I needed
* for various things.
*************************************************************************
*/
{
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.evie.users;
in {
  options.evie.users = {
    enable = lib.mkEnableOption "users defaults";
  };

  config = lib.mkIf cfg.enable {
    programs.fish.enable = true;

    users = {
      groups = {plugdev = {};};
      users = {
        evie = {
          isNormalUser = true;
          extraGroups = [
            # allow running sudo commands
            "wheel"
            # allow access to the networkmanager, https://wiki.nixos.org/wiki/NetworkManager
            "networkmanager"
            # allow access to webcams and such
            "video"
            # allow access/control of docker
            "docker"
            # allow mounting and umounting removable devices through pmount
            "plugdev"
            # allow virtualbox
            "vboxusers"
            # access to input devices, potentially not needed
            "input"
            "uinput"
            # printer
            "lp"
          ];
          shell = pkgs.nushell;
          hashedPassword = "$6$2bJFtErxPXqeCEJO$w4K0Fm1WmRL3tpUUJxkesiBFsM03Q2/IrtX9QvJjIBH3bxlOr1VtMIgWhCtIR1B./3QtmBCKo4H8ajTk51JW2/";
          openssh.authorizedKeys.keys = [
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDP+cKsCIfQyFN06ZX1VhfsHvu7FT3F3cwY7fmnZ8kIjmhSz/uYlG4wIJikeZhqWrMsu4ozQaiWdl0zKsw3KpQyyJ8alhfxAtOeBkuifL61Tnq4QOQ2y8/8zziD5eVD2WY2Nw4n8+fT13pAK/117WWOpoWMC1CbRr0YZWoobkABhOIVKDMWj+pUgbmMTAbg1rCyRuQYrlo5dsdK/mWQJRnOFUK0u9mx/lu2Iz22pU1oBxwSJQ2FhyDX/DSKG732paSblC7o4GwiH5uIKmJPDUtT4LCZy9j5DpxD2q5HcQAgsA1xRTSxnAg+8gJXo1BSX7eepLwBtAJJq6IuRKxIRlvq4HA5WEfLvpddNzMOVDNWmVi1i+oO6xZNDbxOUat0G36MzAdhExsBmwiw5ga9soOMu/QqNz4vAQ3/Hnyh17AntrkjekyYoT1nXrGKS+f+sGjoFNDsWwdfDpiDn6B5nCrIb5b5eX3XmCKz1NVLspKyELVVCmFdJfEvxhnFOevNOW3kWSwtQW4VJshcIR8NydkEjWjcezWeIChym+bFBmgjDjYwOxFGyRHDooPwXhmeQDORA7mY/az6tQLSk0UTeoLmavcUI73Qv8aNtuXrAWTG4BJ2rf/+yvs3ygpckZGydq9L7xtFHqHGY3gOnGjcFAp8xnsSVJE5h32/PDxi6j+HBw== cardno:31_472_472"
          ];
        };
      };
    };

    security.sudo.wheelNeedsPassword = false;
  };
}
