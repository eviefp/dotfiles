/****************************************************************************
  * Users module
  *
  * Nothing much to say here. Just my default user, along with the groups I needed
  * for various things.
  **************************************************************************/
{ pkgs, ... }:
{
  config = {
    programs.fish.enable = true;

    users = {
      groups = { plugdev = { }; };
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
          shell = pkgs.fish;
          hashedPassword =
            "$6$2bJFtErxPXqeCEJO$w4K0Fm1WmRL3tpUUJxkesiBFsM03Q2/IrtX9QvJjIBH3bxlOr1VtMIgWhCtIR1B./3QtmBCKo4H8ajTk51JW2/";
        };
      };
    };

    security.sudo.wheelNeedsPassword = false;
  };
}
