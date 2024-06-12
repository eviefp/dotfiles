/****************************************************************************
  * Users module
  *
  * Nothing much to say here. Just my default user, along with the groups I needed
  * for various things.
  *
  * TODO: document groups.
  **************************************************************************/
{ pkgs, ... }:
{
  imports = [ ];

  options.evie.users = { };

  config = {
    programs.fish.enable = true;
    users = {
      groups = { plugdev = { }; };


      users = {
        evie = {
          isNormalUser = true;
          extraGroups = [ "wheel" "networkmanager" "video" "docker" "plugdev" "vboxusers" "input" "uinput" "lp" ];
          shell = pkgs.fish;
          hashedPassword =
            "$6$2bJFtErxPXqeCEJO$w4K0Fm1WmRL3tpUUJxkesiBFsM03Q2/IrtX9QvJjIBH3bxlOr1VtMIgWhCtIR1B./3QtmBCKo4H8ajTk51JW2/";
        };
      };
    };

    security.sudo.wheelNeedsPassword = false;
  };
}
