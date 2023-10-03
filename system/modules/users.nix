/****************************************************************************
  * Users module
  *
  * Nothing much to say here. Just my default user, along with the groups I needed
  * for various things.
  *
  * TODO: document groups.
  **************************************************************************/
{ config, pkgs, ... }:
{
  imports = [ ];

  options.evie.users = { };

  config = {
    users = {
      groups = { plugdev = { }; };

      users = {
        evie = {
          isNormalUser = true;
          extraGroups = [ "wheel" "networkmanager" "video" "docker" "plugdev" "vboxusers" ];
          shell = pkgs.bash;
          hashedPassword =
            "$6$2bJFtErxPXqeCEJO$w4K0Fm1WmRL3tpUUJxkesiBFsM03Q2/IrtX9QvJjIBH3bxlOr1VtMIgWhCtIR1B./3QtmBCKo4H8ajTk51JW2/";
        };
      };
    };

    nix = {
      settings.trusted-users = [ "root" "evie" ];
      package = pkgs.nixFlakes;
      nrBuildUsers = 50;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };

    security.sudo.wheelNeedsPassword = false;
  };
}
