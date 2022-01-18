/*******************************************************************************
 * Users module
 *
 * Nothing much to say here. Just my default user, along with the groups I needed
 * for various things.
 *
 * TODO: document groups.
 ******************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.users;
in {
  imports = [];

  options.evie.users = {
  };

  config = {
    users = {
      groups = {
        plugdev = { };
      };

      users = {
        evie = {
          isNormalUser = true;
          extraGroups = [ "wheel" "networkmanager" "video" "docker" "plugdev" ];
          shell = pkgs.fish;
        };
      };
    };

    security.sudo.wheelNeedsPassword = false;
  };
}
