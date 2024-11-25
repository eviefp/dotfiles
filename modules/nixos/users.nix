/****************************************************************************
  * Users module
  *
  * Nothing much to say here. Just my default user, along with the groups I needed
  * for various things.
  **************************************************************************/
{ pkgs, lib, config, ... }:
let
  cfg = config.evie.users;
in
{
  options.evie.users = {
    enable = lib.mkEnableOption "users defaults";
  };

  config = lib.mkIf cfg.enable {
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
          openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCgyRLzazCIXAc+y6Tsgy6ryX+sYTVZEjHhaGYFGqUEoGMwygKUZmO9ykBBb6qxpeAe7q7d/RfxHNsLNyY2DTZmh5niNT8Rpr5EHcWDoD7VBUee0GZ4I/ne/KXrcK7QGQ6rejIDc2FMJtWe+AosyGp2YHVSDpp19ViFk6HUQ1PfYCfwAzbIjfIIkTZfdLAwkzgMDGhYQHWZEv5Bfl2p6DNqXFPAR6jp8eglrp/7tXfayK7Wuw1OR3J7ydEigtOX253cpn7Syv/R8azJAoREFN6QhviiNopUoPI/EbulxY/ABEgR0BShVfgPXv039IjrnP82yVJxsxL5R73B/6nQfYyBsdc9mUs738Wa8G7LHBAd2LuWGsrx0jPSkhYVfvUv6a/1Lg/UUSXC946ohB+od6LROAEtKxU5Ke8LgDABMF/wX6ZCmJNj0eiS7FdPxL8x+7g4LuHip2MyRQNR2O/37oFPh2evMsjUNWLQhfikvgCzlb7ZMlSjzitK/P5OSydXJV8862EZ567GifUWv7/UaYytam9wgnjxzTUYSVcJ1xjWBlGwu3HNWB5L4EYXKQVpgqFo0wmo6xUKiqUzXURUwYipISnZrPK1wNGY3+zJGXHqZTKTJeqC/oqEXAO8vjGYdaMBitV7dR7qWG9Hz49Zs4MBmAnreah0cVZYgpxmqhsgVQ== cardno:31_472_473" ];
        };
      };
    };

    security.sudo.wheelNeedsPassword = false;
  };
}
