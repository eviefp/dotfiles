#*****************************************************************************
# Services module
#
# Enable openssh, printing, lorri, and optionally, the XCompose CapsLock key as
# the Multi_Key, which I use for typing various math symbols.
#****************************************************************************
{ lib, config, pkgs, ... }:
let cfg = config.evie.services;
in {
  imports = [ ];

  options.evie.services = {
    xcompose = lib.options.mkEnableOption "Enable XCompose Multi_Key.";
  };

  config = lib.mkMerge [{
    services = {
      openssh.enable = true;

      printing = {
        enable = true;
        drivers = [ pkgs.hplip pkgs.gutenprint ];
      };

      lorri.enable = true;

      udev = lib.mkIf cfg.xcompose {
        extraRules = ''
          SUBSYSTEM=="usb", ATTR{idVendor}=="3297", GROUP="plugdev"
          ACTION=="add", SUBSYSTEM=="input", RUN+="${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:none"
          ACTION=="add", SUBSYSTEM=="input", RUN+="${pkgs.xorg.xmodmap}/bin/xmodmap -e \"keycode 66 = Multi_key\""
        '';
        path = [ pkgs.xorg.xmodmap pkgs.xorg.setxkbmap ];
      };
    };

    # TODO: This should be in a docker module which also adds the docker group,
    # as well as my default user to that group.
    virtualisation.docker = { enable = true; };

    # I don't recall what this is. Going to randomly leave it here.
    nix.extraOptions = ''
      binary-caches-parallel-connections = 5
    '';
  }];
}
