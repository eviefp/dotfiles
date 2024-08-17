/****************************************************************************
  * Services module
  *
  * Enable openssh, printing, lorri, and optionally, the XCompose Pause key as
  * the Multi_Key, which I use for typing various math symbols.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.services;
in
{
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

      udev = lib.mkIf cfg.xcompose {
        extraRules = ''
          SUBSYSTEM=="usb", ATTR{idVendor}=="3297", GROUP="plugdev"
          ACTION=="add", SUBSYSTEM=="input", RUN+="${pkgs.xorg.setxkbmap}/bin/setxkbmap -option compose:pause"
        '';
      };
    };

    # TODO: This should be in a docker module which also adds the docker group,
    # as well as my default user to that group.
    virtualisation.docker = { enable = true; };

    virtualisation.virtualbox.host = {
      enable = false;
      enableExtensionPack = false;
    };
    virtualisation.virtualbox.guest = {
      enable = false;
    };

  }];
}
