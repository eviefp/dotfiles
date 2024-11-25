/****************************************************************************
  * Services module
  *
  * Enable openssh, printing, virtualisation.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.services;
in
{
  options.evie.services = {
    enable = lib.mkEnableOption "enable services";
  };

  config = lib.mkIf cfg.enable {
    services = {
      openssh.enable = true;

      printing = {
        enable = true;
        drivers = [ pkgs.hplip pkgs.gutenprint ];
      };
    };

    virtualisation.docker = { enable = true; };

    virtualisation.virtualbox.host = {
      enable = false;
      enableExtensionPack = false;
    };
    virtualisation.virtualbox.guest = {
      enable = false;
    };

  };
}
