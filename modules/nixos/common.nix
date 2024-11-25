/****************************************************************************
  * Common module
  *
  * This is basically just a shorthand for what most of my systems use.
  **************************************************************************/
{ dotfiles, lib, config, ... }:
let
  cfg = config.evie.common;
  removeCommon = n: _: n != "common";
in
{
  options.evie.common = {
    enable = lib.mkEnableOption "common config";
  };

  imports = lib.attrValues (lib.filterAttrs removeCommon dotfiles.self.nixosModules);

  config = lib.mkIf cfg.enable {
    evie = {
      boot.enable = true;
      locale.enable = true;
      nix-settings.enable = true;
      packages = {
        enable = true;
        enableDconf = true;
      };
      services.enable = true;
      sops.enable = true;
      users.enable = true;
    };

    system.stateVersion = "25.05";
  };
}
