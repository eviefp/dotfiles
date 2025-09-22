/**
**************************************************************************
* Packages module
*
* Sets default packages I want installed "globally" (as opposed to through
* home-manager).
*************************************************************************
*/
{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.packages;
in {
  options.evie.packages = {
    enable = lib.mkEnableOption "packages defaults";
    extra = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [];
      example =
        lib.options.literalExpression "[ pkgs.firefox pkgs.thunderbird ]";
      description = ''
        The set of packages that appear in
        /run/current-system/sw.  These packages are
        automatically available to all users, and are
        automatically updated every time you rebuild the system
        configuration.  (The latter is the main difference with
        installing them in the default profile,
        <filename>/nix/var/nix/profiles/default</filename>.
      '';
    };
  };

  config = {
    environment.systemPackages = builtins.concatLists [
      [pkgs.cachix pkgs.dbus pkgs.vim pkgs.nfs-utils]
      cfg.extra
    ];

    programs = {
      # this is needed by home-manager
      dconf.enable = true;
    };
  };
}
