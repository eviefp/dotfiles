/****************************************************************************
  * Packages module
  *
  * Sets default packages I want installed "globally" (as opposed to through
  * home-manager). It's usually stuff I need to either boot or setup a new system.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let cfg = config.evie.packages;
in
{
  imports = [ ];

  options.evie.packages = {
    extra = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
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

    enableGPG = lib.mkEnableOption "Enable GPG agent.";
    enableDconf = lib.mkEnableOption "Enable dconf.";
  };

  config = {
    nixpkgs.config.allowUnfree = true;

    environment.systemPackages = builtins.concatLists [
      [ pkgs.cachix pkgs.dbus pkgs.vim pkgs.xorg.xmodmap ]
      cfg.extra
    ];

    programs = {
      gnupg.agent = {
        enable = cfg.enableGPG;
        enableSSHSupport = true;
        pinentryFlavor = "gnome3";
      };

      dconf.enable = cfg.enableDconf;
    };
  };
}
