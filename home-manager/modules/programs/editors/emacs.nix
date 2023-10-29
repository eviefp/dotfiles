/****************************************************************************
  * Emacs module
  *
  * Emacs package using emacs-overlay.
  **************************************************************************/
{ lib, config, pkgs, emacs-overlay, ... }:
let
  initFile = ../../../../config/init.el;
  package-desktop = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    extraEmacsPackages = epkgs: [
      epkgs.rainbow-delimiters
      epkgs.org-roam-ui
    ];
    package = pkgs.emacs-git.override {
      withX = true;
      withGTK3 = true;
    };
  };
  package-term-only = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    extraEmacsPackages = epkgs: [ epkgs.rainbow-delimiters ];
    package = pkgs.emacs-git-nox.override { };
  };
  cfg = config.evie.programs.editors.emacs;
in
{
  imports = [ ];

  options.evie.programs.editors.emacs = {
    enable = lib.options.mkEnableOption "Enable the Emacs package.";

    no-x = lib.options.mkEnableOption "Use terminal only Emacs.";

    service = lib.options.mkEnableIOption "Use emacs service.";

    locals = {
      enable = lib.options.mkEnableOption "Enable local config.";

      file = lib.mkOption {
        type = lib.types.path;
        description = "Emacs locals file.";
      };
    };

  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      (if cfg.no-x then package-term-only else package-desktop)
      pkgs.graphviz # dot, needed for org-roam
    ];

    home.file = lib.mkMerge [
      { ".emacs.d/init.el".source = initFile; }
      (lib.mkIf cfg.locals.enable {
        ".emacs.d/locals.el".source = cfg.locals.file;
      })
    ];

    # TODO: This doesn't quite work. Should investigate...?
    services = {
      emacs = {
        enable = cfg.locals.service;
        package = if cfg.no-x then package-term-only else package-desktop;
        client = {
          enable = false;
          arguments = [ "-c" ];
        };
        socketActivation.enable = true;
        startWithUserSession = true;
      };

      nginx = lib.mkIf cfg.locals.service {
        virtualHosts = {
          "fractal.eevie.ro".locations."/org" = {
            proxyWebsockets = true;
            proxyPass = "http://localhost:30010";
          };
        };
      };
    };
  };
}
