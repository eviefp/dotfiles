/****************************************************************************
  * Emacs module
  *
  * Emacs package using emacs-overlay.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.editors.emacs;
  initFile = ../../../config/init.el;
  package-desktop = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;

    defaultInitFile = true;
    alwaysEnsure = true;

    extraEmacsPackages = epkgs: [
      epkgs.rainbow-delimiters
      epkgs.treesit-grammars.with-all-grammars
    ];

    package = pkgs.emacs-git.override {
      withX = false;
      withPgtk = true;
      withGTK3 = true;
      withTreeSitter = true;
      withNativeCompilation = true;
      withWebP = true;
    };
  };
  package-term-only = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    extraEmacsPackages = epkgs: [
      epkgs.rainbow-delimiters
      epkgs.org-roam-ui
      epkgs.ligature
    ];

    package = pkgs.emacs-git-nox.override { };
  };
in
{
  options.evie.editors.emacs = {
    enable = lib.mkEnableOption "emacs defaults";

    no-x = lib.options.mkEnableOption "Use terminal only Emacs.";

    service = lib.options.mkOption {
      description = "Use emacs service.";
      type = lib.types.bool;
      default = true;
    };

    locals = {
      enable = lib.options.mkEnableOption "Enable local config.";

      file = lib.mkOption {
        type = lib.types.path;
        description = "Emacs locals file.";
      };
    };

  };

  config = lib.mkIf cfg.enable {
    home.packages =
      if cfg.no-x
      then [ package-term-only ]
      else
        [
          package-desktop
          pkgs.python3 # needed by emacs-elisp-autofmt
          pkgs.graphviz # dot, needed for org-roam
          pkgs.tree-sitter
        ];

    home.file = lib.mkMerge [
      { ".emacs.d/init.el".source = initFile; }
      (lib.mkIf cfg.locals.enable {
        ".emacs.d/locals.el".source = cfg.locals.file;
      })
    ];

    services = {
      emacs = {
        enable = cfg.service;
        package = if cfg.no-x then package-term-only else package-desktop;
        client = {
          enable = true;
          arguments = [ "-c" ];
        };
        socketActivation.enable = true;
        startWithUserSession = true;
      };
    };
  };
}
