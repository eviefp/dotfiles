/****************************************************************************
  * Emacs module
  *
  * Emacs package using emacs-overlay.
  **************************************************************************/
{ lib, config, pkgs, nixpkgs, emacs-overlay, ... }:
let
  initFile = ../../../../config/init.el;
  derivation = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    # package = pkgs.emacsGit;
    extraEmacsPackages = epkgs: [ epkgs.rainbow-delimiters ];

    package-desktop = pkgs.emacs-git.override {
      withX = true;
      withGTK3 = true;
    };
    package-term-only = pkgs.emacs-git-nox;
  };
  cfg = config.evie.programs.editors.emacs;
in
{
  imports = [ ];

  options.evie.programs.editors.emacs = {
    enable = lib.options.mkEnableOption "Enable the Emacs package.";

    no-x = lib.options.mkEnableOption "Use terminal only Emacs.";

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
      derivation
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
        enable = false;
        package = if cfg.no-x then package-desktop else package-term-only;
        client = {
          enable = false;
          arguments = [ "-c" ];
        };
        socketActivation.enable = false;
        startWithUserSession = false;
      };
    };
  };
}
