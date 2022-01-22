/****************************************************************************
  * Emacs module
  *
  * Emacs package using emacs-overlay.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  sources = import ../../../../nix/sources.nix;
  emacsOverlay = import sources.emacs-overlay;
  initFile = ../../../../config/init.el;
  derivation = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    package = pkgs.emacsGit;
    extraEmacsPackages = epkgs: [ epkgs.rainbow-delimiters ];
  };
  cfg = config.evie.programs.editors.emacs;
in
{
  imports = [ ];

  options.evie.programs.editors.emacs = {
    enable = lib.options.mkEnableOption "Enable the Emacs package.";

    locals = {
      enable = lib.options.mkEnableOption "Enable local config.";

      file = lib.mkOption {
        type = lib.types.path;
        description = "Emacs locals file.";
      };
    };

  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [ emacsOverlay ];

    home.packages = [ derivation ];

    home.file = lib.mkMerge [
      { ".emacs.d/init.el".source = initFile; }
      (lib.mkIf cfg.locals.enable {
        ".emacs.d/locals.el".source = cfg.locals.file;
      })
    ];

    # TODO: This doesn't quite work. Should investigate...?
    services = {
      emacs = {
        enable = true;
        package = derivation;
        client = {
          enable = true;
          arguments = [ "-c" ];
        };
        # socketActivation.enable = true;
      };
    };
  };
}
