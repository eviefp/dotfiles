/****************************************************************************
  * Emacs module
  *
  * Emacs package using emacs-overlay.
  **************************************************************************/
{ lib, config, pkgs, emacs-overlay, lean4-mode, kbd-mode, ... }:
let
  initFile = ../../../../config/init.el;
  agendaFile = ../../../../config/agenda.el;
  lean4mode = epkgs: epkgs.trivialBuild {
    pname = "lean4-mode";
    src = lib.cleanSource lean4-mode;
    version = "1";

    buildPhase = ''
      runHook preBuild
      emacs -L . --eval '(setq max-lisp-eval-depth 4000 max-specpdl-size 4000)' --batch -f batch-byte-compile *.el
      runHook postBuild
    '';
    buildInputs = [
      epkgs.dash
      epkgs.f
      epkgs.flycheck
      epkgs.magit-section
      epkgs.lsp-mode
      epkgs.s
    ];
  };

  kbdmode = epkgs: epkgs.trivialBuild {
    pname = "kbd-mode";
    src = lib.cleanSource kbd-mode;
    version = "1";
  };

  package-desktop = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    extraEmacsPackages = epkgs: [
      epkgs.rainbow-delimiters
      epkgs.lean4-mode
      epkgs.kbd-mode
    ];

    package = pkgs.emacs-git.override {
      withX = true;
      withGTK3 = true;
    };

    override = final: prev: {
      lean4-mode = (lean4mode prev);
      kbd-mode = (kbdmode prev);
    };
  };
  package-term-only = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    extraEmacsPackages = epkgs: [
      epkgs.rainbow-delimiters
      epkgs.org-roam-ui
      epkgs.ligature
      epkgs.lean4-mode
    ];

    package = pkgs.emacs-git-nox.override { };

    override = final: prev: {
      lean4-mode = (lean4mode prev);
    };
  };
  cfg = config.evie.programs.editors.emacs;
in
{
  imports = [ ];

  options.evie.programs.editors.emacs = {
    enable = lib.options.mkEnableOption "Enable the Emacs package.";

    no-x = lib.options.mkEnableOption "Use terminal only Emacs.";

    service = lib.options.mkEnableOption "Use emacs service.";

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
          pkgs.gnome.zenity # needed for the color picker
        ];

    home.file = lib.mkMerge [
      { ".emacs.d/init.el".source = initFile; }
      { ".emacs.d/agenda.el".source = agendaFile; } # needed for faster batch mode
      (lib.mkIf cfg.locals.enable {
        ".emacs.d/locals.el".source = cfg.locals.file;
      })
    ];

    # TODO: This doesn't quite work. Should investigate...?
    services = {
      emacs = {
        enable = cfg.service;
        package = if cfg.no-x then package-term-only else package-desktop;
        client = {
          enable = false;
          arguments = [ "-c" ];
        };
        socketActivation.enable = true;
        startWithUserSession = true;
      };
    };
  };
}
