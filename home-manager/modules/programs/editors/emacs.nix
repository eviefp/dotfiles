/****************************************************************************
  * Emacs module
  *
  * Emacs package using emacs-overlay.
  **************************************************************************/
{ lib, config, pkgs, lean4-mode, ... }:
let
  initFile = ../../../../config/init.el;
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

  package-desktop = pkgs.emacsWithPackagesFromUsePackage {
    config = initFile;
    extraEmacsPackages = epkgs: [
      epkgs.rainbow-delimiters
      epkgs.lean4-mode
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

    override = final: prev: {
      lean4-mode = (lean4mode prev);
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

  config = {
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

    # TODO: This doesn't quite work. Should investigate...?
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
