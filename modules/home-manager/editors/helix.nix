/****************************************************************************
  * editors/helix module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.editors.helix;
in
{
  options.evie.editors.helix = {
    enable = lib.mkEnableOption "helix defaults";
  };

  config = lib.mkIf cfg.enable {
    home.file.".config/helix/themes/gh_dark_transparent.toml".source = ../../../../config/gh_dark_transparent.toml;

    programs.helix = {
      package = pkgs.helix;
      enable = true;
      languages.language = [
        {
          name = "haskell";
          auto-format = true;
          scope = "source.haskell";
          file-types = [ "hs" ];
          roots = [ "flake.nix" "cabal.project" ];
          comment-token = "--";
          language-servers = [ "haskell-language-server" ];
        }
        {
          name = "nix";
          auto-format = true;
          formatter.command = "nixpkgs-fmt";
          language-servers = [ "nil" ];
          indent.tab-width = 2;
          indent.unit = " ";
        }
        {
          name = "rust";
          auto-format = true;
          file-types = [ "rs" ];
          roots = [ "Cargo.toml" "Cargo.lock" ];
          language-servers = [ "rust-analyzer" ];
        }
        {
          name = "purescript";
          auto-format = true;
          formatter.command = "purs-tidy";
          language-servers = [ "purescript-language-server" ];
          roots = [ "spago.yaml" "spago.dhall" ];
          file-types = [ "purs" ];
          indent.tab-width = 2;
          indent.unit = " ";
        }
      ];
      settings = {
        theme = "gh_dark_transparent";
        editor = {
          line-number = "relative";
          mouse = false;
          cursorline = true;
          cursorcolumn = false;
          gutters = [ "diagnostics" "spacer" "line-numbers" "spacer" "diff" ];
          auto-completion = true;
          auto-format = true;
          bufferline = "always";
          color-modes = true;
          rulers = [ 80 100 120 ];
          lsp = {
            display-messages = true;
          };
          auto-pairs = false;
          whitespace = {
            render = "all";
            characters = {
              space = "·";
              nbsp = "⍽";
              tab = "→";
              newline = "⏎";
              tabpad = "·";
            };
          };
          indent-guides = {
            render = true;
            character = "|";
            skip-levels = 0;
          };
          statusline = {
            left = [
              "mode"
              "spinner"
              "file-name"
              "read-only-indicator"
              "file-modification-indicator"
            ];
            center = [
              "diagnostics"
              "workspace-diagnostics"
            ];
            right = [
              "version-control"
              "workspace-diagnostics"
              "selections"
              "register"
              "position"
              "total-line-numbers"
              "file-encoding"
              "file-line-ending"
            ];
          };
        };
        keys.normal = {
          space.c = ":buffer-close";
          space.C = ":buffer-close-others";
        };
      };
      themes = {
        nordmod =
          let
            transparent = {
              fg = "white";
            };
          in
          {
            inherits = "nord";
            "ui.menu" = transparent;
            "ui.background" = transparent;
            "ui.popup" = transparent;
            "ui.window" = transparent;
            "ui.statusline" = transparent;
          };
      };
    };
  };
}
