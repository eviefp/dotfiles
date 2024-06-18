/****************************************************************************
  * programs/editors/helix module
  *
  **************************************************************************/
{ pkgs, ... }:
{
  imports = [ ];

  config = {
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
          roots = [ "*.cabal" ];
          comment-token = "--";
          language-servers = [ "haskell-language-server" ];
          formatter = {
            command = "ormolu";
            args = [ "--stdin-input-file" "." ];
          };
        }
        {
          name = "nix";
          auto-format = true;
          formatter.command = "nixpkgs-fmt";
          language-servers = [ "nil" ];
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
          cursorcolumn = true;
          gutters = [ "diagnostics" "spacer" "line-numbers" "spacer" "diff" ];
          auto-completion = true;
          auto-format = true;
          bufferline = "always";
          color-modes = true;
          lsp = {
            display-messages = true;
          };
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
            right = [
              "version-control"
              "workspace-diagnostics"
              "selections"
              "register"
              "position"
              "file-encoding"
              "file-line-ending"
            ];
          };
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
