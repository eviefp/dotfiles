/****************************************************************************
  * programs/editors/helix module
  *
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.editors.helix;
in
{
  imports = [ ];

  options.evie.programs.editors.helix = {
    enable = lib.options.mkEnableOption "Enable helix";
  };

  config = lib.mkIf cfg.enable {
    programs.helix = {
      package = pkgs.helix;
      enable = true;
      languages.language = [
        {
          name = "haskell";
          scope = "source.haskell";
          file-types = [ "hs" ];
          roots = [ "*.cabal" ];
          comment-token = "--";
          language-server = {
            command = "haskell-language-server";
            args = [
              "--lsp"
            ];
          };
          config = {
            formattingProvider = "fourmolu";
          };
        }
      ];
      settings = {
        theme = "base16_transparent"; #"nordmod";
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
