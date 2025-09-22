/**
**************************************************************************
* editors/helix module
*
*************************************************************************
*/
{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.evie.editors.helix;
in {
  options.evie.editors.helix = {
    enable = lib.mkEnableOption "helix defaults";
  };

  config = lib.mkIf cfg.enable {
    home.file.".config/helix/themes/gh_dark_transparent.toml".source = ./../../../config/gh_dark_transparent.toml;
    home.file.".config/helix/themes/cyberdream.toml".text = ''
      # cyberdream theme for helix
      "ui.background" = "bg_highlight" # file picker border color
      "ui.text" = "fg"
      "ui.text.focus" = "magenta" # file picker selection color
      "ui.cursor" = { bg = "fg", fg = "bg" }
      "ui.linenr" = "grey"
      "ui.statusline" = { fg = "cyan" }
      "ui.selection" = "green"
      "ui.selection.primary" = "magenta"
      "ui.virtual.ruler" = { bg = "bg_highlight" }

      # Syntax Highlighting for Markdown
      "markup.raw" = { fg = "magenta", bg = "bg" }
      "markup.raw.inline" = { fg = "pink" }
      "markup.heading.1" = { fg = "cyan", bg = "bg"}
      "markup.heading.2" = { fg = "blue", bg = "bg" }
      "markup.heading.3" = { fg = "green", bg = "bg" }
      "markup.heading.4" = { fg = "magenta", bg = "bg" }
      "markup.heading.5" = { fg = "pink", bg = "bg" }
      "markup.heading.6" = { fg = "orange", bg = "bg" }
      "markup.list" = { fg = "magenta" }
      "markup.bold" = { fg = "pink", modifiers = ["bold"] }
      "markup.italic" = { fg = "pink" }
      "markup.strikethrough" = { fg = "red", modifiers = ["crossed_out"] }
      "markup.link" = { fg = "cyan" }
      "markup.quote" = { fg = "fg" }

      # Syntax Highlighting for Code
      "comment" = { fg = "grey", modifiers = ["italic"] }
      "comment.line" = { fg = "grey", modifiers = ["italic"] }
      "comment.block" = { fg = "grey", modifiers = ["italic"] }
      "comment.documentation" = { fg = "blue", modifiers = ["italic"] }
      "keyword" = "orange"
      "keyword.control" = "orange"
      "keyword.operator" = "pink"
      "keyword.function" = "orange"
      "type" = "cyan"
      "type.builtin" = "cyan"
      "function" = "blue"
      "function.builtin" = "pink"
      "function.method" = "blue"
      "variable" = "fg"
      "variable.builtin" = "magenta"
      "variable.parameter" = "cyan"
      "string" = "green"
      "string.special" = "pink"
      "constant" = "fg"
      "constant.builtin" = "red"
      "constant.numeric" = "yellow"
      "constant.character" = "pink"
      "constant.boolean" = "red"
      "attribute" = "magenta"
      "operator" = "purple"
      "tag" = { fg = "purple", modifiers = ["bold"] }
      "tag.special" = { fg = "orange", modifiers = ["bold"] }
      "namespace" = "purple"
      "macro" = "orange"
      "label" = "red"

      # Interface specific
      "ui.cursorline.primary" = { bg = "bg_highlight" }
      "ui.cursorline.secondary" = { bg = "bg_alt" }
      "ui.cursorcolumn.primary" = { bg = "bg_highlight" }
      "ui.cursorcolumn.secondary" = { bg = "bg_alt" }
      "ui.statusline.normal" = { fg = "bg", bg = "cyan" }
      "ui.statusline.insert" = { fg = "bg", bg = "green" }
      "ui.statusline.select" = { fg = "bg", bg = "magenta" }

      # Diagnostic styles
      "warning" = { fg = "yellow", modifiers = ["bold"] }
      "error" = { fg = "red", modifiers = ["bold"] }
      "info" = { fg = "cyan", modifiers = ["bold"] }
      "hint" = { fg = "blue", modifiers = ["bold"] }
      "diagnostic.error" = { fg = "red" }
      "diagnostic.warning" = { fg = "yellow" }
      "diagnostic.info" = { fg = "cyan" }
      "diagnostic.hint" = { fg = "blue" }

      # Popups and Menus
      "ui.popup" = { fg = "fg", bg = "bg" }
      "ui.popup.info" = { fg = "cyan", bg = "bg" }
      "ui.menu" = { fg = "fg", bg = "bg" }
      "ui.menu.selected" = { fg = "bg", bg = "fg" }

      # Additional overrides
      "ui.virtual.whitespace" = "grey"
      "ui.virtual.indent-guide" = { fg = "grey", style = "dotted" }

      [palette]
      bg = "#16181a"
      fg = "#ffffff"
      grey = "#7b8496"
      blue = "#5ea1ff"
      green = "#5eff6c"
      cyan = "#5ef1ff"
      red = "#ff6e5e"
      yellow = "#f1ff5e"
      magenta = "#ff5ef1"
      pink = "#ff5ea0"
      orange = "#ffbd5e"
      purple = "#bd5eff"
      bg_alt = "#1e2124"
      bg_highlight = "#3c4048"
    '';

    programs.helix = {
      package = pkgs.helix;
      enable = true;
      languages.language = [
        {
          name = "haskell";
          auto-format = false;
          scope = "source.haskell";
          file-types = ["hs"];
          roots = ["flake.nix" "cabal.project"];
          comment-token = "--";
          language-servers = ["haskell-language-server"];
        }
        {
          name = "nix";
          auto-format = false;
          formatter.command = "nixpkgs-fmt";
          language-servers = ["nil"];
          indent.tab-width = 2;
          indent.unit = " ";
        }
        {
          name = "rust";
          auto-format = false;
          file-types = ["rs"];
          roots = ["Cargo.toml" "Cargo.lock"];
          language-servers = ["rust-analyzer"];
        }
        {
          name = "purescript";
          auto-format = false;
          formatter.command = "purs-tidy";
          language-servers = ["purescript-language-server"];
          roots = ["spago.yaml" "spago.dhall"];
          file-types = ["purs"];
          indent.tab-width = 2;
          indent.unit = " ";
        }
      ];
      settings = {
        theme = "cyberdream";
        editor = {
          line-number = "relative";
          mouse = false;
          cursorline = true;
          cursorcolumn = false;
          gutters = ["diagnostics" "spacer" "line-numbers" "spacer" "diff"];
          auto-completion = true;
          auto-format = true;
          bufferline = "always";
          color-modes = true;
          rulers = [80 100 120];
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
        nordmod = let
          transparent = {
            fg = "white";
          };
        in {
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
