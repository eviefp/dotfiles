{pkgs, ...}: {
  config = {
    home.packages = [
      pkgs.nodejs
    ];
    programs.nixvim = {
      plugins = {
        treesitter = {
          enable = true;
          nixGrammars = true;
          grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
            agda
            asm
            awk
            bash
            bibtex
            c
            c_sharp
            cmake
            commonlisp
            cpp
            css
            csv
            dhall
            diff
            dockerfile
            dot
            editorconfig
            ebnf
            elixir
            erlang
            fennel
            fish
            fsharp
            gdscript
            gdshader
            git_config
            git_rebase
            gitattributes
            gitcommit
            gitignore
            glsl
            go
            godot_resource
            gomod
            gpg
            haskell
            haskell_persistent
            html
            ini
            javascript
            jq
            json
            just
            latex
            lua
            make
            markdown
            markdown_inline
            mermaid
            muttrc
            nginx
            nim
            nix
            ocaml
            ocaml_interface
            perl
            purescript
            python
            racket
            regex
            rst
            rust
            scheme
            scss
            sql
            ssh_config
            strace
            toml
            tsx
            typescript
            vim
            vimdoc
            xcompose
            xml
            yaml
            yuck
            zathurarc
            zig
          ];

          settings = {
            highlight = {
              enable = true;
              additional_vim_regex_highlighting = false;
            };

            incremental_selection = {
              enable = true;
              keymaps = {
                init_selection = "gsa";
                node_incremental = "gsl";
                node_decremental = "gsL";
                scope_incremental = "gsk";
              };
            };

            indent.enable = true;
          };
        };

        treesitter-context = {
          enable = true;
          settings = {
            max_lines = 12;
            trim_scope = "outer";
            mode = "cursor";
          };
        };

        treesitter-refactor = {
          enable = true;

          highlightDefinitions.enable = false;
          highlightCurrentScope.enable = false;

          smartRename = {
            enable = true;
            keymaps.smartRename = "grr";
          };

          navigation = {
            enable = false;
            keymaps = {
              gotoDefinitionLspFallback = "gd";
              listDefinitions = "grd";
              listDefinitionsToc = "gO";
              gotoNextUsage = "grj";
              gotoPreviousUsage = "grk";
            };
          };
        };

        # TODO: look into this
        treesitter-textobjects = {
          enable = true;

          select = {
            enable = false;
          };

          swap = {
            enable = true;
            swapNext = {
              "]wf" = "@function.outer";
              "]wp" = "@parameter.inner";
            };
            swapPrevious = {
              "[wf" = "@function.outer";
              "[wp" = "@parameter.inner";
            };
          };

          move = {
            enable = true;
            gotoNext = {
              "]f" = "@function.outer";
            };
            gotoPrevious = {
              "[f" = "@function.outer";
            };
          };
        };
      };
    };
  };
}
