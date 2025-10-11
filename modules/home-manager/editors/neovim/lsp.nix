{
  lib,
  config,
  ...
}: let
  cfg = config.evie.editors.neovim;
in {
  config = lib.mkIf cfg.enable {
    programs.nixvim = {
      keymaps = [
        {
          key = "gD";
          action = ":split<cr>:lua vim.lsp.buf.definition()<cr>";
          options.desc = "lsp definition: horizontal split";
        }
        {
          key = "]d";
          action = ":lua vim.diagnostic.goto_next()<cr>";
          options.desc = "lsp next diagnostic";
        }
        {
          key = "]d";
          action = ":lua vim.diagnostic.goto_prev()<cr>";
          options.desc = "lsp prev diagnostic";
        }
      ];
      lsp = {
        keymaps = [
          {
            key = "gd";
            lspBufAction = "definition";
          }
          {
            key = "gj";
            lspBufAction = "references";
          }
          {
            key = "gt";
            lspBufAction = "type_definition";
          }
          {
            key = "gi";
            lspBufAction = "implementation";
          }
          {
            key = "K";
            lspBufAction = "hover";
          }
        ];
      };
    };
  };
}
