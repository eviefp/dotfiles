{...}: {
  config.programs.nixvim = {
    keymaps = [
      {
        key = "]g";
        action = "<cmd>Gitsigns next_hunk<cr>";
        options.desc = "goto next git hunk";
      }
      {
        key = "[g";
        action = "<cmd>Gitsigns prev_hunk<cr>";
        options.desc = "goto prev git hunk";
      }
      {
        key = "<leader>gt";
        action = "<Cmd>Gitsigns toggle_current_line_blame<cr>";
        options.desc = "blame: current line toggle";
      }
    ];
    plugins.gitsigns = {
      enable = true;

      settings = {
        signs_staged_enable = true;
        signcolumn = true;
        numhl = true;
        auto_attach = true;
        attach_to_untracked = false;
        current_line_blame = false;
        current_line_blame_opts = {
          virt_text = true;
          virt_text_pos = "right_align";
          delay = 500;
          ignore_whitespace = false;
        };
      };
    };
  };
}
