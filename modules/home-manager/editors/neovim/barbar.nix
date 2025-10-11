{...}: {
  config.programs.nixvim = {
    keymaps = [
      {
        key = "gn";
        action = "<cmd>BufferNext<cr>";
        options.desc = "buffer: goto next";
      }
      {
        key = "gp";
        action = "<cmd>BufferPrevious<cr>";
        options.desc = "buffer: goto prev";
      }
      {
        key = "gN";
        action = "<cmd>BufferMoveNext<cr>";
        options.desc = "buffer: move next";
      }
      {
        key = "gP";
        action = "<cmd>BufferMovePrevious<cr>";
        options.desc = "buffer: move ";
      }
      {
        key = "<leader>bc";
        action = "<cmd>BufferClose<cr>";
        options.desc = "buffer: close";
      }
      {
        key = "<leader>bC";
        action = "<cmd>BufferCloseAllButCurrent<cr>";
        options.desc = "buffer: close all but current";
      }
      {
        key = "<leader>bp";
        action = "<cmd>BufferPin<cr>";
        options.desc = "buffer: pin";
      }
    ];

    plugins.barbar = {
      enable = true;
      settings = {
        highlight_visible = true;
      };
    };
  };
}
