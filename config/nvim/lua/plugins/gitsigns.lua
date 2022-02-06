-------------------------------------------------------------------------------
-- gitSigns
-------------------------------------------------------------------------------

require('gitsigns').setup {
  numhl = true,
}

local map = vim.api.nvim_set_keymap

map('', '<leader>ub', '<cmd>Gitsigns toggle_current_line_blame<cr>', {})
