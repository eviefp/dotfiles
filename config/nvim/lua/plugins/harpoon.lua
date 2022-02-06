-------------------------------------------------------------------------------
-- harpoon
------------------------------------------------------------------------------

require('telescope').load_extension('harpoon')

local harpoon = require 'harpoon'


harpoon.setup {
  -- sets the marks upon calling `toggle` on the ui, instead of require `:w`.
  save_on_toggle = false,

  -- saves the harpoon file upon every change. disabling is unrecommended.
  save_on_change = true,

  -- sets harpoon to run the command immediately as it's passed to the terminal when calling `sendCommand`.
  enter_on_sendcmd = false,

  -- closes any tmux windows harpoon that harpoon creates when you close Neovim.
  tmux_autoclose_windows = false,

  -- filetypes that you want to prevent from adding to the harpoon list menu.
  excluded_filetypes = { "harpoon" }
}


local map = vim.api.nvim_set_keymap

map('', '<leader>ha', "<cmd>lua require('harpoon.mark').add_file()<cr>", {})
map('', '<leader>hm', "<cmd>lua require('harpoon.ui').toggle_quick_menu()<cr>", {})
map('', '<leader>hj', "<cmd>lua require('harpoon.ui').nav_next()<cr>", {})
map('', '<leader>hk', "<cmd>lua require('harpoon.ui').nav_prev()<cr>", {})
map('', '<leader>fh', "<cmd>Telescope harpoon marks<cr>", {})

