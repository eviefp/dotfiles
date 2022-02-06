-------------------------------------------------------------------------------
-- plugins
------------------------------------------------------------------------------

vim.g.mergetool_layout = 'unmodified'
vim.g.mergetool_prefer_revision = 'mr,b'

local map = vim.api.nvim_set_keymap

map('', '<leader>gm', '<cmd>MergetoolToggle<cr>', {})
