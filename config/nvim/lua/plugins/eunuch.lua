-------------------------------------------------------------------------------
-- eunuch
-------------------------------------------------------------------------------

local map = vim.api.nvim_set_keymap

map('n', '<leader>ed', '<cmd>Delete<cr>', {})
map('n', '<leader>eu', '<cmd>Unlink<cr>', {})
map('n', '<leader>em', ':Rename ', {})
