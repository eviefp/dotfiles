-------------------------------------------------------------------------------
-- rainbow
-------------------------------------------------------------------------------

vim.g.rainbow_active = 1

local map = vim.api.nvim_set_keymap

map('', '<leader>rr', ':RainbowToggle', {})
