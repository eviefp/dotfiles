-------------------------------------------------------------------------------
-- hop
------------------------------------------------------------------------------

local hop = require 'hop'

hop.setup {
  keys = 'etovxqpdygfblzhckisuran',
  jump_on_sole_occurrence = false,
}

local map = vim.api.nvim_set_keymap

map('n', 'f', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>", {})
map('n', 'F', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>", {})
map('o', 'f', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true, inclusive_jump = true })<cr>", {})
map('o', 'F', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true, inclusive_jump = true })<cr>", {})
map('', 't', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>", {})
map('', 'T', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>", {})

map('n', '<leader>hw', '<cmd>HopWord<cr>', {})
map('n', '<leader>h/', '<cmd>HopPattern<cr>', {})
map('n', '<leader>h2', '<cmd>HopChar2<cr>', {})
map('n', '<leader>hl', '<cmd>HopLine<cr>', {})
