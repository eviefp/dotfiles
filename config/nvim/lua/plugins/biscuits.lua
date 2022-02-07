-------------------------------------------------------------------------------
-- biscuits
------------------------------------------------------------------------------

local biscuits = require 'nvim-biscuits'

biscuits.setup {
  default_config = {
    max_length = 32,
    min_distance = 3,
    prefix_string = " 🍪 ",
  },
  language_config = {
    haskell = {
      disabled = true, -- does not quite work well
    },
  },
  toggle_keybind = '<leader>bb',
  show_on_start = false,
};
