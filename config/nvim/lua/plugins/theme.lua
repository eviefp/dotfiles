-------------------------------------------------------------------------------
-- theme
-------------------------------------------------------------------------------

require 'plugins.themes.neon'
-- require 'plugins.themes.witch'
-- require 'plugins.themes.tokyonight'
-- require 'plugins.themes.material'

-- vim.cmd 'colorscheme catppuccin'
-- vim.cmd 'colorscheme tokyonight'
-- vim.cmd 'colorscheme material'
vim.cmd 'colorscheme neon'

-- transparent background
vim.cmd [[
hi Normal guibg=NONE ctermbg=NONE
hi LineNr guibg=NONE ctermbg=NONE
hi SignColumn guibg=NONE ctermbg=NONE
hi EndOfBuffer guibg=NONE ctermbg=NONE
hi lualine_c_normal guibg=NONE ctermbg=NONE
hi lualine_transitional_lualine_a_buffers_active_to_lualine_c_normal guibg=NONE ctermbg=NONE
]]
