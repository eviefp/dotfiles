-------------------------------------------------------------------------------
-- theme
-------------------------------------------------------------------------------

require 'plugins.themes.catppuccin'
require 'plugins.themes.tokyonight'
require 'plugins.themes.material'

-- vim.cmd 'colorscheme catppuccin'
vim.cmd 'colorscheme tokyonight'
-- vim.cmd 'colorscheme material'

-- transparent background
vim.cmd [[
hi Normal guibg=NONE ctermbg=NONE
hi LineNr guibg=NONE ctermbg=NONE
hi SignColumn guibg=NONE ctermbg=NONE
hi EndOfBuffer guibg=NONE ctermbg=NONE
]]
