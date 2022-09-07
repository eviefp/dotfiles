-------------------------------------------------------------------------------
-- firenvim

local lualine = require 'lualine'

if vim.g.started_by_firenvim then
  vim.cmd 'set laststatus=0 ruler'
  vim.cmd 'set showtabline=0'
end
