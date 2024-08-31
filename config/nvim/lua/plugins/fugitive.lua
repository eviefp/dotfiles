-------------------------------------------------------------------------------
-- fugitive
-------------------------------------------------------------------------------

local map = vim.api.nvim_set_keymap

map('' , '<leader>gs'  , '<cmd>Git<cr>'                         , {})
map('' , '<leader>gg'  , '<cmd>GBrowse<cr>'                     , {})

map('' , '<leader>gb'  , '<cmd>Git blame<cr>'                   , {})
map('' , '<leader>gl'  , '<cmd>Git log<cr>'                     , {})

map('' , '<leader>gf'  , '<cmd>Git fetch<cr>'                   , {})
map('' , '<leader>gp'  , '<cmd>Git push<cr>'                    , {})
map('' , '<leader>gP'  , '<cmd>Git push --force-with-lease<cr>' , {})
map('' , '<leader>gu'  , '<cmd>Git pull<cr>'                    , {})

map('' , '<leader>gds' , '<cmd>Gdiffsplit!<cr>'                 , {})
map('' , '<Leader>gdh' , '<cmd>diffget //2<cr>:diffupdate<cr>'  , {})
map('' , '<Leader>gdl' , '<cmd>diffget //3<cr>:diffupdate<cr>'  , {})

vim.cmd [[
let g:nremap = { '=': '<Tab>' }
]]
