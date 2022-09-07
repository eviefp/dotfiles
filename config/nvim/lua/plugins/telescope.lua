-------------------------------------------------------------------------------
-- telescope
-------------------------------------------------------------------------------

local telescope = require('telescope')
local actions = require('telescope.actions')

telescope.load_extension('fzf')
telescope.load_extension('file_browser')
telescope.load_extension('ui-select')

local fb_actions = telescope.extensions.file_browser.actions

telescope.setup {
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = 'smart_case',
    },
    file_browser = {
    },
    ['ui-select'] = {
      require('telescope.themes').get_dropdown {
      }
    },
  },
  defaults = {
    mappings = {
      i = {
        ['<C-k>'] = actions.move_selection_previous,
        ['<C-j>'] = actions.move_selection_next,
        ['<C-h>'] = actions.preview_scrolling_up,
        ['<C-l>'] = actions.preview_scrolling_down,
        ['<C-Space>'] = actions.select_vertical,
        ['<C-Tab>'] = actions.select_horizontal,
      },
    },
  },
}


local map = vim.api.nvim_set_keymap
local lsp_doc_symbols = "<cmd>lua require('telescope.builtin').lsp_document_symbols()<cr>"
local current_buf_fzf = "<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<cr>"

map(''  , '<leader>gr'       , '<cmd>Telescope git_branches<cr>'          , {})
map(''  , '<leader>gc'       , '<cmd>Telescope git_bcommits<cr>'          , {})
map(''  , '<leader>gC'       , '<cmd>Telescope git_commits<cr>'           , {})

map('n' , '<leader>xd'       , '<cmd>Telescope diagnostics<cr>'           , { noremap = true})
map('n' , 'gd'               , '<cmd>Telescope lsp_definitions<cr>'       , { noremap = true   , silent = true })
map('n' , 'gi'               , '<cmd>Telescope lsp_implementations<cr>'   , { noremap = true   , silent = true })
map('n' , 'gt'               , '<cmd>Telescope lsp_type_definitions<cr>'  , { noremap = true   , silent = true })
map('n' , 'gr'               , '<cmd>Telescope lsp_references<cr>'        , { noremap = true   , silent = true })

map('n' , '<leader><leader>' , '<cmd>Telescope git_files<cr>'             , { noremap = true})
map('n' , '<leader>ff'       , '<cmd>Telescope live_grep<cr>'             , { noremap = true})
map('n' , '<leader>ft'       , '<cmd>Telescope file_browser<cr>'          , { noremap = true})
map('n' , '<leader>fF'       , '<cmd>Telescope find_files<cr>'            , { noremap = true})
map('n' , '<leader>fz'       , "<cmd>Telescope grep_string<cr>"           , { noremap = true})
map('n' , '<leader>fs'       , current_buf_fzf                            , { noremap = true})

map('n' , '<leader>fb'       , '<cmd>Telescope buffers<cr>'               , { noremap = true})
map('n' , '<leader>fm'       , '<cmd>Telescope commands<cr>'              , { noremap = true})
map('n' , '<leader>fk'       , '<cmd>Telescope marks<cr>'                 , { noremap = true})
map('n' , '<leader>fq'       , '<cmd>Telescope quickfix<cr>'              , { noremap = true})
map('n' , '<leader>fl'       , '<cmd>Telescope loclist<cr>'               , { noremap = true})
map('n' , '<leader>fr'       , '<cmd>Telescope registers<cr>'             , { noremap = true})
map('n' , '<leader>f?'       , '<cmd>Telescope keymaps<cr>'               , { noremap = true})

map('n' , '<leader>fa'       , '<cmd>Telescope lsp_document_symbols<cr>'  , { noremap = true})
map('n' , '<leader>fA'       , '<cmd>Telescope lsp_workspace_symbols<cr>' , { noremap = true})
map('n' , '<leader>fe'       , '<cmd>Telescope diagnostics<cr>'           , { noremap = true})

map('n' , '<leader>fB'       , '<cmd>Telescope builtin<cr>'               , { noremap = true})
