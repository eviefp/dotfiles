-------------------------------------------------------------------------------
-- neorg
------------------------------------------------------------------------------

local parser_configs = require('nvim-treesitter.parsers').get_parser_configs()

-- These two are optional and provide syntax highlighting
-- for Neorg tables and the @document.meta tag
parser_configs.norg_meta = {
    install_info = {
        url = 'https://github.com/nvim-neorg/tree-sitter-norg-meta',
        files = { 'src/parser.c' },
        branch = 'main'
    },
}

parser_configs.norg_table = {
    install_info = {
        url = 'https://github.com/nvim-neorg/tree-sitter-norg-table',
        files = { 'src/parser.c' },
        branch = 'main'
    },
}

local neorg = require 'neorg'

neorg.setup {
  load = {
    ['core.defaults'] = {},
    ['core.keybinds'] = {
      config = {
        default_keybinds = false,
        hook = function(keybinds)
          -- Generic norg
          keybinds.map_event_to_mode('norg', {
            n = {
              { '<localleader>n'       , 'core.norg.dirman.new.note' },
              { '<cr>'                 , 'core.norg.esupports.hop.hop-link' },
              { '<c-cr>'               , 'core.norg.esupports.hop.hop-link', 'vsplit' },
              { '<localleader>f'       , 'core.integrations.telescope.find_linkable' },
              { '<localleader><space>' , 'core.norg.qol.todo_items.todo.task_cycle' },
              { 'gtu'                  , 'core.norg.qol.todo_items.todo.task_undone' },
              { 'gtp'                  , 'core.norg.qol.todo_items.todo.task_pending' },
              { 'gtd'                  , 'core.norg.qol.todo_items.todo.task_done' },
              { 'gth'                  , 'core.norg.qol.todo_items.todo.task_on_hold' },
              { 'gtc'                  , 'core.norg.qol.todo_items.todo.task_cancelled' },
              { 'gtr'                  , 'core.norg.qol.todo_items.todo.task_recurring' },
              { 'gti'                  , 'core.norg.qol.todo_items.todo.task_important' },

              { '<localleader>tc'      , 'core.gtd.base.capture' },
              { '<localleader>tv'      , 'core.gtd.base.views' },
              { '<localleader>te'      , 'core.gtd.base.edit' },
              { '<localleader>fl'      , 'core.integrations.telescope.find_linkable' },
              { '<leader>fl'           , 'core.integrations.telescope.find_linkable' },
            },
            i = {
              { '<c-l>', 'core.integrations.telescope.insert_link' },
            },
          }, {
            silent = true,
            noremap = true,
          })
          -- Map the below keys on gtd displays
          keybinds.map_event_to_mode("gtd-displays", {
            n = {
              { "<CR>", "core.gtd.ui.goto_task" },

              -- Keys for closing the current display
              { "q", "core.gtd.ui.close" },
              { "<Esc>", "core.gtd.ui.close" },

              { "e", "core.gtd.ui.edit_task" },
              { "<Tab>", "core.gtd.ui.details" },
            },
          }, {
              silent = true,
              noremap = true,
              nowait = true,
          })
          -- TOC related
          keybinds.map_event_to_mode('toc-split', {
              n = {
                  { '<cr>', 'core.norg.qol.toc.hop-toc-link' },

                  -- Keys for closing the current display
                  { 'q', 'core.norg.qol.toc.close' },
                  { '<esc>', 'core.norg.qol.toc.close' },
              },
          }, {
              silent = true,
              noremap = true,
              nowait = true,
          })

          -- Map the below keys on presenter mode
          keybinds.map_event_to_mode('presenter', {
              n = {
                  { '<cr>', 'core.presenter.next_page' },
                  { 'l', 'core.presenter.next_page' },
                  { 'h', 'core.presenter.previous_page' },

                  -- Keys for closing the current display
                  { 'q', 'core.presenter.close' },
                  { '<esc>', 'core.presenter.close' },
              },
          }, {
              silent = true,
              noremap = true,
              nowait = true,
          })

          -- Apply the below keys to all modes
          keybinds.map_to_mode('all', {
              n = {
                  { '<localleader>ts', ':Neorg toc split<cr>' },
                  { '<leader>mn', ':Neorg mode norg<cr>' },
                  { '<leader>mh', ':Neorg mode traverse-heading<cr>' },
              },
          }, {
              silent = true,
              noremap = true,
          })
        end,
      },
    },
    ['core.norg.dirman'] = {
      config = {
        workspaces = {
          home = '~/code/wiki',
        },
      },
    },
    ['core.norg.concealer'] = {
      config = {
        icon_preset = 'varied',
        markup_preset = 'brave',
        dim_code_blocks = true,
      },
    },
    ['core.norg.qol.toc'] = {
      config = {
      },
    },
    ['core.presenter'] = {
      config = {
        zen_mode = 'truezen',
      },
    },
    ['core.norg.completion'] = {
      config = {
        engine = 'nvim-cmp',
      },
    },
    ['core.gtd.base'] = {
      config = {
        workspace = 'home',
        default_lists = {
          inbox = 'todo.norg',
        },
      },
    },
    ['core.integrations.telescope'] = {
      config = {
        workspace = 'home',
        default_lists = {
          inbox = 'todo.norg',
        },
      },
    },
  },
}

