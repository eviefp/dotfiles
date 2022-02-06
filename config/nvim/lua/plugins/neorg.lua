-------------------------------------------------------------------------------
-- neorg
------------------------------------------------------------------------------

local parser_configs = require('nvim-treesitter.parsers').get_parser_configs()

-- These two are optional and provide syntax highlighting
-- for Neorg tables and the @document.meta tag
parser_configs.norg_meta = {
    install_info = {
        url = "https://github.com/nvim-neorg/tree-sitter-norg-meta",
        files = { "src/parser.c" },
        branch = "main"
    },
}

parser_configs.norg_table = {
    install_info = {
        url = "https://github.com/nvim-neorg/tree-sitter-norg-table",
        files = { "src/parser.c" },
        branch = "main"
    },
}

local neorg = require 'neorg'

neorg.setup {
  load = {
    ["core.defaults"] = {},
    ["core.norg.dirman"] = {
      config = {
        workspaces = {
          work = "~/Documents/wiki/work",
          home = "~/Documents/wiki/home",
        },
      },
    },
    ["core.norg.concealer"] = {
      config = {
        icon_preset = 'varied',
        markup_preset = 'brave',
        dim_code_blocks = true,
      },
    },
    ["core.norg.qol.toc"] = {
      config = {
      },
    },
    ["core.presenter"] = {
      config = {
        zen_mode = 'truezen',
      },
    },
    ["core.norg.completion"] = {
      config = {
        engine = 'nvim-cmp',
      },
    },
  },
}
