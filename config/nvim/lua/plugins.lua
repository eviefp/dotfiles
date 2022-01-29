-------------------------------------------------------------------------------
-- settings
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- theme
vim.cmd 'colorscheme melange'

-- transparent background
vim.cmd 'hi Normal guibg=NONE ctermbg=NONE'

-------------------------------------------------------------------------------
-- devicons
local devicons = require 'nvim-web-devicons'

devicons.setup {
  override = {},
  default = true,
}

-------------------------------------------------------------------------------
-- which-key.nvim
vim.o.timeoutlen = 500
local wk = require 'which-key'

wk.setup({
  plugins = {
    marks = false, -- shows a list of your marks on ' and `
    registers = false, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
    spelling = {
      enabled = false, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
      suggestions = 20, -- how many suggestions should be shown in the list?
    },
    -- the presets plugin, adds help for a bunch of default keybindings in Neovim
    -- No actual key bindings are created
    presets = {
      operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
      motions = true, -- adds help for motions
      text_objects = true, -- help for text objects triggered after entering an operator
      windows = true, -- default bindings on <c-w>
      nav = true, -- misc bindings to work with windows
      z = true, -- bindings for folds, spelling and others prefixed with z
      g = true, -- bindings for prefixed with g
    },
  },
  window = {
    border = "none", -- none, single, double, shadow
    position = "bottom", -- bottom, top
    margin = { 0, 0, 0, 0 }, -- extra window margin [top, right, bottom, left]
    padding = { 1, 1, 1, 1 }, -- extra window padding [top, right, bottom, left]
    winblend = 0
  },
  layout = {
    height = { min = 4, max = 25 }, -- min and max height of the columns
    width = { min = 20, max = 50 }, -- min and max width of the columns
    spacing = 3, -- spacing between columns
    align = "left", -- align columns left, center or right
  },
})

-- TODO: revisit after changing theme
vim.cmd [[
highlight WhichKeyFloat ctermbg=NONE
]]

-- wk.register({}, {})

-------------------------------------------------------------------------------
-- telescope
local telescope = require('telescope')

telescope.setup {
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = 'smart_case',
    }
  }
}

telescope.load_extension('fzf')

-------------------------------------------------------------------------------
-- airline
vim.g['airline#extensions#tabline#enabled'] = 1
vim.g['airline#extensions#tabline#formatter'] = 'default'
vim.g.airline_theme = 'bubblegum'


-------------------------------------------------------------------------------
-- rainbow parentheses
vim.g.rainbow_active = 1

-------------------------------------------------------------------------------
-- better-whitespace

vim.g.better_whitespace_enabled = 1
vim.g.strip_whitespace_on_save = 1

-------------------------------------------------------------------------------
-- haskell
vim.wo.concealcursor = 'nciv'
vim.g.haskell_conceal = 0
vim.g.haskell_conceal_wide = 0
vim.g.haskell_conceal_enumerations = 0
vim.g.haskell_hsp = 0
vim.g.haskell_indent_if = 4
vim.g.haskell_indent_case = 4
vim.g.haskell_indent_let = 4
vim.g.haskell_indent_where = 2
vim.g.haskell_indent_before_where = 2
vim.g.haskell_indent_after_bare_where = 2
vim.g.haskell_indent_do = 4
vim.g.haskell_indent_in = 2
vim.g.haskell_disable = 1

-------------------------------------------------------------------------------
-- markdown
vim.g.markdown_fenced_languages = { 'html', 'python', 'bash=sh', 'nix', 'haskell', 'purescript' }
vim.g.markdown_minlines = 256

-------------------------------------------------------------------------------
-- lexical
vim.cmd [[
augroup lexical
  autocmd!
  autocmd FileType markdown,mkd call lexical#init()
  autocmd FileType textile call lexical#init()
  autocmd FileType tex call lexical#init()
  autocmd FileType text call lexical#init()
augroup END
]]


-------------------------------------------------------------------------------
-- gitSigns
require('gitsigns').setup()

-------------------------------------------------------------------------------
-- treesitter
require'nvim-treesitter.configs'.setup {
  -- One of "all", "maintained" (parsers with maintainers), or a list of languages
  ensure_installed = "all",

  -- Install languages synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- List of parsers to ignore installing
  ignore_install = {},

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- list of language that will be disabled
    disable = {
      -- TOOD: remove when this gets fixed https://github.com/cstrahan/tree-sitter-nix/issues/23
      'nix',
      'haskell',
    },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
  indent = {
    enable = true,
  },
}

-------------------------------------------------------------------------------
-- lspconfig
local lspconfig = require 'lspconfig'

local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
end

-- Add additional capabilities supported by nvim-cmp
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

lspconfig.hls.setup {
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = capabilities,
}
lspconfig.rnix.setup {
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = capabilities,
}
lspconfig.texlab.setup {
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = capabilities,
}

-- nvim-cmp setup
local cmp = require 'cmp'
cmp.setup {
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end,
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'vsnip' },
  },
}
