-------------------------------------------------------------------------------
-- nvim-cmp
-------------------------------------------------------------------------------

vim.o.completeopt = 'menuone,noselect'

local cmp = require 'cmp'
local lspkind = require 'lspkind'

cmp.setup {
  completion = {
    autocomplete = true,
  },
  formatting = {
    format = lspkind.cmp_format {
      with_text = true,
      menu = {
        buffer        = '(Buffer)',
        nvim_lsp      = '(LSP)',
        path          = '(Path)',
        vsnip         = '(Snippet)',
        emoji         = '(Emoji)',
        latex_symbols = '(Latex)',
        nvim_lua = '(NvimLua)',
      },
      madwidth = 100,
      -- This can be used for awesome customisation, see https://github.com/onsails/lspkind-nvim/pull/30
      before = function(entry, vim_item)
        return vim_item
      end,
    },
  },
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
      -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },
  mapping = {
    ['<C-k>'] = cmp.mapping(cmp.mapping.select_prev_item() , { 'i' , 'c' }) ,
    ['<C-j>'] = cmp.mapping(cmp.mapping.select_next_item() , { 'i' , 'c' }) ,
    ['<C-h>'] = cmp.mapping(cmp.mapping.scroll_docs(-4)    , { 'i' , 'c' }) ,
    ['<C-l>'] = cmp.mapping(cmp.mapping.scroll_docs(4)     , { 'i' , 'c' }) ,
    ['<C-Space>'] = cmp.mapping(cmp.mapping.complete()     , { 'i' , 'c' }) ,
    ['<C-g>'] = cmp.mapping(cmp.mapping.close()            , { 'i' , 'c' }) ,
    ['<CR>'] = cmp.mapping.confirm {
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
    { name = 'path' },
    { name = 'vsnip' },
    { name = 'nvim_lua' },
    { name = 'emoji' },
    { name = 'latex_symbols' },
    { name = 'buffer' },
  },
}

-- set autocomplete to false and enable this for super mega autocomplete
-- _G.vimrc = _G.vimrc or {}

-- _G.vimrc.cmp = _G.vimrc.cmp or {}

-- _G.vimrc.cmp.on_text_changed = function ()
--   local cursor = vim.api.nvim_win_get_cursor(0)
--   local line = vim.api.nvim_get_current_line()
--   local before = string.sub(line, 1, cursor[2] + 1)
--   if before:match('%s*$') then
--     cmp.complete() -- Trigger completion only if the cursor is placed at the end of line.
--   end
-- end

-- vim.cmd([[
--   augroup vimrc
--     autocmd TextChanged,TextChangedI,TextChangedP * call luaeval('vimrc.cmp.on_text_changed()')
--   augroup END
-- ]])
