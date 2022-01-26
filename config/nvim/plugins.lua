-------------------------------------------------------------------------------
-- settings
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- airline
vim.g['airline#extensions#tabline#enabled'] = 1
vim.g['airline#extensions#tabline#formatter'] = 'default'
vim.g.airline_theme = 'badwolf'


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
-- coc.nvim
vim.o.updatetime = 300
vim.o.signcolumn = 'number'

vim.cmd [[
autocmd CursorHold * silent call CocActionAsync('highlight')
]]

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
    disable = {},

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = {},
  },
}
