-------------------------------------------------------------------------------
-- config
-------------------------------------------------------------------------------

-- leader
vim.g.mapleader = ' '
vim.g.maplocalreader = ','

-- enable syntax highlighting
vim.cmd 'syntax on'

-- do not highlight searches
vim.o.hlsearch = false


-- dark background
vim.o.background = 'dark'

-- enable 24bit rgb in the terminal
vim.o.termguicolors = true

-- enable figuring out file types automatically, and loading plugin/indent
-- information/related scripts for them
vim.cmd 'filetype plugin indent on'

-- the default value is '50', which is a bit low
vim.cmd 'syntax sync minlines=256'

-- file name in terminal title
vim.o.title = true

-- auto reload files that have been changed
vim.o.autoread = true

-- show line numbers
vim.wo.number = true

-- show relative line numbers
vim.wo.relativenumber = true

-- no backup files
vim.o.backup = false

-- no swap files
vim.o.swapfile = false

-- no bells
vim.o.visualbell = false
vim.o.errorbells = false

-- shorten info messages; 'c' is important for coc-nvim
-- vim.o.shortmess = 'aotIcF'

-- copy indent previous line to next line
vim.o.autoindent = true
-- tab = 2 chars long
vim.o.tabstop = 2
-- number of spaces to use for autoindent
vim.o.shiftwidth = 2
-- indent by multiple of shift width
vim.o.shiftround = true
-- <BS> removes tabs (even if tab = spaces)
vim.o.softtabstop = 2
-- change tabs to spaces
vim.o.expandtab = true
-- change tabs to spaces
vim.bo.expandtab = true

-- allow results to contain upper case characters
vim.o.smartcase = true
-- the above doesn't work unless this is on
vim.o.ignorecase = true

-- unfold everything by default
vim.o.foldlevel = 99

-- show substitutions on the fly, in the same window
vim.o.inccommand = "nosplit"

-- open new vertical windows below
vim.o.splitbelow = true

-- open new horizontal windows right
vim.o.splitright = true

-- use system clipboard
vim.o.clipboard = 'unnamedplus'

-- don't wrap lines
vim.wo.wrap = true

-- line width, spelling for text files
vim.cmd [[
autocmd FileType md setlocal textwidth=80 spell
autocmd FileType txt setlocal textwidth=80 spell
autocmd FileType tex setlocal textwidth=80 spell
]]

vim.o.textwidth = 80

-- highlight yank
vim.cmd [[
au TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=150, on_visual=false}
]]



