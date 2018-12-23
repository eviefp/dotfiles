" Leader setup
let mapleader = ' '
let maplocalleader = ','

" Specify a directory for plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'editorconfig/editorconfig-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" Plug 'ervandew/supertab'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'neovimhaskell/haskell-vim'
Plug 'nbouscal/vim-stylish-haskell'
Plug 'godlygeek/tabular'
Plug 'luochen1990/rainbow'

" Colorschemes
" Plug 'morhetz/gruvbox'
Plug 'mhartington/oceanic-next'
" Plug 'tyrannicaltoucan/vim-deep-space'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'lifepillar/vim-solarized8'

" Haskell
Plug 'edkolev/curry.vim'
" Plug 'parsonsmatt/vim2hs'
Plug 'enomsg/vim-haskellConcealPlus'

" Coq
Plug 'the-lambda-church/coquille'
Plug 'let-def/vimbufsync'

" Idris
Plug 'idris-hackers/idris-vim'
Plug 'vim-syntastic/syntastic'

" Agda
Plug 'derekelkins/agda-vim'

" Testing
" Plug 'https://github.com/junegunn/vim-github-dashboard.git'


" Initialize plugin system
call plug#end()

" Plugin setup

" rainbow
let g:rainbow_active=1

" Not sure
execute "set t_8f=\e[38;2;%lu;%lu;%lum"

let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

" au TermOpen * setlocal nonumber norelativenumber
set number
set colorcolumn=81

set tabstop=4
set shiftwidth=0
set expandtab
set autoindent

filetype plugin indent on
syntax on

" fzf
set splitbelow splitright

" Theme and stuff?
set background=dark
set termguicolors
colorscheme OceanicNext
let g:airline_theme = 'oceanicnext'
let g:palenight_terminal_italics=1

" highlight Conceal ctermbg=NONE guibg=NONE
map <Leader>cl :set background=light<cr>:colorscheme solarized8<cr>let g:airline_theme = 'base16'<cr>
map <Leader>cd :set background=dark<cr>:colorscheme gruvbox<cr>let g:airline_theme = 'gruvbox'<cr>

" Haskell
let g:haskell_conceal = 0
let g:haskell_conceal_wide = 0
let g:haskell_coneal_enumerations = 0
let g:haskell_hsp = 0

let g:idris_conceal = 1

let hscoptions="bfl↱tT"

" Noop the arrow keys in normal mode
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>
noremap <PageUp> <nop>
noremap <PageDown> <nop>

" Better search
set incsearch
set hlsearch
set smartcase

au InsertEnter * set nocursorline
au InsertLeave * set cursorline

set cursorline
set cursorcolumn

set scrolloff=5

" Disable folding
set nofoldenable

set ruler

" Special character inserts
imap <buffer> \forall ∀
imap <buffer> \to →
imap <buffer> \lambda λ
imap <buffer> \Sigma Σ
imap <buffer> \exists ∃
imap <buffer> \equiv ≡


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" fzf - find - f
map <Leader><Space> :Files<CR>
map <Leader>fg      :GFiles?<CR>
map <Leader>fb      :Buffers<CR>
map <Leader>ft      :tag <C-r><C-w><CR>
map <Leader>fc      :Commits<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" window - w
map <Leader>ws :vsplit<CR>
map <Leader>wh :split<CR>

map <Leader>ww <C-w><C-w>

map <Leader>wl <C-w><C-l>
map <Leader>wh <C-w><C-h>
map <Leader>wj <C-w><C-j>
map <Leader>wk <C-w><C-k>

map <Leader>wr <C-w>r

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" buffer - b
map <Leader>bd :bd<Cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" git stuff - g
map <Leader>gg :Gstatus<CR>
map <Leader>gd :Gvdiff<CR>
map <Leader>gsd :Gdivv<CR>
map <Leader>dh :diffget //2<CR>:diffupdate<CR>
map <Leader>dl :diffget //3<CR>:diffupdate<CR>
map <Leader>gw :Gwrite<CR>
map <Leader>du :diffupdate<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Theme - t
map <Leader>tt :Tags<CR>
map <Leader>tc :Colors<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Other - r
nnoremap <Leader>fed :e ~/.config/nvim/init.vim<CR>
nnoremap <Leader>fer :so ~/.config/nvim/init.vim<CR>
map <Leader>fs :Rg <C-r><C-w><CR>
