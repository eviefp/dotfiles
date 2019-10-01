" Leader setup
let mapleader = ' '
let maplocalleader = ','

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" This uses the `vim-plug` manager:
" https://github.com/junegunn/vim-plug
"
" Note: the autoload directory for nvim seems to be
" ~/.local/share/nvim/site/autoload

call plug#begin('~/.local/share/nvim/plugged')

""" General vim stuff

" Support for '.editorconfig' files
Plug 'editorconfig/editorconfig-vim'

" Status bar & themes.
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" FuZzy Find, need to install fzf first
Plug 'junegunn/fzf', {
    \ 'dir': '~/.fzf',
    \ 'do': './install --all'
    \ }
Plug 'junegunn/fzf.vim'

""" Generic coding helpers

" gc / gcc for commenting stuff
Plug 'tpope/vim-commentary'

" Select and Tabularize /= for aligning around equals sign
Plug 'godlygeek/tabular'

" Surround is awesome.
Plug 'tpope/vim-surround'

" Rainbow parentheses
Plug 'luochen1990/rainbow'

" Indentation guides
Plug 'nathanaelkane/vim-indent-guides'

""" Git

" Basic git plugin, g? for help in gstatus
Plug 'tpope/vim-fugitive'
" Additional plugin for github (pretty much for :Gbrowse I think)
Plug 'tpope/vim-rhubarb'
" Git gutter, must read more 
Plug 'mhinz/vim-signify'

" Try out: https://github.com/jreybert/vimagit

" Multiple cursor edit. Select word, use <C-n> then c or something.
Plug 'terryma/vim-multiple-cursors'

""" Language-Server Client
Plug 'neoclide/coc.nvim', {'branch': 'release'}

""" Markdown
Plug 'tpope/vim-markdown'

""" Haskell
" Indentation and syntax highlighting
Plug 'neovimhaskell/haskell-vim'
Plug 'nbouscal/vim-stylish-haskell'


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

""" Colorschemes
" Plug 'morhetz/gruvbox'
Plug 'mhartington/oceanic-next'
" Plug 'tyrannicaltoucan/vim-deep-space'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'lifepillar/vim-solarized8'

" Initialize plugin system
call plug#end()

" Plugin setup

" rainbow
let g:rainbow_active=1

" indent guides
let g:indent_guides_enable_on_vim_startup = 1

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
let g:airline#extensions#tabline#enabled = 1

" signify
let g:signify_vcs_list = ['git']

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

" No-op the arrow keys in normal mode
" noremap <Up> <nop>
" noremap <Down> <nop>
" noremap <Left> <nop>
" noremap <Right> <nop>
" noremap <PageUp> <nop>
" noremap <PageDown> <nop>

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

" LSP
let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']
let g:LanguageClient_serverCommands = {
    \ 'haskell': ['ghcide', '--lsp'],
    \ }

tnoremap <Esc> <C-\><C-n>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" fzf - find - f
map <Leader><Space> :Files<CR>
map <Leader>fg      :GFiles?<CR>
map <Leader>fb      :Buffers<CR>
map <Leader>bb      :Buffers<CR>
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
" term - t
map <Leader>tt :vsplit<CR>:term<CR>
map <Leader>th :split<CR>:term<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" buffer - b
map <Leader>bd :bd<Cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" git stuff - g
map <Leader>gg :Gstatus<CR>
map <Leader>gs :Gstatus<CR>
map <Leader>gd :Gvdiff<CR>
map <Leader>gsd :Gdivv<CR>
map <Leader>dh :diffget //2<CR>:diffupdate<CR>
map <Leader>dl :diffget //3<CR>:diffupdate<CR>
map <Leader>gw :Gwrite<CR>
map <Leader>du :diffupdate<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Theme - t
" map <Leader>tt :Tags<CR>
map <Leader>tc :Colors<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Other - r
nnoremap <Leader>fed :e ~/.config/nvim/init.vim<CR>
nnoremap <Leader>fer :so ~/.config/nvim/init.vim<CR>
map <Leader>fs :Rg<CR>


""" Testing stuff

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)



" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold ?? this doesnt seem to work
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word -- doesn't seem to work yet
nmap <leader>rn <Plug>(coc-rename)

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

