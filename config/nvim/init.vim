""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" This uses the `vim-plug` manager:
" https://github.com/junegunn/vim-plug
"
" Note: the autoload directory for nvim seems to be
" ~/.local/share/nvim/site/autoload
call plug#begin('~/.local/share/nvim/plugged')

""" General vim stuff
" Support for '.editorconfig' files
Plug 'editorconfig/editorconfig-vim'
" Nerd tree
Plug 'scrooloose/nerdtree'
" dev icons
Plug 'ryanoasis/vim-devicons'
" Status bar & themes.
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" FuZzy Find, need to install fzf first
Plug 'junegunn/fzf', {
    \ 'dir': '~/.fzf',
    \ 'do': './install --all'
    \ }
Plug 'junegunn/fzf.vim'
" Multiple cursor edit. Select word, use <C-n> then c or something.
Plug 'terryma/vim-multiple-cursors'
" Which-key
Plug 'liuchengxu/vim-which-key'
" Buffer delete
Plug 'moll/vim-bbye'
" Highlight yanked text for a bit
Plug 'machakann/vim-highlightedyank'

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
" %S stuff
Plug 'tpope/vim-abolish'

""" Git
" Basic git plugin, g? for help in gstatus
Plug 'tpope/vim-fugitive'
" Additional plugin for github (pretty much for :Gbrowse I think)
Plug 'tpope/vim-rhubarb'
" Git gutter, must read more
Plug 'airblade/vim-gitgutter'
" Try out: https://github.com/jreybert/vimagit

""" Coding
" Language Server
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Markdown
Plug 'tpope/vim-markdown'

""" Haskell
" Indentation and syntax highlighting
Plug 'vladciobanu/haskell-vim'
Plug 'vladciobanu/vim-stylish-haskell'
" <C-y> to insert function above, [[ and ]] to navigate
Plug 'edkolev/curry.vim'
"Plug 'enomsg/vim-haskellConcealPlus'

""" PureScript
Plug 'purescript-contrib/purescript-vim'
"Plug 'FrigoEU/psc-ide-vim'

""" Rust
"Plug 'rust-lang/rust.vim'
"Plug 'sebastianmarkow/deoplete-rust'
"Plug 'racer-rust/vim-racer'

""" Nix
Plug 'LnL7/vim-nix'

""" Coq
Plug 'the-lambda-church/coquille'
Plug 'let-def/vimbufsync'

""" Idris
Plug 'idris-hackers/idris-vim'
Plug 'vim-syntastic/syntastic'

""" Agda
Plug 'derekelkins/agda-vim'

""" Dhall
Plug 'vmchale/dhall-vim'

""" Colorschemes
Plug 'fatih/molokai'
Plug 'lifepillar/vim-solarized8'

""" Testing
Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'mbbill/undotree'
Plug 'kshenoy/vim-signature'
Plug 'plasticboy/vim-markdown'
Plug 'bronson/vim-trailing-whitespace'
" Plug 'vimwiki/vimwiki'
Plug 'lervag/wiki.vim'
Plug 'lervag/wiki-ft.vim'

Plug 'justinmk/vim-sneak'

Plug 'scalameta/coc-metals'
" Plug 'liuchengxu/vista.vim'

Plug 'thanethomson/vim-jenkinsfile'
" Initialize plugin system
call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Generic vim setup
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vimwiki
" set nocompatible
let g:wiki_root = '~/Documents/wiki'

set autoread
" Forgive me, padre
let g:stylish_haskell_command = "floskell"
let g:stylish_haskell_dont_override = "on"

" Leader setup
let mapleader = ' '
let maplocalleader = ','
" Not sure what these are
execute "set t_8f=\e[38;2;%lu;%lu;%lum"
let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
" Relative line numbers
set number relativenumber
set nu rnu
au TermOpen * setlocal nonumber norelativenumber
" Fill column indicator
set colorcolumn=81
" TODO: Figure out what each of these are.
set tabstop=4
set shiftwidth=0
set expandtab
set autoindent
" These seem like they turn on indentation by plugins and syntax highlighting.
filetype plugin indent on
syntax on
" Better search
set incsearch
set hlsearch
set smartcase
" Not sure what these are
" au InsertEnter * set nocursorline
au InsertLeave * set cursorline
set cursorline
set cursorcolumn
set ruler
" Things like "zb" will keep 3 lines below target
set scrolloff=3
" Disable folding, what does this do?
set nofoldenable
" Basic setup for text files
" autocmd FileType md setlocal textwidth=80 spell
" autocmd FileType txt setlocal textwidth=80 spell
" autocmd FileType tex setlocal textwidth=80 spell
" Show substitutions as you type them
set inccommand=nosplit

""" Agda
au BufNewFile,BufRead *.agda setf agda
let g:agda_extraincpaths = [ "/nix/store/d31lf1zq5mwfi3hw276hpnb45hkd3yvc-agda-stdlib-1.1/share/agda", "/nix/store/qndjjs3nrn25pn9y1wa1sqpyr7bfs62w-Agda-2.6.0.1-data/share/ghc-8.6.5/x86_64-linux-ghc-8.6.5/Agda-2.6.0.1/lib/prim"]

""" Plugin-specific
" for vim-devicons
set encoding=UTF-8
" Shorter update time, added for gitgutter
set updatetime=100
" Enable rainbow parentheses
let g:rainbow_active=1
" Enable indent guides
let g:indent_guides_enable_on_vim_startup = 1
" Added for fzf
set splitbelow splitright

" Theme and stuff
set background=dark
set termguicolors
colorscheme molokai

" Airline
let g:airline_theme = 'molokai'
let g:palenight_terminal_italics=1
let g:airline#extensions#tabline#enabled = 1

" Haskell
set concealcursor=nciv
let g:haskell_conceal = 0
let g:haskell_conceal_wide = 0
let g:haskell_coneal_enumerations = 0
let g:haskell_hsp = 0
" haskell-vim
let g:haskell_indent_if = 4
let g:haskell_indent_case = 4
let g:haskell_indent_let = 4
let g:haskell_indent_where = 2
let g_haskell_indent_before_where = 2
let g_haskell_indent_after_bare_where = 2
let g_haskell_indent_do = 4
let g_haskell_indent_in = 2

" Idris

" haskellConcealPlus settings
" TODO: there's a bug with 'S'
let hscoptions="STEMsrl↱w-tBQZNDC"

" coc.nvim
set hidden
set nobackup
set nowritebackup
set shortmess+=c
set signcolumn=yes

let g:vista_icon_indent = ["╰─▸ ", "├─▸ "]

let g:vista_default_executive = 'coc'
let g:vista#renderer#enable_icon = 0
" let g:vista#renderer#icons = {
" \   "function": "\uf794",
" \   "variable": "\uf71b",
" \  }

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Keyboard customization
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Which-key
nnoremap <silent> <leader>      :<c-u>WhichKeyVisual '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKeyVisual  ','<CR>

" Light/dark theme
map <Leader>cl :set background=light<cr>:colorscheme solarized8<cr>
map <Leader>cd :set background=dark<cr>:colorscheme molokai<cr>

" NERDTree
map <Leader>nt :NERDTreeToggle<CR>

map <Leader>ar  :Tabularize /

map <Leader>v :Vista!!<CR>

" fzf - find - f
map <Leader><Space> :call fzf#run(fzf#wrap({'source': 'git ls-files --exclude-standard --others --cached'}))<CR>
map <Leader>fg      :GFiles?<CR>
map <Leader>fb      :Buffers<CR>
map <Leader>cr      :Rg<CR>
map <Leader>bb      :Buffers<CR>
map <Leader>ft      :tag <C-r><C-w><CR>
map <Leader>fc      :Commits<CR>

" window - w
map <Leader>ss :vsplit<CR>
map <Leader>se :split<CR>

map <Leader>wr <C-w>r

" term - t
map <Leader>tt :vsplit<CR>:term<CR>
map <Leader>th :split<CR>:term<CR>
" Get out of terminal mode
tnoremap <Esc> <C-\><C-n>

" buffer - b
map <Leader>bd :Bd<Cr>

" git stuff - g
map <Leader>gg :Gstatus<CR>
map <Leader>gs :Gstatus<CR>
map <Leader>gp :Gpush<CR>
map <Leader>gd :Gvdiffsplit!<CR>
map <Leader>gsd :Gdiffsplit!<CR>
map <Leader>dh :diffget //2<CR>:diffupdate<CR>
map <Leader>dl :diffget //3<CR>:diffupdate<CR>
map <Leader>gw :Gwrite<CR>
map <Leader>du :diffupdate<CR>

" Theme - t
" map <Leader>tt :Tags<CR>
map <Leader>tc :Colors<CR>

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

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Format
nmap <leader>f <Plug>(coc-format-selected)

""" Under test
nnoremap <leader>ut :UndotreeToggle<cr>

