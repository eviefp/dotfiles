" Added with HIE stuff
"set hidden
"set nobackup
"set nowritebackup

set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes

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

Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}
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

let hscoptions="bfl↱tqSTEMxrw-RBQZNDC"

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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Haksell
" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

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

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Use <tab> for select selections ranges, needs server support, like: coc-tsserver, coc-python
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <S-TAB> <Plug>(coc-range-select-backword)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
