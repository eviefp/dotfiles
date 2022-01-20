"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Generic vim setup
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set autoread

" let g:stylish_haskell_command = "stylish-haskell"
" let g:stylish_haskell_dont_override = "false"

" Leader setup
let mapleader = ' '
let maplocalleader = ','

au BufEnter github.com_*.txt set filetype=markdown

let g:firenvim_config = {
    \ 'globalSettings': {
        \ 'alt': 'all',
    \  },
    \ 'localSettings': {
        \ '.*': {
            \ 'cmdline': 'neovim',
            \ 'priority': 0,
            \ 'selector': 'textarea',
            \ 'takeover': 'always',
        \ },
    \ }
\ }

if exists('g:started_by_firenvim')
  let g:airline_disable_statusline = 1
  let g:airline#extensions#tabline#enabled = 0
  set laststatus=0
else
  let g:airline#extensions#tabline#enabled = 1
  set laststatus=2
endif

" let fc = g:firenvim_config['localSettings']
" let fc['https?://[^/]+\.co\.uk/'] = { 'takeover': 'never', 'priority': 1 }

" Not sure what these are
" execute "set t_8f=\e[38;2;%lu;%lu;%lum"
" let $NVIM_TUI_ENABLE_TRUE_COLOR = 1

" Relative line numbers
" set number relativenumber
" set nu rnu
" au TermOpen * setlocal nonumber norelativenumber

" Fill column indicator
" set colorcolumn=81
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%82v', 100)

" TODO: Figure out what each of these are.
set tabstop=4
set shiftwidth=0
set expandtab
"
set autoindent

"""
" let g:purescript_disable_indent=1
"""

" These seem like they turn on indentation by plugins and syntax highlighting.
filetype plugin indent on
syntax on

set nocompatible

augroup lexical
  autocmd!
  autocmd FileType markdown,mkd call lexical#init()
  autocmd FileType textile call lexical#init()
  autocmd FileType tex call lexical#init()
  autocmd FileType text call lexical#init()
augroup END

" Better search
set incsearch
set hlsearch
set smartcase

" Not sure what these are
" au InsertEnter * set nocursorline
" au InsertLeave * set cursorline

" set cursorline
" set cursorcolumn

" set ruler

" Things like "zb" will keep 3 lines below target
set scrolloff=3

" Disable folding, what does this do?
set nofoldenable

" Basic setup for text files
autocmd FileType md setlocal textwidth=80 spell
autocmd FileType txt setlocal textwidth=80 spell
autocmd FileType tex setlocal textwidth=80 spell

" Show substitutions as you type them
set inccommand=nosplit

""" Plugin-specific
" for vim-devicons
set encoding=UTF-8

" Enable rainbow parentheses
let g:rainbow_active=1

" Enable indent guides
" let g:indent_guides_enable_on_vim_startup = 1

" Added for fzf
set splitbelow splitright

" Theme and stuff
set background=dark
set termguicolors
colorscheme molokai
hi Normal guibg=NONE ctermbg=NONE

" Airline
" let g:airline_theme = 'molokai'
let g:palenight_terminal_italics=1

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
let g_haskell_disable = 1

" coc.nvim
set hidden
set nobackup
set nowritebackup
set shortmess+=c
" set signcolumn=yes

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Keyboard customization
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Which-key
nnoremap <silent> <leader>      :<c-u>WhichKeyVisual '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKeyVisual  ','<CR>
nnoremap \ "_

" Light/dark theme
map <Leader>cl :set background=light<cr>:colorscheme solarized8<cr>
map <Leader>cd :set background=dark<cr>:colorscheme molokai<cr>

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
map <Leader>gg :Git<CR>
map <Leader>gs :Git<CR>
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

""" tex stuff
nnoremap <localleader>p $a \pause<ESC>
nnoremap <localleader>lp i~\pause~<ESC>
nnoremap <localleader>eq o\setcounter{equation}{0}<CR>\begin{eqnarray}<CR>\end{eqnarray}<ESC>O

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

nmap <silent> <leader>cf :call CocAction('format')<cr>