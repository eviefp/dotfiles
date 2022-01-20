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
" Plug 'nathanaelkane/vim-indent-guides'

" %S stuff
Plug 'tpope/vim-abolish'

""" Git
" Basic git plugin, g? for help in gstatus
Plug 'tpope/vim-fugitive'

" Additional plugin for github (pretty much for :Gbrowse I think)
Plug 'tpope/vim-rhubarb'
" Git gutter, must read more
" Plug 'airblade/vim-gitgutter'
" Try out: https://github.com/jreybert/vimagit

""" Coding
" Language Server
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Markdown
Plug 'tpope/vim-markdown'

""" Haskell
" Indentation and syntax highlighting
Plug 'eviefp/haskell-vim'
"Plug 'enomsg/vim-haskellConcealPlus'

" <C-y> to insert function above, [[ and ]] to navigate
Plug 'edkolev/curry.vim'


""" PureScript
Plug 'purescript-contrib/purescript-vim'

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
Plug 'morhetz/gruvbox'
Plug 'jnurmine/Zenburn'
Plug 'jacoborus/tender.vim'
Plug 'sjl/badwolf'

""" Testing
Plug 'xuyuanp/nerdtree-git-plugin'
Plug 'mbbill/undotree'
Plug 'kshenoy/vim-signature'
Plug 'plasticboy/vim-markdown'
Plug 'bronson/vim-trailing-whitespace'

Plug 'justinmk/vim-sneak'

" Plug 'liuchengxu/vista.vim'

Plug 'reedes/vim-lexical'

Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }

call plug#end()

source ~/.config/nvim/nix-init.vim