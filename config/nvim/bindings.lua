local map = vim.api.nvim_set_keymap

-------------------------------------------------------------------------------
-- find stuff (f)
map('' , '<leader><space>' , ":call fzf#run(fzf#wrap({'source': 'git ls-files --exclude-standard --others --cached'}))<CR>" , {})
map('' , '<leader>fb'      , ':Buffers<cr>'                                                                                 , {})
map('' , '<leader>ff'      , ':Rg<cr>'                                                                                      , {})
map('' , '<leader>fc'      , ':Commits<cr>'                                                                                 , {})

-------------------------------------------------------------------------------
-- window (w)
map('' , '<leader>we' , ':vsplit<cr>' , {})
map('' , '<leader>ws' , ':split<cr>'  , {})
map('' , '<leader>wr' , '<C-w>r'      , {})
map('' , '<leader>wq' , ':q<cr>'      , {})
map('' , '<leader>ww' , ':w<cr>'      , {})

-------------------------------------------------------------------------------
-- buffer (b)
map('', '<leader>bd', ':bdelete<cr>', {})

-------------------------------------------------------------------------------
-- term (t)
map(''  , '<leader>tt' , ':vsplit<cr>:term<cr>a' , {})
map(''  , '<leader>tg' , ':split<cr>:term<cr>a'  , {})
map('t' , '<esc>'      , '<C-\\><C-n>'            , {})

-------------------------------------------------------------------------------
-- git (g)
map('' , '<leader>gs'  , ':Git<cr>'                        , {})
map('' , '<leader>gp'  , ':Git push<cr>'                   , {})
map('' , '<leader>gu'  , ':Git pull<cr>'                   , {})
map('' , '<leader>gsd' , ':Gdiffsplit!<cr>'                , {})
map('' , '<Leader>gdh' , ':diffget //2<cr>:diffupdate<cr>' , {})
map('' , '<Leader>gdl' , ':diffget //3<cr>:diffupdate<cr>' , {})

-------------------------------------------------------------------------------
-- localleader stuff (,)
-- TODO: should only work for tex
map('n' , '<localleader>p'  , '$a \\pause<esc>'                                                        , {})
map('n' , '<localleader>cp' , '$a ~\\pause~<esc>'                                                      , {})
map('n' , '<localleader>eq' , 'o\\setcounter{equation}{0}<cr>\\begin{eqnarray}<cr>\\end{eqnarray}<esc>O' , {})

-------------------------------------------------------------------------------
-- coc-nvim

-- general stuff
map('i' , '<c-space>'      , 'coc#refresh()'                               , { noremap = true   , expr = true    , silent = true })
map('i' , '<cr>'           , 'pumvisible() ? "\\<c-y>" : "\\<c-g>u\\<cr>"' , { noremap = true   , expr = true })

-- lsp errors
map('n' , '<localleader>h' , '<Plug>(coc-diagnostic-prev)'                 , { silent = true })
map('n' , '<localleader>l' , '<Plug>(coc-diagnostic-next)'                 , { silent = true })

-- goto (g)
map('n' , 'gd' , '<Plug>(coc-definition)'      , { silent = true })
map('n' , 'gy' , '<Plug>(coc-type-definition)' , { silent = true })
map('n' , 'gi' , '<Plug>(coc-implementation)'  , { silent = true })
map('n' , 'gr' , '<Plug>(coc-references)'      , { silent = true })

-- random stuff, should consolidate
map('n' , '<leader>rn' , '<Plug>(coc-rename)'      , {})
map('n' , '<leader>ac' , '<Plug>(coc-codeaction)'  , {})
map('n' , '<leader>qf' , '<Plug>(coc-fix-current)' , {})

-- hover docs
map('n' , 'K', "call CocAction('doHover')<cr>", { noremap = true })

map('n', '<leader>cf', ":call CocAction('format')", { silent = true })

-------------------------------------------------------------------------------
-- Tabularize
map('v', '<leader>ar', ':Tabularize /', {})

