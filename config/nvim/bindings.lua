local map = vim.api.nvim_set_keymap

-------------------------------------------------------------------------------
-- find stuff (f)
map('n' , '<leader><leader>' , '<cmd>Telescope git_files<cr>'       , { noremap = true})
map('n' , '<leader>ff'       , '<cmd>Telescope live_grep<cr>'       , { noremap = true})
map('n' , '<leader>fb'       , '<cmd>Telescope buffers<cr>'         , { noremap = true})
map('n' , '<leader>fm'       , '<cmd>Telescope commands<cr>'        , { noremap = true})
map('n' , '<leader>fk'       , '<cmd>Telescope marks<cr>'           , { noremap = true})
map('n' , '<leader>fq'       , '<cmd>Telescope quickfix<cr>'        , { noremap = true})
map('n' , '<leader>fl'       , '<cmd>Telescope loclist<cr>'         , { noremap = true})
map('n' , '<leader>fc'       , '<cmd>Telescope git_commits<cr>'     , { noremap = true})
map('n' , '<leader>fC'       , '<cmd>Telescope git_bcommits<cr>'    , { noremap = true})
map('n' , '<leader>fs'       , '<cmd>Telescope git_status<cr>'      , { noremap = true})
map('n' , '<leader>fa'       , '<cmd>Telescope builtin.builtin<cr>' , { noremap = true})



-------------------------------------------------------------------------------
-- window (w)
map('' , '<leader>we' , ':vsplit<cr>' , {})
map('' , '<leader>ws' , ':split<cr>'  , {})
map('' , '<leader>wr' , '<C-w>r'      , {})
map('' , '<leader>wq' , ':q<cr>'      , {})
map('' , '<leader>ww' , ':w<cr>'      , {})

-------------------------------------------------------------------------------
-- buffer (b)
-- uses bbye
map('', '<leader>bd', ':Bdelete<cr>', {})

-------------------------------------------------------------------------------
-- term (t)
map(''  , '<leader>tt' , ':vsplit<cr>:term<cr>a' , {})
map(''  , '<leader>tg' , ':split<cr>:term<cr>a'  , {})
map('t' , '<esc>'      , '<C-\\><C-n>'            , {})

-------------------------------------------------------------------------------
-- git (g)
map('' , '<leader>gs'  , ':Git<cr>'                        , {})
map('' , '<leader>gg'  , ':GBrowse<cr>'                    , {})
map('' , '<leader>gb'  , ':Git blame<cr>'                  , {})
map('' , '<leader>gl'  , ':Git log<cr>'                    , {})
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
map('n' , '<leader>cl' , '<Plug>(coc-codelens-action)' , {})

-- experimenting
map('n' , '<leader>cd' , ':<c-u>CocList diagnostics<cr>' , { nowait = true , silent = true })
map('n' , '<leader>ce' , ':<c-u>CocList extensions<cr>'  , { nowait = true , silent = true })
map('n' , '<leader>cc' , ':<c-u>CocList commands<cr>'    , { nowait = true , silent = true })
map('n' , '<leader>co' , ':<c-u>CocList outline<cr>'     , { nowait = true , silent = true })
map('n' , '<leader>cs' , ':<c-u>CocList -I symbols<cr>'  , { nowait = true , silent = true })
map('n' , '<leader>cj' , ':<c-u>CocNext<cr>'             , { nowait = true , silent = true })
map('n' , '<leader>ck' , ':<c-u>CocPrev<cr>'             , { nowait = true , silent = true })
map('n' , '<leader>cp' , ':<c-u>CocListResume<cr>'       , { nowait = true , silent = true })

-- hover docs
map('n' , 'K', ":call CocAction('doHover')<cr>", { noremap = true })

map('n', '<leader>cf', ":call CocAction('format')", { silent = true })

-------------------------------------------------------------------------------
-- Tabularize
map('v', '<leader>ar', ':Tabularize /', {})

