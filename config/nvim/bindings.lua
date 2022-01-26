local map = vim.api.nvim_set_keymap

-------------------------------------------------------------------------------
-- find stuff (f)
map('n' , '<leader><leader>' , '<cmd>Telescope git_files<cr>'       , { noremap = true})
map('n' , '<leader>ff'       , '<cmd>Telescope live_grep<cr>'       , { noremap = true})
map('n' , '<leader>fs'       , '<cmd>Telescope grep_string<cr>'     , { noremap = true})
map('n' , '<leader>fb'       , '<cmd>Telescope buffers<cr>'         , { noremap = true})
map('n' , '<leader>fm'       , '<cmd>Telescope commands<cr>'        , { noremap = true})
map('n' , '<leader>fk'       , '<cmd>Telescope marks<cr>'           , { noremap = true})
map('n' , '<leader>fq'       , '<cmd>Telescope quickfix<cr>'        , { noremap = true})
map('n' , '<leader>fl'       , '<cmd>Telescope loclist<cr>'         , { noremap = true})
map('n' , '<leader>fc'       , '<cmd>Telescope git_commits<cr>'     , { noremap = true})
map('n' , '<leader>fC'       , '<cmd>Telescope git_bcommits<cr>'    , { noremap = true})
map('n' , '<leader>fs'       , '<cmd>Telescope git_status<cr>'      , { noremap = true})
map('n' , '<leader>fa'       , '<cmd>Telescope builtin<cr>'         , { noremap = true})



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
-- lsp

map('n' , '<leader>xe' , '<cmd>lua vim.diagnostic.open_float()<cr>' , { noremap = true , silent = true })
map('n' , '<leader>xj' , '<cmd>lua vim.diagnostic.goto_next()<cr>'  , { noremap = true , silent = true })
map('n' , '<leader>xk' , '<cmd>lua vim.diagnostic.goto_prev()<cr>'  , { noremap = true , silent = true })
map('n' , '<leader>rn' , '<cmd>lua vim.lsp.buf.rename()<cr>'        , { noremap = true , silent = true })
map('n' , '<leader>ac' , '<cmd>lua vim.lsp.buf.code_action()<cr>'   , { noremap = true , silent = true })
map('n' , '<leader>xf' , '<cmd>lua vim.lsp.buf.formatting()<cr>'    , { noremap = true , silent = true })

map('n' , 'gD' , '<cmd>lua vim.lsp.buf.declaration()<cr>'    , { noremap = true , silent = true })
map('n' , 'gd' , '<cmd>lua vim.lsp.buf.definition()<cr>'     , { noremap = true , silent = true })
map('n' , 'K'  , '<cmd>lua vim.lsp.buf.hover()<cr>'          , { noremap = true , silent = true })
map('n' , 'gi' , '<cmd>lua vim.lsp.buf.implementation()<cr>' , { noremap = true , silent = true })
map('n' , 'gr' , '<cmd>lua vim.lsp.buf.references()<cr>'     , { noremap = true , silent = true })


-------------------------------------------------------------------------------
-- localleader stuff (,)
-- TODO: should only work for tex
map('n' , '<localleader>p'  , '$a \\pause<esc>'                                                        , {})
map('n' , '<localleader>cp' , '$a ~\\pause~<esc>'                                                      , {})
map('n' , '<localleader>eq' , 'o\\setcounter{equation}{0}<cr>\\begin{eqnarray}<cr>\\end{eqnarray}<esc>O' , {})

-------------------------------------------------------------------------------
-- Tabularize
map('v', '<leader>ar', ':Tabularize /', {})

