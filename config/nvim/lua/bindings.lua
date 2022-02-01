local map = vim.api.nvim_set_keymap

-------------------------------------------------------------------------------
-- window (w)
map('' , '<leader>we' , ':vsplit<cr>' , {})
map('' , '<leader>ws' , ':split<cr>'  , {})
map('' , '<leader>wr' , '<C-w>r'      , {})
map('' , '<leader>wq' , ':q<cr>'      , {})
map('' , '<leader>ww' , ':w<cr>'      , {})

-------------------------------------------------------------------------------
-- term (t)
map(''  , '<leader>tt' , ':vsplit<cr>:term<cr>a' , {})
map(''  , '<leader>tg' , ':split<cr>:term<cr>a'  , {})
map('t' , '<esc>'      , '<C-\\><C-n>'            , {})


-------------------------------------------------------------------------------
-- lsp
map('n' , '<leader>xj' , '<cmd>lua vim.diagnostic.goto_next()<cr>'                          , { noremap = true   , silent = true })
map('n' , '<leader>xk' , '<cmd>lua vim.diagnostic.goto_prev()<cr>'                          , { noremap = true   , silent = true })
map('n' , '<leader>rn' , '<cmd>lua vim.lsp.buf.rename()<cr>'                                , { noremap = true   , silent = true })
map('n' , '<leader>xf' , '<cmd>lua vim.lsp.buf.formatting()<cr>'                            , { noremap = true   , silent = true })

map('n' , 'gD'         , '<cmd>vsplit<cr><cmd>lua vim.lsp.buf.definition()<cr>'             , { noremap = true   , silent = true })
map('n' , 'K'          , '<cmd>lua vim.lsp.buf.hover()<cr>'                                 , { noremap = true   , silent = true })
map('n' , '<c-k>'      , '<cmd>lua vim.lsp.buf.signature_help()<cr>'                        , { noremap = true   , silent = true })


-------------------------------------------------------------------------------
-- localleader stuff (,)
-- TODO: should only work for tex
map('n' , '<localleader>p'  , '$a \\pause<esc>'                                                        , {})
map('n' , '<localleader>cp' , '$a ~\\pause~<esc>'                                                      , {})
map('n' , '<localleader>eq' , 'o\\setcounter{equation}{0}<cr>\\begin{eqnarray}<cr>\\end{eqnarray}<esc>O' , {})

-------------------------------------------------------------------------------
-- Tabularize
map('v', '<leader>ar', ':Tabularize /', {})

