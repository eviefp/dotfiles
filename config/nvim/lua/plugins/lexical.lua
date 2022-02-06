-------------------------------------------------------------------------------
-- lexical
-------------------------------------------------------------------------------

vim.cmd [[
augroup lexical
  autocmd!
  autocmd FileType markdown,mkd,md call lexical#init()
  autocmd FileType textile call lexical#init()
  autocmd FileType tex call lexical#init()
  autocmd FileType text call lexical#init()
augroup END
]]


vim.g['lexical#thesaurus']      = { '~/.config/nvim/lua/thesaurus.txt' }
vim.g['lexical#spell_key']      = '<leader>s'
vim.g['lexical#thesaurus_key']  = '<leader>st'
vim.g['lexical#dictionary_key'] = '<leader>sk'

