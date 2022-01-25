-------------------------------------------------------------------------------
-- settings
-------------------------------------------------------------------------------

-- enable rainbow parentheses
vim.cmd [[
let g:rainbow_active=1
]]

-- remove?
-- vim.cmd [[
-- let g:palenight_terminal_italics=1
-- ]]

-- haskell
vim.cmd [[
set concealcursor=nciv
let g:haskell_conceal = 0
let g:haskell_conceal_wide = 0
let g:haskell_coneal_enumerations = 0
let g:haskell_hsp = 0

let g:haskell_indent_if = 4
let g:haskell_indent_case = 4
let g:haskell_indent_let = 4
let g:haskell_indent_where = 2
let g_haskell_indent_before_where = 2
let g_haskell_indent_after_bare_where = 2
let g_haskell_indent_do = 4
let g_haskell_indent_in = 2
let g_haskell_disable = 1
]]
