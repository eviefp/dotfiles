-------------------------------------------------------------------------------
-- lspconfig
-------------------------------------------------------------------------------

local lspconfig = require 'lspconfig'

-- Add additional capabilities supported by nvim-cmp
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

-------------------------------------------------------------------------------
-- Haskell
lspconfig.hls.setup {
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = capabilities,
}

-------------------------------------------------------------------------------
-- nix
lspconfig.rnix.setup {
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = capabilities,
}

-------------------------------------------------------------------------------
-- tex
lspconfig.texlab.setup {
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = capabilities,
}

