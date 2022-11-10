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
  settings = {
    haskell = {
      formattingProvider = 'fourmolu',
    },
  },
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
-- rust
lspconfig.rust_analyzer.setup {
  settings = {
    ["rust-analyzer"] = {
      cargo = {
        loadOutDirsFromCheck = true,
      },
      procMacro = {
        enable = true,
      },
--      checkOnSave = {
--        command = "clippy"
--      },
    },
  },
}

-------------------------------------------------------------------------------
-- tex
lspconfig.texlab.setup {
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = capabilities,
}

-------------------------------------------------------------------------------
-- purescript
lspconfig.purescriptls.setup {
  settings = {
    purescript = {
      addSpagoSources = true
    },
  },
}

-------------------------------------------------------------------------------
-- typescript
lspconfig.tsserver.setup {}

-------------------------------------------------------------------------------
-- prisma
lspconfig.prismals.setup {}


