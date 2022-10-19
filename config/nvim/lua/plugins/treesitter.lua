-------------------------------------------------------------------------------
-- treesitter
-------------------------------------------------------------------------------

local parser_install_dir = "/home/evie/.local/share/treesitters"
vim.fn.mkdir(parser_install_dir, "p")

require'nvim-treesitter.configs'.setup {
  parser_install_dir = parser_install_dir,
  -- One of "all", "maintained" (parsers with maintainers), or a list of languages
  ensure_installed = {
    "agda",
    "bash",
    "c",
    "c_sharp",
    "cmake",
    "cpp",
    "css",
    "dockerfile",
    "fish",
    "gitattributes",
    "gitignore",
    "graphql",
    "html",
    "javascript",
    "json",
    "latex",
    "llvm",
    "lua",
    "make",
    "markdown",
    "markdown_inline",
    "nix",
    "prisma",
    "python",
    "rust ",
    "scss",
    "sql",
    "toml",
    "typescript",
    "vim",
    "yaml",
    -- unmaintained
    "haskell",

  },

  -- Install languages synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- List of parsers to ignore installing
  ignore_install = {},

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- list of language that will be disabled
    disable = {
      -- TOOD: remove when this gets fixed https://github.com/cstrahan/tree-sitter-nix/issues/23
      -- also see if removing vim-nix after this is fixed makes sense
      'org',
      'tex',
    },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = { 'org' },
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
  indent = {
    enable = true,
  },
  refactor = {
    highlight_definitions = {
      enable = true,
      clear_on_cursor_move = true,
    },
    highlight_current_scope = {
      enable = false,
    },
    smart_rename = {
      enable = true,
      keymaps = {
        smart_rename = "gmm",
      },
    },
  },
}

