/****************************************************************************
  * Neovim module
  *
  * Neovim package, plugins, and init file.
  **************************************************************************/
{ lib
, config
, pkgs
, nvim-visuals-multi
, nvim-bbye
, nvim-kommentary
, nvim-tabular
, nvim-rainbow
, nvim-abolish
, nvim-fugitive
, nvim-rhubarb
, nvim-markdown
, nvim-purescript
, nvim-nix
, nvim-signature
, nvim-better-whitespace
, nvim-lexical
, nvim-solarized
, nvim-plenary
, nvim-telescope
, nvim-treesitter
, nvim-lspconfig
, nvim-cmp
, nvim-cmp-path
, nvim-cmp-vsnip
, nvim-vim-vsnip
, nvim-cmp-lsp
, nvim-lspkind
, nvim-cmp-emoji
, nvim-cmp-latex-symbols
, nvim-cmp-lua
, nvim-gitsigns
, nvim-telescope-fzf
, nvim-dev-webicons
, nvim-eunuch
, nvim-which-key
, nvim-telescope-symbols
, nvim-lualine
, nvim-octo
, nvim-trouble
, nvim-harpoon
, nvim-tokyo
, nvim-git-messenger
, nvim-hop
, nvim-merge-tool
, nvim-truezen
, nvim-sandwich
, nvim-snitch
, nvim-fairy-floss
, nvim-material
, nvim-treesitter-refactor
, nvim-telescope-file-browser
, nvim-telescope-ui-select
, nvim-fish-syntax
, nvim-ranger
, nvim-libp
, nvim-pest
, ...
}:
let
  cfg = config.evie.programs.editors.neovim;
  vimUtils = pkgs.vimUtils;

  visualMulti = vimUtils.buildVimPlugin {
    name = "vim-visual-multi-master-e209089";
    src = nvim-visuals-multi;
  };

  bbye = vimUtils.buildVimPlugin {
    name = "vim-bbye-master-25ef93a";
    src = nvim-bbye;
  };

  kommentary = vimUtils.buildVimPlugin {
    name = "kommentary-master-a190d05";
    src = nvim-kommentary;
  };

  tabular = vimUtils.buildVimPlugin {
    name = "tabular-master-339091a";
    src = nvim-tabular;
  };

  rainbow = vimUtils.buildVimPlugin {
    name = "rainbow-master-c18071e";
    src = nvim-tabular;
  };

  abolish = vimUtils.buildVimPlugin {
    name = "vim-abolish-master-3f0c8fa";
    src = nvim-abolish;
  };

  fugitive = vimUtils.buildVimPlugin {
    name = "vim-fugitive-master-a93ceff";
    src = nvim-fugitive;
  };

  rhubarb = vimUtils.buildVimPlugin {
    name = "vim-rhubarb-master-977b3cc";
    src = nvim-rhubarb;
  };

  vim-markdown = vimUtils.buildVimPlugin {
    name = "vim-markdown-master";
    src = nvim-markdown;
  };

  purescript = vimUtils.buildVimPlugin {
    name = "purescript-vim-master-d493b57";
    src = nvim-purescript;
  };

  nix = vimUtils.buildVimPlugin {
    name = "vim-nix-master-63b47b3";
    src = nvim-nix;
  };

  signature = vimUtils.buildVimPlugin {
    name = "vim-signature-master-6bc3dd1";
    src = nvim-signature;
  };

  betterWhitespace = vimUtils.buildVimPlugin {
    name = "vim-better-whitespace-master-c5afbe9";
    src = nvim-better-whitespace;
  };

  lexical = vimUtils.buildVimPlugin {
    name = "vim-lexical-master-0898c0c";
    src = nvim-lexical;
  };

  solarized = vimUtils.buildVimPlugin {
    name = "vim-solarized8-master-28b81a4";
    src = nvim-solarized;
  };

  plenary = vimUtils.buildVimPlugin {
    name = "plenary.nvim-master-563d9f6";
    src = nvim-plenary;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  telescope = vimUtils.buildVimPlugin {
    name = "telescope-master-0011b11";
    src = nvim-telescope;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  treesitter = vimUtils.buildVimPlugin {
    name = "nvim-treesitter-master-620cc93";
    src = nvim-treesitter;
  };

  lspConfig = vimUtils.buildVimPlugin {
    name = "nvim-lspconfig-master-c510964";
    src = nvim-lspconfig;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  cmp = vimUtils.buildVimPlugin {
    name = "nvim-cmp-main-d931042";
    src = nvim-cmp;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  cmpPath = vimUtils.buildVimPlugin {
    name = "cmp-path-main-c5230cb";
    src = nvim-cmp-path;
  };

  cmpVsnip = vimUtils.buildVimPlugin {
    name = "cmp-vsnip-main-0abfa18";
    src = nvim-cmp-vsnip;
  };

  vimVsnip = vimUtils.buildVimPlugin {
    name = "vim-vsnip-main-7fde9c0";
    src = nvim-vim-vsnip;
  };

  cmpLsp = vimUtils.buildVimPlugin {
    name = "cmp-nvim-lsp-main-ebdfc20";
    src = nvim-cmp-lsp;
  };

  lspKind = vimUtils.buildVimPlugin {
    name = "lspkind-nvim-master-f0d1552";
    src = nvim-lspkind;
  };

  cmpEmoji = vimUtils.buildVimPlugin {
    name = "cmp-emoji-main-19075c3";
    src = nvim-cmp-emoji;
  };

  cmpLatexSymbols = vimUtils.buildVimPlugin {
    name = "cmp-latex-symbols-main-29dc9e5";
    src = nvim-cmp-latex-symbols;
  };

  cmpLua = vimUtils.buildVimPlugin {
    name = "cmp-nvim-lua-main-d276254";
    src = nvim-cmp-lua;
  };

  gitSigns = vimUtils.buildVimPlugin {
    name = "gitsigns.nvim-main-4861666";
    src = nvim-gitsigns;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  telescopefzf = vimUtils.buildVimPlugin {
    name = "telescope-fzf-native.nvim-main-b8662b0";
    src = nvim-telescope-fzf;
  };

  devIcons = vimUtils.buildVimPlugin {
    name = "nvim-dev-webicons-master-634e268";
    configurePhase = ''
      rm -rf Makefile
    '';
    src = nvim-dev-webicons;
  };

  eunuch = vimUtils.buildVimPlugin {
    name = "vim-eunuch-master-e2c9e01";
    src = nvim-eunuch;
  };

  whichKey = vimUtils.buildVimPlugin {
    name = "which-key.nvim-main-28d2bd1";
    src = nvim-which-key;
  };

  symbols = vimUtils.buildVimPlugin {
    name = "telescope-symbols-master-d2d7d6b";
    src = nvim-telescope-symbols;
  };

  luaLine = vimUtils.buildVimPlugin {
    name = "lualine.nvim-master-aed7f25";
    src = nvim-lualine;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  octo = vimUtils.buildVimPlugin {
    name = "octo.nvim-master-d21c5cc";
    src = nvim-octo;
  };

  trouble = vimUtils.buildVimPlugin {
    name = "trouble.nvim-master-d035ef2";
    src = nvim-trouble;
  };

  harpoon = vimUtils.buildVimPlugin {
    name = "harpoon-master-d035ef2";
    src = nvim-harpoon;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  tokyo = vimUtils.buildVimPlugin {
    name = "tokyonight.nvim-main-8223c97";
    src = nvim-tokyo;
  };

  gitMessenger = vimUtils.buildVimPlugin {
    name = "git-messenger.vim-master-2e67899";
    src = nvim-git-messenger;
  };

  hop = vimUtils.buildVimPlugin {
    name = "hop.nvim-master-f418a37";
    src = nvim-hop;
    configurePhase = ''
      rm -rf doc
    '';
  };

  merge = vimUtils.buildVimPlugin {
    name = "vim-mergetool-master-0275a85";
    src = nvim-merge-tool;
  };

  truezen = vimUtils.buildVimPlugin {
    name = "truezen.nvim-main-508b977";
    src = nvim-truezen;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  sandwich = vimUtils.buildVimPlugin {
    name = "vim-sandwitch-master-48acdad";
    src = nvim-sandwich;
  };

  snitch = vimUtils.buildVimPlugin {
    name = "nvim-snitch-ed57352";
    src = nvim-snitch;
  };

  fairyfloss = vimUtils.buildVimPlugin {
    name = "fairyfloss.vim-master-61c8bbd";
    src = nvim-fairy-floss;
  };

  material = vimUtils.buildVimPlugin {
    name = "material.vim-main-445534b";
    src = nvim-material;
  };

  tresitter-refactor = vimUtils.buildVimPlugin {
    name = "nvim-treesitter-refactor-master-0dc8069";
    src = nvim-treesitter-refactor;
  };

  telescope-file-browser = vimUtils.buildVimPlugin {
    name = "telescope-file-browser.nvim-master-d06fe1d";
    src = nvim-telescope-file-browser;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  # nerveux = vimUtils.buildVimPlugin {
  #   name = "nerveux.nvim-master";
  #   src = sources."nerveux.nvim";
  # };

  telescope-ui-select = vimUtils.buildVimPlugin {
    name = "telescope-ui-select";
    src = nvim-telescope-ui-select;
  };

  vim-fish-syntax = vimUtils.buildVimPlugin {
    name = "vim-fish-syntax";
    src = nvim-fish-syntax;
  };

  ranger-nvim = vimUtils.buildVimPlugin {
    name = "ranger-nvim";
    src = nvim-ranger;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  # required by ranger-nvim
  libp = vimUtils.buildVimPlugin {
    name = "libp.nvim";
    src = nvim-libp;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  pest = vimUtils.buildVimPlugin {
    name = "pest.vim";
    src = nvim-pest;
  };
in
{
  imports = [ ];

  options.evie.programs.editors.neovim = {
    enable = lib.options.mkEnableOption "Enable neovim.";
  };

  config = (lib.mkIf cfg.enable {
    home.file = {
      ".config/nvim/lua" = {
        source = ../../../../config/nvim/lua;
        recursive = true;
      };
    };

    home.packages = [
      pkgs.fd
      pkgs.gcc
      (pkgs.neovim.override {
        viAlias = true;
        vimAlias = true;
        withNodeJs = true;
        withPython3 = true;
        configure = {
          customRC = ''
            lua require('config')
            lua require('plugins')
            lua require('bindings')
          '';
          packages.myPlugins.start = [
            abolish
            bbye
            betterWhitespace
            cmp
            cmpEmoji
            cmpLatexSymbols
            cmpLsp
            cmpLua
            cmpPath
            cmpVsnip
            devIcons
            eunuch
            fugitive
            gitSigns
            kommentary
            lexical
            lspConfig
            lspKind
            vim-markdown
            nix
            plenary
            purescript
            rainbow
            rhubarb
            signature
            solarized
            symbols
            tabular
            telescope
            pkgs.vimPlugins.telescope-fzf-native-nvim
            pkgs.vimPlugins.nvim-treesitter-context
            pkgs.vimPlugins.nvim-treesitter.withAllGrammars
            pkgs.vimPlugins.nvim-treesitter-refactor
            vimVsnip
            visualMulti
            whichKey
            luaLine
            octo
            trouble
            harpoon
            tokyo
            gitMessenger
            hop
            merge
            truezen
            sandwich
            snitch
            fairyfloss
            material
            telescope-file-browser
            # nerveux
            telescope-ui-select
            vim-fish-syntax
            libp
            ranger-nvim
            pest
          ];
        };
      })
    ];

  });
}
