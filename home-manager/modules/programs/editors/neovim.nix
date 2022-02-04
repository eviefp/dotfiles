/****************************************************************************
  * Neovim module
  *
  * Neovim package, plugins, and init file.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.editors.neovim;
  vimUtils = pkgs.vimUtils;
  sources = import ../../../../nix/sources.nix;
  unstable = import sources.unstable { };

  airline = vimUtils.buildVimPlugin {
    name = "vim-airline-master-95935e6";
    src = sources.vim-airline;
  };

  airlineThemes = vimUtils.buildVimPlugin {
    name = "vim-airline-themes-master-97cf3e6";
    src = sources.vim-airline-themes;
  };

  visualMulti = vimUtils.buildVimPlugin {
    name = "vim-visual-multi-master-e209089";
    src = sources.vim-visual-multi;
  };

  bbye = vimUtils.buildVimPlugin {
    name = "vim-bbye-master-25ef93a";
    src = sources.vim-bbye;
  };

  kommentary = vimUtils.buildVimPlugin {
    name = "kommentary-master-a190d05";
    src = sources.kommentary;
  };

  tabular = vimUtils.buildVimPlugin {
    name = "tabular-master-339091a";
    src = sources.tabular;
  };

  surround = vimUtils.buildVimPlugin {
    name = "vim-surround-master-aeb9332";
    src = sources.vim-surround;
  };

  rainbow = vimUtils.buildVimPlugin {
    name = "rainbow-master-c18071e";
    src = sources.rainbow;
  };

  abolish = vimUtils.buildVimPlugin {
    name = "vim-abolish-master-3f0c8fa";
    src = sources.vim-abolish;
  };

  fugitive = vimUtils.buildVimPlugin {
    name = "vim-fugitive-master-a93ceff";
    src = sources.vim-fugitive;
  };

  rhubarb = vimUtils.buildVimPlugin {
    name = "vim-rhubarb-master-977b3cc";
    src = sources.vim-rhubarb;
  };

  markdown = vimUtils.buildVimPlugin {
    name = "vim-markdown-master-59a551f";
    src = sources.vim-markdown;
  };

  purescript = vimUtils.buildVimPlugin {
    name = "purescript-vim-master-d493b57";
    src = sources.purescript-vim;
  };

  nix = vimUtils.buildVimPlugin {
    name = "vim-nix-master-63b47b3";
    src = sources.vim-nix;
  };

  signature = vimUtils.buildVimPlugin {
    name = "vim-signature-master-6bc3dd1";
    src = sources.vim-signature;
  };

  betterWhitespace = vimUtils.buildVimPlugin {
    name = "vim-better-whitespace-master-c5afbe9";
    src = sources.vim-better-whitespace;
  };

  lexical = vimUtils.buildVimPlugin {
    name = "vim-lexical-master-0898c0c";
    src = sources.vim-lexical;
  };

  solarized = vimUtils.buildVimPlugin {
    name = "vim-solarized8-master-28b81a4";
    src = sources.vim-solarized8;
  };

  plenary = vimUtils.buildVimPlugin {
    name = "plenary.nvim-master-563d9f6";
    src = sources."plenary.nvim";
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  telescope = vimUtils.buildVimPlugin {
    name = "telescope-master-0011b11";
    src = sources."telescope.nvim";
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  treesitter = vimUtils.buildVimPlugin {
    name = "nvim-treesitter-master-620cc93";
    src = sources.nvim-treesitter;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  lspConfig = vimUtils.buildVimPlugin {
    name = "nvim-lspconfig-master-c510964";
    src = sources.nvim-lspconfig;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  cmp = vimUtils.buildVimPlugin {
    name = "nvim-cmp-main-d931042";
    src = sources.nvim-cmp;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  cmpPath = vimUtils.buildVimPlugin {
    name = "cmp-path-main-c5230cb";
    src = sources.cmp-path;
  };

  cmpVsnip = vimUtils.buildVimPlugin {
    name = "cmp-vsnip-main-0abfa18";
    src = sources.cmp-vsnip;
  };

  vimVsnip = vimUtils.buildVimPlugin {
    name = "vim-vsnip-main-7fde9c0";
    src = sources.vim-vsnip;
  };

  cmpLsp = vimUtils.buildVimPlugin {
    name = "cmp-nvim-lsp-main-ebdfc20";
    src = sources.cmp-nvim-lsp;
  };

  lspKind = vimUtils.buildVimPlugin {
    name = "lspkind-nvim-master-f0d1552";
    src = sources.lspkind-nvim;
  };

  cmpEmoji = vimUtils.buildVimPlugin {
    name = "cmp-emoji-main-19075c3";
    src = sources.cmp-emoji;
  };

 cmpLatexSymbols = vimUtils.buildVimPlugin {
    name = "cmp-latex-symbols-main-29dc9e5";
    src = sources.cmp-latex-symbols;
  };

 cmpLua = vimUtils.buildVimPlugin {
    name = "cmp-nvim-lua-main-d276254";
    src = sources.cmp-nvim-lua;
  };

  gitSigns = vimUtils.buildVimPlugin {
    name = "gitsigns.nvim-main-4861666";
    src = sources."gitsigns.nvim";
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  telescopefzf = vimUtils.buildVimPlugin {
    name = "telescope-fzf-native.nvim-main-b8662b0";
    src = sources."telescope-fzf-native.nvim";
  };

  devIcons = vimUtils.buildVimPlugin {
    name = "nvim-dev-webicons-master-634e268";
    src = sources.nvim-web-devicons;
  };

  eunuch = vimUtils.buildVimPlugin {
    name = "vim-eunuch-master-e2c9e01";
    src = sources.vim-eunuch;
  };

  whichKey = vimUtils.buildVimPlugin {
    name = "which-key.nvim-main-28d2bd1";
    src = sources."which-key.nvim";
  };

  melange = vimUtils.buildVimPlugin {
    name = "melange-master-e3a3a2c";
    src = sources.melange;
    configurePhase = ''
      rm -rf makefile
    '';
  };

  symbols = vimUtils.buildVimPlugin {
    name = "telescope-symbols-master-d2d7d6b";
    src = sources."telescope-symbols.nvim";
  };

  biscuits = vimUtils.buildVimPlugin {
    name = "nvim-biscuits-main-15a0cb";
    src = sources.nvim-biscuits;
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
      (unstable.neovim.override {
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
            airline
            airlineThemes
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
            markdown
            melange
            nix
            plenary
            purescript
            rainbow
            rhubarb
            signature
            solarized
            surround
            symbols
            tabular
            telescope
            telescopefzf
            treesitter
            vimVsnip
            visualMulti
            whichKey
            biscuits
          ];
        };
      })
    ];

  });
}
