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

  vim-markdown = vimUtils.buildVimPlugin {
    name = "vim-markdown-master";
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

  symbols = vimUtils.buildVimPlugin {
    name = "telescope-symbols-master-d2d7d6b";
    src = sources."telescope-symbols.nvim";
  };

  biscuits = vimUtils.buildVimPlugin {
    name = "nvim-biscuits-main-15a0cb";
    src = sources.nvim-biscuits;
  };

  luaLine = vimUtils.buildVimPlugin {
    name = "lualine.nvim-master-aed7f25";
    src = sources."lualine.nvim";
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  catppuccin = vimUtils.buildVimPlugin {
    name = "nvim-main-d48d392";
    src = sources.catppuccin;
  };


  octo = vimUtils.buildVimPlugin {
    name = "octo.nvim-master-d21c5cc";
    src = sources."octo.nvim";
  };

  trouble = vimUtils.buildVimPlugin {
    name = "trouble.nvim-master-d035ef2";
    src = sources."trouble.nvim";
  };

  harpoon = vimUtils.buildVimPlugin {
    name = "harpoon-master-d035ef2";
    src = sources.harpoon;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  textobjects = vimUtils.buildVimPlugin {
    name = "nvim-treesitter-textobjects-master-438d2cf";
    src = sources.nvim-treesitter-textobjects;
  };

  tokyo = vimUtils.buildVimPlugin {
    name = "tokyonight.nvim-main-8223c97";
    src = sources."tokyonight.nvim";
  };

  gitMessenger = vimUtils.buildVimPlugin {
    name = "git-messenger.vim-master-2e67899";
    src = sources."git-messenger.vim";
  };

  hop = vimUtils.buildVimPlugin {
    name = "hop.nvim-master-f418a37";
    src = sources."hop.nvim";
    configurePhase = ''
      rm -rf doc
      '';
  };

  merge = vimUtils.buildVimPlugin {
    name = "vim-mergetool-master-0275a85";
    src = sources."vim-mergetool";
  };

  neorg = vimUtils.buildVimPlugin {
    name = "neorg-main-944de8a";
    src = sources.neorg;
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  truezen = vimUtils.buildVimPlugin {
    name = "truezen.nvim-main-508b977";
    src = sources."truezen.nvim";
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  sandwich = vimUtils.buildVimPlugin {
    name = "vim-sandwitch-master-48acdad";
    src = sources.vim-sandwich;
  };

  snitch = vimUtils.buildVimPlugin {
    name = "nvim-snitch-ed57352";
    src = sources.nvim-snitch;
  };

  fairyfloss = vimUtils.buildVimPlugin {
    name = "fairyfloss.vim-master-61c8bbd";
    src = sources."fairyfloss.vim";
  };

  material = vimUtils.buildVimPlugin {
    name = "material.vim-main-445534b";
    src = sources."material.vim";
  };

  neorg-telescope = vimUtils.buildVimPlugin {
    name = "neorg-telescope-main-75c2ad0";
    src = sources.neorg-telescope;
  };

  tresitter-refactor = vimUtils.buildVimPlugin {
    name = "nvim-treesitter-refactor-master-0dc8069";
    src = sources.nvim-treesitter-refactor;
  };

  telescope-file-browser = vimUtils.buildVimPlugin {
    name = "telescope-file-browser.nvim-master-d06fe1d";
    src = sources."telescope-file-browser.nvim";
    configurePhase = ''
      rm -rf Makefile
    '';
  };

  neuron = vimUtils.buildVimPlugin {
    name = "neuron-master-c44032e";
    src = sources."neuron.nvim";
  };

  nerveux = vimUtils.buildVimPlugin {
    name = "nerveux.nvim-master";
    src = sources."nerveux.nvim";
  };

  firenvim = vimUtils.buildVimPlugin {
    name = "firenvim-master";
    src = sources."firenvim";
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
            telescopefzf
            treesitter
            vimVsnip
            visualMulti
            whichKey
            biscuits
            luaLine
            catppuccin
            octo
            trouble
            harpoon
            tokyo
            gitMessenger
            hop
            merge
            neorg
            truezen
            sandwich
            snitch
            fairyfloss
            material
            neorg-telescope
            tresitter-refactor
            telescope-file-browser
            neuron
            nerveux
            firenvim
          ];
        };
      })
    ];

  });
}
