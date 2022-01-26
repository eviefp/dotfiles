/****************************************************************************
  * Neovim module
  *
  * Neovim package, plugins, and init file.
  **************************************************************************/
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.editors.nvim;
  vimUtils = pkgs.vimUtils;
  sources = import ../../../../nix/sources.nix;
  unstable = import sources.unstable { };

  devIcons = vimUtils.buildVimPlugin {
    name = "vim-devicons-master-a225865";
    src = sources.vim-devicons;
  };

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

  markdown = vimUtils.buildVimPlugin {
    name = "vim-markdown-master-59a551f";
    src = sources.vim-markdown;
  };

  haskell = vimUtils.buildVimPlugin {
    name = "haskell-vim-master-be00e14";
    src = sources.haskell-vim;
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

  molokai = vimUtils.buildVimPlugin {
    name = "lexical-master-c3c3cc7";
    src = sources.molokai;
  };

  solarized = vimUtils.buildVimPlugin {
    name = "vim-solarized8-master-28b81a4";
    src = sources.vim-solarized8;
  };

  betterLua = vimUtils.buildVimPlugin {
    name = "BetterLua.vim-master-d2d6c11";
    src = sources."BetterLua.vim";
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

  gitSigns = vimUtils.buildVimPlugin {
    name = "gitsigns.nvim-main-4861666";
    src = sources."gitsigns.nvim";
    configurePhase = ''
      rm -rf Makefile
      '';
  };

in
{
  imports = [ ];

  options.evie.programs.editors.nvim = {
    enable = lib.options.mkEnableOption "Enable neovim.";
  };

  config = (lib.mkIf cfg.enable {
    home.file = {
      ".config/nvim/lua/config.lua".source = ../../../../config/nvim/config.lua;
      ".config/nvim/lua/plugins.lua".source = ../../../../config/nvim/plugins.lua;
      ".config/nvim/lua/bindings.lua".source = ../../../../config/nvim/bindings.lua;
    };

    home.packages = [
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
            betterLua
            betterWhitespace
            cmp
            cmpLsp
            cmpVsnip
            devIcons
            fugitive
            gitSigns
            haskell
            kommentary
            lexical
            lspConfig
            markdown
            molokai
            nix
            plenary
            purescript
            rainbow
            signature
            solarized
            surround
            tabular
            telescope
            treesitter
            vimVsnip
            visualMulti
          ];
        };
      })
    ];

  });
}
