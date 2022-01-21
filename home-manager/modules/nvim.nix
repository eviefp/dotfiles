#*****************************************************************************
# Neovim module
#
#
#****************************************************************************
{ lib, config, pkgs, ... }:
let
  cfg = config.evie.programs.nvim;
  vimUtils = pkgs.vimUtils;
  sources = import ../../nix/sources.nix;

  editorConfig = vimUtils.buildVimPlugin {
    name = "editorconfig-vim-master-3078cd1";
    src = sources.editorconfig-vim;
  };

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
    src = sources.vim-devicons;
  };

  multipleCursors = vimUtils.buildVimPlugin {
    name = "vim-multiple-cursors-master-6456718";
    src = sources.vim-multiple-cursors;
  };

  whichKey = vimUtils.buildVimPlugin {
    name = "vim-which-key-master-165772f";
    src = sources.vim-which-key;
  };

  bbye = vimUtils.buildVimPlugin {
    name = "vim-bbye-master-25ef93a";
    src = sources.vim-bbye;
  };

  highlightedYank = vimUtils.buildVimPlugin {
    name = "vim-highlightedyank-master-9669226";
    src = sources.vim-highlightedyank;
  };

  commentary = vimUtils.buildVimPlugin {
    name = "vim-commentary-master-627308e";
    src = sources.vim-commentary;
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

  coc = vimUtils.buildVimPlugin {
    name = "coc.nvim-release-07a5708";
    src = sources."coc.nvim";
  };

  markdown = vimUtils.buildVimPlugin {
    name = "vim-markdown-master-59a551f";
    src = sources.vim-markdown;
  };

  haskell = vimUtils.buildVimPlugin {
    name = "haskell-vim-master-be00e14";
    src = sources.haskell-vim;
  };

  curry = vimUtils.buildVimPlugin {
    name = "curry.vim-master-b94bd87";
    src = sources."curry.vim";
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

  trailingWhitespace = vimUtils.buildVimPlugin {
    name = "vim-trailing-whitespace-master-05f068e";
    src = sources.vim-trailing-whitespace;
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
in {
  imports = [ ];

  options.evie.programs.nvim = {
    enable = lib.options.mkEnableOption "Enable neovim.";
  };

  config = (lib.mkIf cfg.enable {
    home.sessionVariables = { EDITOR = "nvim"; };
    home.file.".config/nvim/coc-settings.json".source =
      ../../config/nvim/coc-settings.json;

    programs.neovim = {
      enable = true;
      # This does not work!
      # package = pkgs.neovim;
      plugins = [
        abolish
        airline
        airlineThemes
        bbye
        coc
        commentary
        curry
        devIcons
        editorConfig
        fugitive
        pkgs.vimPlugins.fzf-vim
        haskell
        highlightedYank
        lexical
        markdown
        molokai
        multipleCursors
        nix
        purescript
        rainbow
        signature
        solarized
        surround
        tabular
        trailingWhitespace
        whichKey
      ];
      extraConfig = builtins.readFile ../../config/nvim/nix-init.vim;
      withNodeJs = true;
      withPython3 = true;
      withRuby = false;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
    };
  });
}
