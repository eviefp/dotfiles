{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-on-droid = {
      url = "github:nix-community/nix-on-droid/master";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    # neovim
    nvim-visuals-multi = {
      url = "github:mg979/vim-visual-multi/master";
      flake = false;
    };

    nvim-bbye = {
      url = "github:moll/vim-bbye/master";
      flake = false;
    };
    nvim-kommentary = {
      url = "github:b3nj5m1n/kommentary/main";
      flake = false;
    };
    nvim-tabular = {
      url = "github:godlygeek/tabular/master";
      flake = false;
    };
    nvim-rainbow = {
      url = "github:luochen1990/rainbow/master";
      flake = false;
    };
    nvim-abolish = {
      url = "github:tpope/vim-abolish/master";
      flake = false;
    };
    nvim-fugitive = {
      url = "github:tpope/vim-fugitive/master";
      flake = false;
    };
    nvim-rhubarb = {
      url = "github:tpope/vim-rhubarb/master";
      flake = false;
    };
    nvim-markdown = {
      url = "github:preservim/vim-markdown/master";
      flake = false;
    };
    nvim-purescript = {
      url = "github:purescript-contrib/purescript-vim/main";
      flake = false;
    };
    nvim-nix = {
      url = "github:LnL7/vim-nix/master";
      flake = false;
    };
    nvim-signature = {
      url = "github:kshenoy/vim-signature/master";
      flake = false;
    };
    nvim-better-whitespace = {
      url = "github:ntpeters/vim-better-whitespace/master";
      flake = false;
    };
    nvim-lexical = {
      url = "github:reedes/vim-lexical/master";
      flake = false;
    };
    nvim-solarized = {
      url = "github:lifepillar/vim-solarized8/master";
      flake = false;
    };
    nvim-plenary = {
      url = "github:nvim-lua/plenary.nvim/master";
      flake = false;
    };
    nvim-telescope = {
      url = "github:nvim-telescope/telescope.nvim/master";
      flake = false;
    };
    nvim-treesitter = {
      url = "github:nvim-treesitter/nvim-treesitter/master";
      flake = false;
    };
    nvim-lspconfig = {
      url = "github:neovim/nvim-lspconfig/master";
      flake = false;
    };
    nvim-cmp = {
      url = "github:hrsh7th/nvim-cmp/main";
      flake = false;
    };
    nvim-cmp-path = {
      url = "github:hrsh7th/cmp-path/main";
      flake = false;
    };
    nvim-cmp-vsnip = {
      url = "github:hrsh7th/cmp-vsnip/main";
      flake = false;
    };
    nvim-vim-vsnip = {
      url = "github:hrsh7th/vim-vsnip/master";
      flake = false;
    };
    nvim-cmp-lsp = {
      url = "github:hrsh7th/cmp-nvim-lsp/main";
      flake = false;
    };
    nvim-lspkind = {
      url = "github:onsails/lspkind-nvim/master";
      flake = false;
    };
    nvim-cmp-emoji = {
      url = "github:hrsh7th/cmp-emoji/main";
      flake = false;
    };
    nvim-cmp-latex-symbols = {
      url = "github:kdheepak/cmp-latex-symbols/main";
      flake = false;
    };
    nvim-cmp-lua = {
      url = "github:hrsh7th/cmp-nvim-lua/main";
      flake = false;
    };
    nvim-gitsigns = {
      url = "github:lewis6991/gitsigns.nvim/main";
      flake = false;
    };
    nvim-telescope-fzf = {
      url = "github:nvim-telescope/telescope-fzf-native.nvim/main";
      flake = false;
    };
    nvim-dev-webicons = {
      url = "github:kyazdani42/nvim-web-devicons/master";
      flake = false;
    };
    nvim-eunuch = {
      url = "github:tpope/vim-eunuch/master";
      flake = false;
    };
    nvim-which-key = {
      url = "github:folke/which-key.nvim/main";
      flake = false;
    };
    nvim-telescope-symbols = {
      url = "github:nvim-telescope/telescope-symbols.nvim/master";
      flake = false;
    };
    nvim-lualine = {
      url = "github:nvim-lualine/lualine.nvim/master";
      flake = false;
    };
    nvim-octo = {
      url = "github:pwntester/octo.nvim/master";
      flake = false;
    };
    nvim-trouble = {
      url = "github:folke/trouble.nvim/main";
      flake = false;
    };
    nvim-harpoon = {
      url = "github:theprimeagen/harpoon/master";
      flake = false;
    };
    nvim-tokyo = {
      url = "github:folke/tokyonight.nvim/main";
      flake = false;
    };
    nvim-git-messenger = {
      url = "github:rhysd/git-messenger.vim/master";
      flake = false;
    };
    nvim-hop = {
      url = "github:phaazon/hop.nvim/master";
      flake = false;
    };
    nvim-merge-tool = {
      url = "github:samoshkin/vim-mergetool/master";
      flake = false;
    };
    nvim-truezen = {
      url = "github:pocco81/truezen.nvim/main";
      flake = false;
    };
    nvim-sandwich = {
      url = "github:machakann/vim-sandwich/master";
      flake = false;
    };
    nvim-snitch = {
      url = "github:tssm/nvim-snitch/main";
      flake = false;
    };
    nvim-fairy-floss = {
      url = "github:tssm/fairyfloss.vim/master";
      flake = false;
    };
    nvim-material = {
      url = "github:kaicataldo/material.vim/main";
      flake = false;
    };
    nvim-treesitter-refactor = {
      url = "github:nvim-treesitter/nvim-treesitter-refactor/master";
      flake = false;
    };
    nvim-telescope-file-browser = {
      url = "github:nvim-telescope/telescope-file-browser.nvim/master";
      flake = false;
    };
    nvim-telescope-ui-select = {
      url = "github:nvim-telescope/telescope-ui-select.nvim/master";
      flake = false;
    };
    nvim-fish-syntax = {
      url = "github:khaveesh/vim-fish-syntax/master";
      flake = false;
    };
    nvim-ranger = {
      url = "github:ipod825/ranger.nvim/main";
      flake = false;
    };
    nvim-libp = {
      url = "github:ipod825/libp.nvim/main";
      flake = false;
    };
    nvim-pest = {
      url = "github:pest-parser/pest.vim/master";
      flake = false;
    };
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , home-manager
    , nix-on-droid
    , emacs-overlay
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
    }:
    let
      system = "x86_64-linux";
    in
    {
      nixosConfigurations."thelxinoe" = nixpkgs.lib.nixosSystem {
        system = system;
        modules =
          [
            ./system/thelxinoe/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = {
                inherit nvim-visuals-multi nvim-bbye nvim-kommentary
                  nvim-tabular nvim-rainbow nvim-abolish nvim-fugitive
                  nvim-rhubarb nvim-markdown nvim-purescript nvim-nix
                  nvim-signature nvim-better-whitespace nvim-lexical
                  nvim-solarized nvim-plenary nvim-telescope nvim-treesitter
                  nvim-lspconfig nvim-cmp nvim-cmp-path nvim-cmp-vsnip
                  nvim-vim-vsnip nvim-cmp-lsp nvim-lspkind nvim-cmp-emoji
                  nvim-cmp-latex-symbols nvim-cmp-lua nvim-gitsigns
                  nvim-telescope-fzf nvim-dev-webicons nvim-eunuch nvim-which-key
                  nvim-telescope-symbols nvim-lualine nvim-octo nvim-trouble
                  nvim-harpoon nvim-tokyo nvim-git-messenger nvim-hop
                  nvim-merge-tool nvim-truezen nvim-sandwich nvim-snitch
                  nvim-fairy-floss nvim-material nvim-treesitter-refactor
                  nvim-telescope-file-browser nvim-telescope-ui-select
                  nvim-fish-syntax nvim-ranger nvim-libp nvim-pest;
                self = self;
                nixpkgs = nixpkgs;
                pkgs = import nixpkgs {
                  inherit system;
                  config.allowUnfree = true;
                  overlays = [ (import emacs-overlay) ];
                };
                home-manager = home-manager;

              };
              home-manager.users.evie = import ./home-manager/thelxinoe/home.nix;
            }
          ];
      };

      nixosConfigurations."janus" = nixpkgs.lib.nixosSystem {
        system = system;
        modules =
          [
            ./system/janus/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = {
                inherit nvim-visuals-multi nvim-bbye nvim-kommentary
                  nvim-tabular nvim-rainbow nvim-abolish nvim-fugitive
                  nvim-rhubarb nvim-markdown nvim-purescript nvim-nix
                  nvim-signature nvim-better-whitespace nvim-lexical
                  nvim-solarized nvim-plenary nvim-telescope nvim-treesitter
                  nvim-lspconfig nvim-cmp nvim-cmp-path nvim-cmp-vsnip
                  nvim-vim-vsnip nvim-cmp-lsp nvim-lspkind nvim-cmp-emoji
                  nvim-cmp-latex-symbols nvim-cmp-lua nvim-gitsigns
                  nvim-telescope-fzf nvim-dev-webicons nvim-eunuch nvim-which-key
                  nvim-telescope-symbols nvim-lualine nvim-octo nvim-trouble
                  nvim-harpoon nvim-tokyo nvim-git-messenger nvim-hop
                  nvim-merge-tool nvim-truezen nvim-sandwich nvim-snitch
                  nvim-fairy-floss nvim-material nvim-treesitter-refactor
                  nvim-telescope-file-browser nvim-telescope-ui-select
                  nvim-fish-syntax nvim-ranger nvim-libp nvim-pest;
                self = self;
                nixpkgs = nixpkgs;
                pkgs = import nixpkgs {
                  inherit system;
                  config.allowUnfree = true;
                  overlays = [ (import emacs-overlay) ];
                };
                home-manager = home-manager;

              };
              home-manager.users.evie = import ./home-manager/janus/home.nix;
            }
          ];
      };

      nixOnDroidConfigurations.thanatos = nix-on-droid.lib.nixOnDroidConfiguration {
        modules = [
          ./system/thanatos/configuration.nix
          {
  home-manager = {
    config = ./home-manager/thanatos/home.nix;
    useGlobalPkgs = true;
    extraSpecialArgs = {
      inherit nvim-visuals-multi nvim-bbye nvim-kommentary
        nvim-tabular nvim-rainbow nvim-abolish nvim-fugitive
        nvim-rhubarb nvim-markdown nvim-purescript nvim-nix
        nvim-signature nvim-better-whitespace nvim-lexical
        nvim-solarized nvim-plenary nvim-telescope nvim-treesitter
        nvim-lspconfig nvim-cmp nvim-cmp-path nvim-cmp-vsnip
        nvim-vim-vsnip nvim-cmp-lsp nvim-lspkind nvim-cmp-emoji
        nvim-cmp-latex-symbols nvim-cmp-lua nvim-gitsigns
        nvim-telescope-fzf nvim-dev-webicons nvim-eunuch nvim-which-key
        nvim-telescope-symbols nvim-lualine nvim-octo nvim-trouble
        nvim-harpoon nvim-tokyo nvim-git-messenger nvim-hop
        nvim-merge-tool nvim-truezen nvim-sandwich nvim-snitch
        nvim-fairy-floss nvim-material nvim-treesitter-refactor
        nvim-telescope-file-browser nvim-telescope-ui-select
        nvim-fish-syntax nvim-ranger nvim-libp nvim-pest;
      self = self;
      nixpkgs = nixpkgs;
      pkgs = import nixpkgs {
        system = "aarch64-linux";
        config.allowUnfree = true;
        overlays = [ (import emacs-overlay) ];
      };
      home-manager = home-manager;
    };
};
}
 
        ];

            pkgs = import nixpkgs {
          system = "aarch64-linux";

          overlays = [
            nix-on-droid.overlays.default
            (import emacs-overlay)
          ];
        };

      };
    };
}
