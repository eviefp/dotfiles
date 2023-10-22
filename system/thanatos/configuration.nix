{ config
, lib
, pkgs
, nixpkgs
, self
, home-manager
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
, ...
}:
{
  system.stateVersion = "23.11";

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  home-manager = {
    config = ../../home-manager/thanatos/home.nix;
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
