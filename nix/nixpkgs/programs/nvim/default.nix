## Under construction

# Based on [Ali Abrar](https://github.com/ali-abrar)'s Vim configuration.

{ pkgs ? import <nixpkgs> {}, fetchGH, ... }:

let
  # cf. https://nixos.wiki/wiki/Vim#Adding_new_plugins

  customPlugins = {
    vim-bbye = pkgs.vimUtils.buildVimPlugin {
      name = "vim-bbye";
      src = fetchGH "moll/vim-bbye" "25ef93a";
    };
    custom-haskell = pkgs.vimUtils.buildVimPlugin {
      name = "custom-haskell";
      src = fetchGH "vladciobanu/haskell-vim" "a00f7ec";
    };

  };
in
  with pkgs; neovim.override {
    configure = {
      # Builtin packaging
      # List of plugins: nix-env -qaP -A nixos.vimPlugins
      packages.myVimPackage = with pkgs.vimPlugins; {
        # Loaded on launch
        start = [ ];
        # Manually loadable by calling `:packadd $plugin-name
        opt = [ ];
      };

      # VAM
      vam.knownPlugins = pkgs.vimPlugins // customPlugins;
      vam.pluginDictionaries = [
        { name = "vim-bbye"; }
        { name = "custom-haskell"; }
        { name = "editorconfig-vim"; }
        { name = "nerdtree"; }
        { name = "nerdtree-git-plugin"; }
        { name = "vim-devicons"; }
        { name = "vim-airline"; }
        { name = "vim-airline-themes"; }
        { name = "fzf-vim"; }
        { name = "vim-multiple-cursors"; }
        { name = "vim-highlightedyank"; }
        { name = "vim-commentary"; }
        { name = "tabular"; }
        { name = "vim-surround"; }
        { name = "rainbow"; }
        { name = "vim-indent-guides"; }
        { name = "vim-abolish"; }
        { name = "vim-fugitive"; }
        { name = "vim-gitgutter"; }
        { name = "coc-nvim"; }
        { name = "vim-markdown"; }
        { name = "vim-stylish-haskell"; }
        { name = "purescript-vim"; }
        { name = "vim-nix"; }
        { name = "dhall-vim"; }
        { name = "molokai"; }
        { name = "vim-trailing-whitespace"; }
        { name = "vim-signature"; }
        { name = "vimwiki"; }
      ];

      customRC = builtins.readFile ./config.vim;
    };
  }

