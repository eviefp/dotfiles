{ pkgs }:

let
  # TODO: try updating nixpkgs to get emacs27
  # TODO: use display-fill-column-indicator-mode w/ e27
  file = {
    ".emacs.d/init.el".source = ./init.el;

  };
  emacsWithPackages = pkgs.emacs26WithPackages;
  deriv = emacsWithPackages (epkgs: (with epkgs; [
    # TODO: sort this
    doom-themes
    company
    ivy
    counsel
    swiper
    direnv
    dhall-mode
    editorconfig
    evil
    evil-goggles
    evil-surround
    evil-collection # keymaps more stuff to evil
    flycheck
    general
    git-gutter
    haskell-mode
    hl-todo
    magit
    markdown-mode
    neotree
    nix-mode
    pdf-tools
    doom-modeline
    all-the-icons
    rainbow-delimiters
    purescript-mode
    psc-ide
    use-package
    which-key
    yaml-mode
    notmuch

    # ## Haskell
    lsp-mode
    lsp-ui
    lsp-haskell

    ## org stuff
    # evil-org
    # org-bullets

    # projectile # maybe?

    ## later
    # yasnippet
    # ws-butler # websockets I think
    # ranger
    # edit-indirect # galaxy brain figure it out later
  ]));
in
  {
    derivation = deriv;
    file = file;
  }
