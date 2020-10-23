{ pkgs }:

let
  myEmacs = pkgs.emacs26;

in
  pkgs.emacsWithPackages (epkgs: (with epkgs; [
    aggressive-indent
    # cask-mode
    company
    company-emoji
    company-lsp
    company-quickhelp
    counsel
    counsel-projectile
    csv-mode
    # delight
    direnv
    dhall-mode
    # dockerfile-mode
    # edit-indirect
    editorconfig emojify
    evil
    evil-collection
    evil-goggles
    evil-org
    evil-magit
    evil-smartparens
    fill-column-indicator
    flow-minor-mode
    flx
    flycheck
    flycheck-haskell
    flycheck-pos-tip
    general
    git-gutter
    # haskell-mode
    helpful
    highlight-numbers
    hl-todo
    imenu-list
    indent-guide
    ivy
    link-hint
    lsp-mode
    lsp-ui
    magit
    markdown-mode
    mmm-mode
    mode-icons
    neotree
    nix-mode
    org-bullets
    pdf-tools
    # pkgbuild-mode
    powerline
    powerline-evil
    projectile
    psc-ide
    purescript-mode
    rainbow-delimiters
    rainbow-mode
    ranger
    # rust-mode
    smartparens
    solarized-theme
    swiper
    # typescript-mode
    unicode-fonts
    use-package
    # web-mode
    which-key
    ws-butler
    yaml-mode
    # zoom-frm
    ## Scala
    scala-mode
    sbt-mode
    lsp-metals
    lsp-ui
    yasnippet
    posframe
    dap-mode

    ## Haskell
    lsp-haskell
  ]))
