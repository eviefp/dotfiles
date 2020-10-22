{ pkgs ? import <nixpkgs> {} }:

let
  emacs = pkgs.emacs26;
  emacsWithPackages = (pkgs.emacsPackagesGen emacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    evil
    evil-commentary
    evil-escape
    evil-leader
    evil-magit
    evil-org
    evil-smartparens
    evil-surround
    evil-visual-mark-mode
    ivy
    ivy-bibtex
    ivy-dired-history
    counsel
    swiper
    which-key
    company
    company-ctags
    company-coq
    company-shell
    company-stan
    diminish
    hl-todo
    doom-themes
    all-the-icons
    smartparens
    doom-modeline
    flycheck
    magit
    magithub
    magit-todos
    magit-gh-pulls
    fill-column-indicator
  ]) ++ (with epkgs.elpaPackages; [
    general
    ivy-todo
    purescript-mode
    psc-ide
    evil-replace
  ]))
