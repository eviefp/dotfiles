;; IMPORTANT: Remember to add `:ensure t` to use-package in order to add them to
;; the available packages.

;; Not sure what this does. This bit was copy/pasted from the NixOS manual.
(require 'package)

;; optional. makes unpure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shamelessly stolen from Christoph Hegeman's emacs config
;; https://github.com/kritzcreek/a-whole-new-world/blob/master/init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic config stuff
(setq debug-on-errror t)
(setq inhibit-startup-screen t)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
(setq fill-column 80)
(global-display-line-numbers-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default config
(setq cvlad-font-size 60)


;; Load config
(ignore-errors (load "~/.emacs.d/locals.el"))

(set-face-attribute 'default nil :family "Hasklug Nerd Font")
(set-face-attribute 'default nil :height cvlad-font-size)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package config
(require 'use-package)

;; This is broken, no idea why. Try again later.
; ; email
; (use-package notmuch
;     :ensure t)

;; evil
(use-package evil
    :ensure t
    :init
    (progn
        (setq evil-want-C-u-scroll t)
        (setq evil-vsplit-window-right t)
        (setq evil-want-integration t)
        (setq evil-want-keybinding nil)
        (evil-mode 1)
        (evil-declare-change-repeat 'company-complete)))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :init
  (progn
    (global-evil-surround-mode 1)
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
    (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)))

;; rainbow-delimiters doesn't work with use-package
;; but also, I don't think I like smartparens
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode))

;; which-key
(use-package which-key
    :ensure t
    :config
      (which-key-mode 1))

(defun start-term ()
  (interactive)
  (evil-window-vsplit)
  (eshell))

;; general
(use-package general :ensure t
    :config
    (general-evil-setup)
    (setq general-default-keymaps 'evil-normal-state-map)
    ; unbind space from dired map to allow for git status
    ; (general-define-key :keymaps 'dired-mode-map "SPC" nil)
    (general-define-key
     :keymaps 'visual
     "g c c"   'comment-or-uncomment-region
     "SPC a r" 'align-regexp)
    (general-define-key
     :keymaps 'normal
     "SPC b d" 'kill-this-buffer
     "SPC b b" 'switch-to-buffer
     "SPC q"   'save-buffers-kill-terminal
     "SPC a d" 'dired
     "SPC TAB" 'switch-to-previous-buffer
     "SPC t f" 'display-fill-column-indicator-mode
     "SPC w s" 'evil-window-vsplit
     "SPC t t" 'start-term
     "C-+" 'text-scale-increase
     "C--" 'text-scale-decrease
     "C-=" '(lambda () (interactive) (text-scale-set 0))))

;; environment stuff
(use-package direnv
    :ensure t
    :config (direnv-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;;; projectile
;;(use-package projectile
;;  :ensure t
;;  :init
;;      (projectile-mode +1)
;;  :config
;;      (setq projectile-project-search-path '("~/code/"))
;;   :general
;;   (general-define-key
;;    :keymaps 'normal
;;      "SPC p s" 'projectile-switch-project))

;; magit
(use-package magit
   :ensure t
   :general
   (general-define-key
    :keymaps 'normal
    "SPC g s" 'magit-status)
   :config
   (setq magit-completing-read-function 'ivy-completing-read))

(use-package git-gutter
    :ensure t
    :init
      (global-git-gutter-mode +1)
    :general
      (general-define-ke
        :keymaps 'normal
          "SPC g n" 'git-gutter:next-hunk
          "SPC g p" 'git-gutter:previous-hunk
          "SPC g w" 'git-gutter:popup-hunk))

;; counsel / ivy / swiper
(use-package ivy
    :ensure t
    ;; :diminish ivy-mode
    :demand t
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 15)
    (setq ivy-count-format "(%d/%d) ")
    :general
    (general-define-key
     :keymaps 'ivy-minibuffer-map
     "C-j" 'ivy-next-line
     "C-k" 'ivy-previous-line)
    (general-define-key
     :keymaps 'ivy-switch-buffer-map
     "C-k" 'ivy-previous-line))

(use-package counsel :ensure t
    :general
    (general-define-key
     :keymaps 'normal
     "SPC f f" 'counsel-find-file
     "SPC SPC" 'counsel-git
     "SPC c r" 'counsel-rg
     "SPC m" 'counsel-M-x))

(use-package swiper
    :ensure t
    :general
    (general-define-key
        :keymaps 'normal
        "SPC s" 'swiper))

;; lsp
(use-package lsp-mode
    :ensure t
    :hook ((haskell-mode . lsp)
           (lsp-mode . lsp-enable-which-key-integration))
    :general
    (general-define-key
     :keymaps 'normal
     "K" 'lsp-describe-thing-at-point))
     ;; 'lsp-organize-imports
     ;; 'lsp-format-buffer

(use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :general
    (general-define-key
     :keymaps 'normal
     "SPC a c" 'lsp-ui-sideline-apply-code-actions))

;; error checking
(use-package flycheck
    :ensure t
    :general
    (general-define-key
     :keymaps 'normal
     "[ g" 'flycheck-next-error
     "] g" 'flycheck-previous-error))

;; completion
(use-package company
    :ensure t
    ;; :diminish company-mode
    :config
    (global-company-mode)
    (setq company-idle-delay 0.25)
    :general
    (general-define-key
     :keymaps 'insert
     "C-SPC" 'company-complete)
    (general-define-key
     :keymaps 'company-active-map
     "<tab>" 'company-complete-selection
     "C-j" 'company-select-next
     "C-k" 'company-select-previous))

;; haskell
(use-package haskell-mode
      :ensure t)

(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-hlint-on t)
  (setq lsp-haskell-max-number-of-problems 100)
  (setq lsp-haskell-diagnostics-on-change t)
  (setq lsp-haskell-liquid-on nil)
  (setq lsp-haskell-completion-snippets-on t)
  (setq lsp-haskell-format-on-import-on t)
  (setq lsp-haskell-formatting-provider "stylish-haskell")
  )

;; purescript
(use-package purescript-mode
  :ensure t
  :diminish 'purescript-indentation-mode)

(defun kc/purescript-hook ()
  "My PureScript mode hook"
  (turn-on-purescript-indentation)
  (psc-ide-mode)
  (company-mode)
  (flycheck-mode))
  ;; (setq-local flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package psc-ide
  :ensure t
  :init (add-hook 'purescript-mode-hook 'kc/purescript-hook)
  :general
  (general-define-key
   :keymaps 'purescript-mode-map
   :states '(normal visual)
   ", s" 'psc-ide-server-start
   ", l" 'psc-ide-load-all
   ", Q" 'psc-ide-server-quit
   ", t" 'psc-ide-show-type
   ", b" 'psc-ide-rebuild
   ", g g" 'psc-ide-goto-definition
   ", a i" 'psc-ide-add-import
   ", q f" 'psc-ide-flycheck-insert-suggestion))

;; nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :general
  (general-define-key
    :keymaps 'nix-mode-map
    :states '(normal visual)
      ", u" 'nix-unpack ; this seems bugged; should investigate
      ", b" 'nix-build))

;; dhall
(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")

;; yaml
(use-package yaml-mode
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)
         ("\\.yml\\.example$" . yaml-mode)))

;; markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :general
  (general-define-key
   :keymaps 'normal
   "C-SPC" 'markdown-toggle-gfm-checkbox))

;; pdf/tex
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))
  ;; TODO: evil keybindings

;; neotree
(use-package neotree
    :ensure t
    :general
    (general-define-key
      :keymaps 'normal
      "SPC n t" 'neotree-toggle)
    (general-define-key
      :keymaps 'neotree-mode-map
      "RET" 'neotree-quick-look
      "k"   'neotree-previous-line
      "j"   'neotree-next-line
      "d"   'neotree-delete-node
      "r"   'neotree-rename-node
      "c"   'neotree-copy-node
      "R"   'neotree-refresh
      "n"   'neotree-create-node))


;; theme
(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-one t))

(use-package all-the-icons
  :ensure t)
;;  :init ;; this only needs to happen once
;;  (all-the-icons-install-fonts))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(global-display-fill-column-indicator-mode)
