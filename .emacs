;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is heavily inspired from @kritzcreek's
;; https://github.com/kRITZCREEK/a-whole-new-world
;; And from Tom Tugel's https://github.com/ttuegel/emacs/blob/master/init.el
;;
;; Requires emacs26
;;
;; For Haskell, see the comments under the section, specifically for 'hhp'
;;
;; For ligatures to work, you will have to also install the hasklig fonts
;; see https://github.com/minad/Hasklig
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Config
(setq inhibit-startup-screen t)
(setq gc-cons-treshold 50000000)
(setq line-number-display-limit-width 10000)
(setq gnutils-min-prime-bits 4096)
(setq tags-revert-without-query t)

;; Force UTF-8
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")
(setq default-buffer-file-coding-system 'utf-8)

;; Disable tool and menu bars
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(setq default-fill-column 81)
(setq fci-rule-column 81)

;; Line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(global-hl-line-mode 1)

; Package management
(require 'package)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))
(package-initialize)

(setq package-list '( ;; evil
                      general
                      evil
                      evil-magit
                      evil-surround
                      evil-escape
                      ;; git
                      magit
                      magithub
                      ; forge
                      ;; project-related stuff
                      projectile
                      powerline
                      company
                      ivy
                      counsel
                      swiper
                      editorconfig
                      counsel-projectile
                      ; flycheck
                      ;; misc
                      ;; visual-fill-column
                      fill-column-indicator
                      diminish
                      which-key
                      rainbow-delimiters
                      ;; org
                      org-plus-contrib
                      org-ref
                      ;; TeX
                      auctex
                      company-auctex
                      ;; Fish
                      fish-mode
                      ;; Markdown
                      markdown-mode
                      ;; PureScript
                      ; psc-ide
                      ; purescript-mode
                      ;; Rust
                      ; cargo
                      ; rust-mode
                      ;; webdev stuff
                      ; restclient
                      ;; test
                      ; yasnippet ;; snippets
                      ; smartparens ;; parens stuff
                      ;; Themes
                      color-theme-sanityinc-tomorrow
                      doom-themes
                      ;; Haskell
                      ;; company-ghci
                      yaml-mode
                      ;; Idris
                      idris-mode
                      ;;
                      use-package ))

; rm -rf ~/.emacs.d/elpa to reload
(when (not package-archive-contents)
(package-refresh-contents))

; install if missing
(dolist (package package-list)
  (when (not (package-installed-p package))
(package-install package)))

(require 'use-package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package general :ensure t
    :config
    (general-evil-setup)
    (setq general-default-keymaps 'evil-normal-state-map)
    (general-define-key :keymaps 'dired-mode-map "SPC" nil)
    (general-define-key :keymaps 'compilation-mode-map "SPC" nil)
    (general-define-key
        :keymaps 'visual
        "SPC ;" 'comment-or-uncomment-region
        "g c c" 'comment-or-uncomment-region)
    (general-define-key
        :keymaps 'normal
        "SPC w s" 'split-window-right
        "SPC w v" 'split-window-below
        "SPC w d" 'kill-this-buffer

        "SPC b d" 'kill-this-buffer
        "SPC b b" 'switch-to-buffer

        "SPC a r" 'align-regexp

        "C-="     'text-scale-increase
        "C--"     'text-scale-decrease
        "C-0"     '(lambda() (interactive) (text-scale-set 1))))

(use-package evil
    :ensure t
    :init
    (progn
        (setq evil-want-abbrev-expand-on-insert-exit nil)
        (setq evil-cant-C-u-scroll t)
        (evil-mode 1)
        (evil-declare-change-repeat 'company-complete)))

(use-package evil-surround
    :ensure t
    :init
    (progn
        (global-evil-surround-mode 1)))

(use-package evil-escape :ensure t
    :diminish evil-escape-mode
    :config
    (evil-escape-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit :ensure t
    :general
    (general-define-key
        :keymaps 'normal
        "SPC g s" 'magit-status
        "SPC g r" 'magit-file-checkout
        "SPC g b" 'magit-blame)
    (general-define-key
        :keymaps 'magit-blame-read-only-mode-map
	"SPC"     nil
        "SPC g b" 'magit-blame-mode)
    :config
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (general-define-key :keymaps 'magit-status-mode-map "SPC" nil)
    (use-package evil-magit :ensure t)
    (setq magit-completing-read-function 'ivy-completing-read))

; (use-package forge :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project-related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile :ensure t
    :general
    (general-define-key
        :keymaps 'normal
        "SPC p f" 'projectile-find-file
        "SPC f f" 'projectile-find-file
        "SPC SPC" 'projectile-find-file)
    :config
    (setq projectile-completion-system 'ivy)
    (projectile-mode 1))

(use-package powerline
    :ensure t
    :config
    (powerline-center-evil-theme))

(use-package company
    :ensure t
    :diminish company-mode
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

(use-package ivy
    :ensure t
    :diminish ivy-mode
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
        "C-k" 'ivy-previous-line))

(use-package counsel :ensure t
    :general
    (general-define-key
        :keymaps 'normal
        "SPC c r" 'counsel-rg
        "SPC h f" 'counsel-describe-function
        "SPC u"   'counsel-unicode-char
        "SPC c g" 'counsel-git
        "SPC c m" 'counsel-M-x))

(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)

(use-package swiper :ensure t
    :general
    (general-define-key
        :keymaps 'normal
        "SPC s" 'swiper))

(require 'editorconfig)
(editorconfig-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish :ensure t
    :config)

(use-package which-key :ensure t
    :diminish which-key-mode
    :config
    (which-key-mode 1))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-ref :ensure t)

(use-package org :ensure t
    :mode (("\\.org$" . org-mode))
    :ensure org-plus-contrib
    :general
    (general-define-key :keymaps 'org-mode-map
                        :states '(normal visual)
			"SPC m a" 'org-agenda
                        "SPC m s" 'org-schedule
                        "SPC m d" 'org-deadline
			"SPC m t" 'org-shiftright
                        "SPC m e" 'org-export-dispatch)
    :init
    (setq org-agenda-files (list "~/code/todo/todo.org"))
    :config
    (use-package evil-org :ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tex-site
    :ensure auctex
    :mode ("\\.tex\\'" . TeX-latex-mode))

(use-package company-auctex
    :defer t
    :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-themes :ensure t
    :preface (defvar region-fg nil)
    :config
    (load-theme 'doom-one t)
    (set-face-attribute 'default nil
			:family "Hasklig"
			:height 100
			:weight 'normal
			:width 'normal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This requires https://github.com/ttuegel/hhp to be at ~/code/install/hhp
; you will need to build and install it as well
; to do so using stack you need to remove 'hhp/tests/data' and run
; > stack init
; you can then restore the folder
; > git checkout .
; > stack build
; > stack install
(use-package hhp 
    :load-path "~/code/install/hhp/elisp" 
    :commands hhp-init hhp-debug)

(use-package haskell-mode :ensure t
    :config
    (setq haskell-literate-default 'tex)
    (setq haskell-process-log t)
    (setq haskell-stylish-on-save t)
    (setq haskell-tags-on-save t)
    ;; (add-hook 'haskell-mode-hook #'company-mode)
    ;; (add-hook 'haskell-mode-hook #'visual-line-mode)
    ;; (add-hook 'haskell-mode-hook #'visual-fill-column-mode)
    (add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'haskell-mode-hook #'display-line-numbers-mode)
    (add-hook 'haskell-mode-hook #'turn-off-eldoc-mode)
    ;; (add-hook 'haskell-mode-hook #'flycheck-mode)
    ;; (add-hook 'haskell-interactive-mode-hook #'company-mode)
    (add-hook 'haskell-mode-hook #'hhp-init)
    (add-hook 'haskell-cabal-mode-hook #'ttuegel/haskell-cabal-mode-hook)
;    (add-hook 'haskel-mode-hook  #'hasklig-mode)
    ;; (mapc (lambda (pair) (push pair prettify-symbols-alist))
    ;; 	  '( ;; ?
    ;; 	    ("\\" . λ)))
    ;; (prettify-symbols-mode t)
    (use-package hasklig-mode
                 :hook (haskell-mode))
    :general
    (general-define-key
        :keymaps 'normal
	"SPC m u" 'turn-on-haskell-unicode-input-method
        "SPC m m" 'hhp-insert-template
        "SPC m b" 'hhp-save-buffer
        "SPC m k" 'hhp-goto-next-error
        "SPC m j" 'hhp-goto-prev-error
        "SPC m e" 'hhp-display-errors
        "SPC m i" 'hhp-show-info
        "SPC m t" 'hhp-show-type
        "SPC m i" 'hhp-import-module))

(use-package company-ghci :ensure t
    :commands company-ghci
    :init
    (with-eval-after-load "company"
    (add-to-list 'company-backends #'company-ghci)))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(general-define-key :keymaps 'evil-motion-state-map "SPC" nil)
(general-define-key :keymaps 'evil-motion-state-map "SPC b d" 'kill-this-buffer)
(general-define-key :keymaps 'help-mode-map "SPC" nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fish
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fish-mode)

; (add-hook 'coq-mode-hook proof-unicode-tokens-enable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "43b219a31db8fddfdc8fdbfdbd97e3d64c09c1c9fdd5dff83f3ffc2ddb8f0ba0" "aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (hasklig-mode proof-general solarized-theme evil-surround blackboard-theme dracula-theme evil ## agda2-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

