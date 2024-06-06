; IMPORTANT: Remember to add `:ensure t` to use-package in order to add them to
;; the available packages.

;;  TODO: this has to be manually run on first install.
;;  (all-the-icons-install-fonts))

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
(menu-bar-mode 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
(setq fill-column 120)
(setq display-fill-column-indicator 120)
;; removed because weird behavior and it's kinda weird anyway
;; (setq display-line-numbers-type 'relative)
;; (setq display-line-numbers-current-absolute t)
(global-display-line-numbers-mode 1)
(global-whitespace-mode)
(setq whitespace-style '(face trailing tabs))

(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq css-indent-offset 2)

;; (global-display-fill-column-indicator-mode)

(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default config
(setq evie-font-size 60)

;; This requires an emacs built with `withGTK3` and `withX`.
(set-frame-parameter nil 'alpha-background 80) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth

;; Load local config (useful for machine-local setups).
(ignore-errors
  (load "~/.emacs.d/locals.el"))

(set-face-attribute 'default nil :family "Hasklug Nerd Font Mono")
(set-face-attribute 'default nil :height evie-font-size)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

(setq org-todo-keyword-faces '(("TODO" . "magenta") ("BACKLOG" . "gray") ("STARTED" . "hot pink") ("WAITING" . "peach puff") ("DELEGATED" . "yellow") ("APPT" . "light green") ("DONE" . "green") ("DEFERRED" . "purple2") ("CANCELLED" . "maroon2") ))

(setq evie-file-init-el "~/code/dotfiles/config/init.el")
(setq evie-file-todo "~/code/personal-org/agenda/todo.org")
(setq evie-file-backlog "~/code/personal-org/agenda/backlog.org")
(setq evie-file-refile "~/code/personal-org/refile.org")

(setq
 send-mail-function 'sendmail-send-it
 sendmail-program "msmtp"
 mail-specify-envelope-from t
 message-sendmail-envelope-from 'header
 mail-envelope-from 'header)

(defun start-term ()
  (interactive)
  (evil-window-vsplit)
  (eshell))

(defun evie-open-file (path)
  "Open file at path."
  (interactive)
  (find-file path))

(defun evie-open-file-other-window (path)
  "Open file at path, in another window."
  (interactive)
  (find-file-other-window path))

(defun goto-def-other-window ()
  (interactive)
  (evil-window-vsplit)
  (lsp-find-definition))

(defun visit-org-roam-index ()
  (interactive)
  (org-roam-node-visit
   (org-roam-node-from-id
    (caar
     (or (org-roam-db-query [:select id :from nodes :where (= title "Index") :limit 1])
         (user-error "No node with title Index"))))))

(defun evie-notmuch-search-toggle-important ()
  "Toggle important tag for message."
  (interactive)
  (evil-collection-notmuch-toggle-tag "important" "search" 'notmuch-tree-next-thread-in-tree))

(defun evie-notmuch-tree-toggle-important ()
  "Toggle important tag for message."
  (interactive)
  (evil-collection-notmuch-toggle-tag "important" "tree" 'notmuch-tree-next-thread-in-tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package config
(require 'use-package)

(use-package
 notmuch
 :ensure t
 :init
 :custom
 (notmuch-fcc-dirs nil)
 (notmuch-always-prompt-for-sender t)
 (notmuch-message-replied-tags '("+replied" "+sent"))
 (notmuch-saved-searches
       '((:name "important" :query "tag:important" :sort-order newest-first :search-type tree)
         (:name "unread" :query "tag:unread" :sort-order newest-first :search-type tree)
         (:name "unsorted" :query "tag:unsorted" :sort-order newest-first :search-type tree)
         (:name "proton" :query "tag:evie" :sort-order newest-first :search-type tree)
         (:name "garnix" :query "tag:garnix" :sort-order newest-first :search-type tree)
         (:name "sent" :query "tag:sent" :sort-order newest-first :search-type tree)
         (:name "hf" :query "tag:hf" :sort-order newest-first :search-type tree)
         (:name "inbox" :query "tag:gmail or tag:evie" :sort-order newest-first :search-type tree)
         (:name "gmail" :query "tag:gmail" :sort-order newest-first :search-type tree))))

;; evil
(use-package
 evil
 :ensure t
 :init
 (setq evil-want-C-u-scroll t)
 (setq evil-want-C-d-scroll t)
 (setq evil-vsplit-window-right t)
 (setq evil-want-integration t)
 (setq evil-want-keybinding nil)
 (evil-mode 1)
 (evil-declare-change-repeat 'company-complete))

(use-package
 evil-collection
 :after evil
 :ensure t
 :config
 (setq evil-collection-want-unimpaired-p nil)
 (evil-collection-init))

;; add: ys<textobject)
;; change: cs
;; delete: ds
(use-package
 evil-surround
 :ensure t
 :init
 (global-evil-surround-mode 1)
 (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
 (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region))

;; rainbow-delimiters doesn't work with use-package
;; but also, I don't think I like smartparens
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook (lambda () (setq fill-column 120)))
(add-hook 'prog-mode-hook (lambda () (column-number-mode)))

;; Display visual hints for some evil commands.
(use-package evil-goggles :ensure t :config (evil-goggles-mode))

;; which-key
(use-package which-key :ensure t :config (which-key-mode 1))

(use-package xclip :ensure t :config (xclip-mode 1))

;; general
(use-package
 general
 :ensure t
 :config (general-evil-setup) (setq general-default-keymaps 'evil-normal-state-map)
 ;; format: off
 (general-define-key
  :states '(normal visual)
  :keymaps 'notmuch-search-mode-map
  "@" 'evie-notmuch-search-toggle-important
  "d" 'evil-collection-notmuch-search-toggle-delete
  "u" 'evil-collection-notmuch-search-toggle-unread
  "t" 'notmuch-search-add-tag
  "T" 'notmuch-search-remove-tag
  "f" 'notmuch-search-filter-by-tag)
 (general-define-key
  :states '(normal visual)
  :keymaps 'notmuch-tree-mode-map
  "@" 'evie-notmuch-tree-toggle-important
  "d" 'evil-collection-notmuch-tree-toggle-delete
  "u" 'evil-collection-notmuch-tree-toggle-unread
  "t" 'notmuch-tree-add-tag
  "T" 'notmuch-tree-remove-tag
  "f" 'notmuch-tree-filter-by-tag)
 (general-define-key
  :keymaps 'dired-mode-map
  "SPC" nil)
 (general-define-key
  :keymaps 'visual
  "g c c" 'comment-or-uncomment-region
  "SPC a r" 'align-regexp)
 (general-define-key
  :keymaps 'normal
  "q" nil
  "g d" 'lsp-find-definition
  "g D" 'goto-def-other-window
  "g r" 'lsp-treemacs-call-hierarchy
  "g c c" 'comment-line
  "SPC n n" 'notmuch
  "SPC b d" '(lambda ()
	       (interactive)
	       (kill-buffer (current-buffer)))
  "SPC f b" 'counsel-ibuffer
  "SPC q" 'save-buffers-kill-terminal
  "SPC a d" 'dired
  "SPC t f" 'display-fill-column-indicator-mode
  "SPC t e" 'ielm
  "SPC w e" 'evil-window-vsplit
  "SPC w s" 'evil-window-split
  "SPC w w" 'save-buffer
  "SPC w q" 'evil-window-delete
  "SPC t E" 'start-term
  "SPC t d" '(lambda ()
                     (interactive)
                     (evie-open-file evie-file-todo))
  "SPC t b" '(lambda ()
                     (interactive)
                     (evie-open-file evie-file-backlog))
  "SPC t l" '(lambda ()
                     (interactive)
                     (evie-open-file evie-local-calendar))
  "C-+" 'text-scale-increase
  "C--" 'text-scale-decrease
  "C-=" '(lambda ()
                 (interactive)
                 (text-scale-set 0))
  "SPC f e d" '(lambda ()
                       (interactive)
                       (evie-open-file evie-file-init-el))
  "SPC o f" 'org-cycle-agenda-files
  "SPC o a" 'org-agenda
  "SPC o C" 'calendar
  "SPC o c" 'org-capture
  "SPC o w" 'org-refile
  "SPC t r" '(lambda ()
                     (interactive)
                     (evie-open-file evie-file-refile))
  "SPC x e" 'eval-last-sexp)
 ;; format: on
 )

;; environment stuff
(use-package direnv :ensure t :config (direnv-mode))

(use-package editorconfig :ensure t :config (editorconfig-mode 1))

;; projectile
(use-package
 projectile
 :ensure t
 :init
 (setq projectile-project-search-path
       '("~/code/"
	 "~/code/blog/"
	 "~/code/experiments/"
	 "~/code/oss/"
         "~/code/lean/"))
 :config (projectile-discover-projects-in-search-path) (projectile-mode +1)
 :general
 ;; format: off
 (general-define-key
  :keymaps 'normal
  "SPC p s" 'projectile-switch-project
  "SPC p f" 'projectile--find-file
  "SPC p b" 'projectile-switch-to-buffer
  "SPC p w" 'projectile-switch-to-buffer-other-frame
  "SPC p q" 'projectile-kill-buffers
  "SPC p i" 'projectile-project-info)
 ;; format: on
 )

;; magit
(use-package
 magit
 :ensure t
 :init (setq magit-completing-read-function 'ivy-completing-read)
 :general
 ;; format: off
 (general-define-key
  :keymaps 'normal
  "SPC g s" 'magit-status
  "SPC g b" 'magit-blame
  "SPC g c" 'magit-blame-cycle-style
  :keymaps 'smerge-mode-map
  ", RET" 'smerge-keep-current
  ", j"   'smerge-next
  ", k"   'smerge-prev
  ", r"   'smerge-resolve
 ;; format: on
 ))

(use-package
 git-gutter
 :ensure t
 :init (global-git-gutter-mode +1)
 :general
 ;; format: off
 (general-define-key
  :keymaps 'normal
  "SPC g j" 'git-gutter:next-hunk
  "SPC g k" 'git-gutter:previous-hunk
  "SPC g w" 'git-gutter:popup-hunk)
 ;; format: on
 )

;; counsel / ivy / swiper
(use-package
 ivy
 :ensure t
 ;; :diminish ivy-mode
 :demand t
 :init
 (setq ivy-use-virtual-buffers t)
 (setq ivy-height 15)
 (setq ivy-count-format "(%d/%d) ")
 (setq ivy-use-selectable-prompt t)
 (setq ivy-initial-inputs-alist nil)
 :config (ivy-mode 1)
 :general
 ;; format: off
 (general-define-key
  :keymaps 'ivy-minibuffer-map
  "C-j" 'ivy-next-line
  "C-k" 'ivy-previous-line
  "C-i" 'ivy-occur
  "C-o" 'ivy-occur
  :keymaps 'ivy-switch-buffer-map
  "C-k" 'ivy-previous-line)
 ;; format: on
 )

;; completion
(use-package
 avy
 :ensure t
 :config
 :general
 ;; format: off
 (general-define-key
  :keymaps 'normal
  "f" 'avy-goto-char-timer
  "F" 'avy-goto-char
  "t" 'avy-goto-line)
 ;; format: on
 )

(use-package
 counsel
 :ensure t
 :general
 ;; format: off
 (general-define-key
  :keymaps 'normal
  "SPC SPC" 'counsel-git
  "SPC c r" 'counsel-rg
  "SPC c e" 'counsel-git-grep
  "SPC c u" 'counsel-unicode-char
  "SPC c f" 'counsel-flycheck
  "SPC c m" 'counsel-evil-marks
  "SPC m" 'counsel-M-x
  :keymaps 'counsel-git-grep-map
  "C-c C-o" 'ivy-occur
  :keymaps 'ivy-occur-grep-mode
  "SPC i e" 'ivy-wgrep-change-to-wgrep-mode
  "SPC i w" 'wgrep-finish-edit
  )
 ;; format: on
 )

(use-package swiper :ensure t :general (general-define-key :keymaps 'normal "/" 'swiper))

(use-package
 undo-tree
 :ensure t
 :config (global-undo-tree-mode)
 ;; format: off
 :general
 (general-define-key
  :keymaps 'normal
  "SPC u t" 'undo-tree-visualize)
 ;; format: on
 )

(use-package
 ace-window
 :ensure t
 :init (setq aw-dispatch-always t) (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
 ;; format: off
 :general
 (general-define-key
  :keymaps 'normal
  "SPC w i" 'ace-window)
 ;; format: on
 )

(use-package elisp-autofmt :ensure t)

;; lsp
(use-package
 lsp-mode
 :ensure t
 :init (setq lsp-enable-folding nil) (setq lsp-enable-file-watchers nil) (setq lsp-enable-symbol-highlighting nil)
 :config
   (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
   (setq lsp-modeline-code-actions-segments '(count icon name))
 :hook ((haskell-mode . lsp) (lsp-mode . lsp-enable-which-key-integration)) (nix-mode . lsp) (rust-mode . lsp) (typescript-ts-mode . lsp)
 :general
 ;; format: off
 (general-define-key
  :keymaps 'normal
  "K" 'lsp-describe-thing-at-point
  ", i" 'interactive-haskell-mode
  ", c" 'lsp-execute-code-action
  ", l" 'haskell-process-load-file)
 ;; format: on
 )

(use-package
  elixir-mode
  :ensure t
  :custom
  (lsp-elixir-server-command '("elixir-ls"))
  )

(use-package lsp-treemacs :ensure t :general (general-define-key :keymaps 'normal ", e" 'lsp-treemacs-errors-list))

(use-package
 lsp-ui
 :ensure t
 :commands lsp-ui-mode
 :init
 (setq lsp-ui-doc-alignment 'window)
 (setq lsp-ui-doc-delay 2)
 (setq lsp-ui-doc-max-height 20)
 (setq lsp-ui-doc-max-width 200)
 :general (general-define-key :keymaps 'normal "SPC a c" 'lsp-execute-code-action))

;; error checking
(use-package
 flycheck
 :ensure t
 :init
 (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
 (setq flycheck-idle-change-delay 3)
 (setq flycheck-standard-error-navigation nil)
 :general
 ;; format: off
 (general-define-key
  :keymaps 'normal
  "SPC x j" 'flycheck-next-error
  "SPC X k" 'flycheck-previous-error
  "SPC e e" 'flycheck-explain-error-at-point
  "SPC e l" 'flycheck-list-errors)
 ;; format: on
 )

;; completion
(use-package
 company
 :ensure t
 :config (add-to-list 'company-backends 'company-capf) (setq company-idle-delay 0.25) (global-company-mode)
 :general
 ;; format: off
 (general-define-key
  :keymaps 'insert
  "C-SPC" 'company-complete
  "C-@" 'company-complete
  :keymaps 'company-active-map
  "<tab>" 'company-complete-selection
  "C-j" 'company-select-next
  "C-k" 'company-select-previous)
 ;; format: on
 )

;; haskell
(use-package haskell-mode :ensure t :hook (before-save . lsp-format-buffer))

;; rust
(use-package rust-mode :ensure t)

(use-package
 lsp-haskell
 :ensure t
 :init
  (setq lsp-haskell-hlint-on t)
  (setq lsp-haskell-max-number-of-problems 100)
  (setq lsp-haskell-diagnostics-on-change nil)
  (setq lsp-haskell-liquid-on nil)
  (setq lsp-haskell-completion-snippets-on t)
  (setq lsp-haskell-formatting-provider "fourmolu")
  (setq lsp-haskell-tactic-on t)
 :general
  ;; format: off
  (general-define-key
   :keymaps 'normal
   "SPC f o" '(lambda ()
		      (interactive)
                      (setq lsp-haskell-formatting-provider "ormolu"))
   "SPC f f" '(lambda ()
		      (interactive)
                      (setq lsp-haskell-formatting-provider "formolu")))
  ;; format: on
 )


(use-package yuck-mode :ensure t)

; purescript
(use-package purescript-mode :ensure t :diminish 'purescript-indentation-mode)

(defun kc/purescript-hook ()
  "My PureScript mode hook"
  (turn-on-purescript-indentation)
  (psc-ide-mode)
  (company-mode)
  (flycheck-mode))

(use-package
 psc-ide
 :ensure t
 :init (add-hook 'purescript-mode-hook 'kc/purescript-hook)
 :general
 ;; format: off
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
  ", q f" 'psc-ide-flycheck-insert-suggestion)
 ;; format: on
 )

;; nix
(use-package
 nix-mode
 :ensure t
 :mode "\\.nix\\'"
 :general
 (general-define-key
  :keymaps 'nix-mode-map
  :states '(normal visual)
  ", u" 'nix-unpack ; this seems bugged; should investigate
  ", b" 'nix-build))

(use-package
 lsp-nix
 :ensure lsp-mode
 :after (lsp-mode)
 :demand t
 :custom
 (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

;; lua
(use-package lua-mode :ensure t :init (setq lua-indent-level 4) (setq lua-indent-string-contents t))

(use-package yuck-mode
  :ensure t
  :mode ("\\.yuck" . yuck-mode)
  )

;; dhall
(use-package dhall-mode :ensure t :mode "\\.dhall\\'")

;; yaml
(use-package
 yaml-mode
 :ensure t
 :mode (("\\.yml$" . yaml-mode) ("\\.yaml$" . yaml-mode) ("\\.yml\\.example$" . yaml-mode)))

;; markdown
(use-package
 markdown-mode
 :ensure t
 :mode (("README\\.md\\'" . gfm-mode) ("\\.md\\'" . markdown-mode) ("\\.markdown\\'" . markdown-mode))
 :init (setq markdown-fontify-code-blocks-natively t)
 :general (general-define-key :keymaps 'normal "C-SPC" 'markdown-toggle-gfm-checkbox))

;; pdf/tex
(use-package pdf-tools :ensure t :init (pdf-tools-install))

(use-package
 lsp-latex
 :ensure t
 :config
 (with-eval-after-load "tex-mode"
   (add-hook 'tex-mode-hook 'lsp)
   (add-hook 'latex-mode-hook 'lsp)))

;; org
(setq org-edit-src-content-indentation nil)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)
(setq org-file-apps '((auto-mode . emacs) ("\\.jpg\\'" . "feh %s") ("\\.jpeg\\'" . "feh %s") ("\\.png\\'" . "feh %s")))

(use-package
 evil-org
 :ensure t
 :after org
 :hook ((org-mode . evil-org-mode) (org-agenda . evil-org-mode) (org-mode . auto-fill-mode))
 :config
 ;; this has a bug: it runs after the buffer is created, so on first run, it will not have the correct keybindings
 (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading calendar))
 (require 'evil-org-agenda)
 (evil-org-agenda-set-keys)
 :general
 ;; format: off
 (general-define-key
  :states 'normal
  :keymaps 'org-mode-map
  "RET" 'org-open-at-point
  "C-SPC" 'org-toggle-checkbox
  "C-@" 'org-toggle-checkbox
  "SPC o s" 'org-schedule
  "SPC o d" 'org-deadline
  "SPC o l" 'org-insert-link
  "SPC o t" 'org-set-tags-command)
 ;; format: on
 )

(use-package org-bullets :ensure t :hook (org-mode . org-bullets-mode))

(use-package
 org-roam
 :ensure t
 :init
 (setq org-roam-directory "~/code/personal-org/roam")
 (setq org-roam-dailies-directory "daily/") ;; relative to roam-directory
 (setq org-roam-index-file "~/code/personal-org/roam/index.org")
 (setq org-roam-update-method 'idle-timer)
 (setq org-roam-db-update-idle-seconds 5)
 :config
 (org-roam-db-autosync-mode)
 (require 'org-roam-export)
 :hook (after-init . org-roam-setup)
 :general
 ;; format: off
 (general-define-key
  :keymaps 'normal
  "SPC r f" 'org-roam-node-find
  "SPC r i" 'org-roam-node-insert
  "SPC r b" 'org-roam-backlinks-get
  "SPC r g" 'org-roam-graph
  "SPC r I" (lambda ()
                    (interactive)
                    (visit-org-roam-index)))
 ;; format: on
 )

(use-package
 org-roam-ui
 :ensure t
 :config
 (setq
  org-roam-ui-sync-theme t
  org-roam-ui-follow t
  org-roam-ui-update-on-save t
  org-roam-ui-open-on-start t))

(use-package
 org-tree-slide
 :ensure t
 :init (setq org-tree-slide-header nil)
 :config
 (add-hook
  'org-tree-slide-play-hook
  (lambda ()
    (text-scale-increase 4)
    (org-redisplay-inline-images)))
 (add-hook 'org-tree-slide-stop-hook (lambda () (text-scale-set 0)))
 :general
 ;; format: off
 (general-define-key
  :keymaps 'org-tree-slide-mode-map
  :state '(normal visual)
  "C-<right>" 'org-tree-slide-move-next-tree
  "C-<left>" 'org-tree-slide-move-previous-tree
  :state '(normal visual)
  "SPC t p" 'org-tree-slide-mode)
 ;; format: on
 )

;; term
(use-package vterm :ensure t :general (general-define-key :keymaps 'normal "SPC t t" 'vterm-other-window))

;; indent guide
(use-package indent-guide :ensure t :init (setq indent-guide-char ".") :config (indent-guide-global-mode))

;; colors
(use-package rainbow-mode :ensure t :general (general-define-key :keymaps 'normal "SPC r m" 'rainbow-mode))

;; theme -- has a bug and fails
;; (use-package doom-themes
;;   :ensure t
;;   :init
;;     (setq doom-challenger-deep-brighter-comments t)
;;     (setq doom-challenger-deep-brighter-modeline t)
;;     (setq doom-challenger-deep-padded-modeline t)
;;     (setq doom-challenger-deep-comment-bg nil)
;;   :config (load-theme 'doom-dracula t))

(use-package
 modus-themes
 :ensure t
 :config
 (setq modus-vivendi-tritanopia-palette-overrides
       '((fg-prompt magenta-warmer)
	 (builtin magenta)
	 (constant blue-warmer)
	 (fnname magenta-cooler)
	 (keyword magenta-warmer)
	 (preprocessor red-cooler)
	 (type cyan-cooler)
	 (variable cyan)
	 (rx-construct blue-cooler)
	 (rx-backslash red-warmer)
	 (name blue-warmer)
	 (identifier magenta)
	 (keybind magenta-warmer)
	 (accent-0 magenta-warmer)
	 (accent-1 cyan)
	 (accent-2 blue-warmer)
	 (accent-3 red-cooler)
	 (date-common cyan-cooler)
	 (date-holiday magenta-warmer)
	 (mail-cite-0 magenta-faint)
	 (mail-cite-1 cyan-cooler)
	 (mail-cite-2 green-warmer)
	 (mail-cite-3 red-faint)
	 (mail-part cyan)
	 (mail-recipient magenta)
	 (mail-subject blue-warmer)
	 (mail-other magenta-warmer)
	 (fg-prose-macro red-cooler)
	 (prose-tag fg-dim)
	 (cursor "#822bd8")
	))
 (setq modus-operandi-tritanopia-palette-overrides
       '(
	 (fnname magenta-warmer)
	 (cursor "#022bd8")
	 ))

 (setq modus-themes-italic-constructs t
       modus-theme-bold-constructs t
       )
 (load-theme 'modus-vivendi-tritanopia :no-confirm)
 )

(use-package all-the-icons :ensure t)

(use-package doom-modeline :ensure t :init (doom-modeline-mode 1))

(use-package
 ranger
 :ensure t
 :init
 (setq helm-descbinds-window-style 'same-window)
 (setq ranger-cleanup-on-disable t)
 (setq ranger-cleanup-eagerly t)
 (setq ranger-show-hidden t)
 (setq ranger-modify-header t)
 (setq ranger-preview-file t)
 :config (ranger-override-dired-mode t)
 :general (general-define-key :keymaps 'normal "SPC r a" 'ranger))

(use-package wgrep :ensure t)

(use-package
 anzu
 :ensure t
 :init (global-anzu-mode +1)
 :general
 ;; format: off
 (general-define-key
  :keymaps 'normal
  "SPC e r" 'anzu-isearch-query-replace-regexp
  "SPC e c" 'anzu-query-replace-at-cursor
  "SPC e i" 'anzu-isearch-query-replace)
 ;; format: on
 )

(setq enable-local-variables :safe)

(setq evie-local-calendar "~/code/personal-org/cal-sync/local.org")

(setq evie-todo-files
     '("~/code/personal-org/cal-sync/local.org"
       "~/code/personal-org/agenda/todo.org"
       "~/code/personal-org/agenda/backlog.org"))

(setq org-agenda-files
    (append
      '("~/code/personal-org/cal-sync/evie.org"
        "~/code/personal-org/cal-sync/garnix.org"
        "~/code/personal-org/cal-sync/gia-evie.org"
        "~/code/personal-org/cal-sync/proton.org")
      evie-todo-files))

(setq org-directory "~/code/personal-org")
(setq org-default-notes-file "~/code/personal-org/refile.org")

(custom-set-variables
 '(org-agenda-ndays 30)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t))

(setq org-refile-targets (quote ((nil :maxlevel . 9) (evie-todo-files :maxlevel . 2))))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq
 org-capture-templates
 (quote
  (("t" "todo" entry (file "~/code/personal-org/refile.org") "* TODO %?\n")
   ("c" "calendar" entry (file+headline "~/code/personal-org/cal-sync/local.org" "Unsorted") "** %^{title} :local:\n\
  %^{start}T--%^{end}T\n\
  :PROPERTIES:\n\
  :UID:                            -\n\
  :Class:                          Public\n\
  :Location:                       \n\
  :Organizer:                      Evie\n\
  :Sequence:                       1\n\
  :Status:                         Confirmed\n\
  :Transparency:                   Opaque\n\
  :END:\n\
%?\n")
  )))

(use-package htmlize :ensure t)

(require 'lean4-mode)

(use-package
 sops
 :ensure t
 :general
 ;; format: off
 (general-define-key
  :keymaps 'normal
    ", s" 'sops-save-file
    ", c" 'sops-cancel
    ", e" 'sops-edit-file)
 ;; format: on
 :init
   (global-sops-mode 1))

(use-package
 just-mode
 :ensure t)

(use-package
 ligature
 :ensure t
 :config
 (ligature-set-ligatures
  't
  '("<*"
    "<*>"
    "<+>"
    "<$>"
    "***"
    "<|"
    "|>"
    "<|>"
    "!!"
    "||"
    "==="
    "==>"
    "<<<"
    ">>>"
    "<>"
    "+++"
    "<-"
    "->"
    "=>"
    ">>"
    "<<"
    ">>="
    "=<<"
    ".."
    "..."
    "::"
    "-<"
    ">-"
    "-<<"
    ">>-"
    "++"
    "/="
    "=="))
 (global-ligature-mode t))

;; experiments

(use-package
  telephone-line
 :ensure t
 :init
 (setq telephone-line-lhs
    '((evil   . (telephone-line-evil-tag-segment))
            (accent . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment
                       telephone-line-process-segment))
            (nil    . (telephone-line-minor-mode-segment
                       telephone-line-buffer-segment))))
 (setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))
 (telephone-line-mode 1))

(use-package rainbow-mode
  :ensure t
  :init
  (rainbow-mode 1))
