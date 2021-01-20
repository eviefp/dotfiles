;; IMPORTANT: Remember to add `:ensure t` to use-package in order to add them to
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
(use-package notmuch
  :ensure t
  :init
  (setq send-mail-function 'sendmail-send-it)
  (setq notmuch-saved-searches
    '((:name "unread"
       :query "tag:unread"
       :sort-order newest-first)
      (:name "keep"
       :query "tag:keep"
       :sort-order newest-first)
      (:name "Hasura"
       :query "tag:hasura OR path:hasura/**"
       :sort-order newest-first)
      (:name "Statically Typed"
       :query "tag:st"
       :sort-order newest-first)
      (:name "sent"
       :query "tag:sent"
       :sort-order newest-first)
      (:name "inbox"
       :query "tag:inbox"
       :sort-order newest-first))))

  ;; :general
  ;; (general-define-key
  ;;  :states '(normal visual)
  ;;  :keymaps 'notmuch-search-mode-map
  ;;  "RET" 'notmuch-search-show-thread))

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
  :after (evil notmuch)
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :init
  (progn
    (global-evil-surround-mode 1)
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
    (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)))

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

(defun cvlad-open-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window "~/code/dotfiles/home-manager/init.el"))

(defun cvlad-open-org-refile-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window "~/Documents/wiki/refile.org"))

;; general
(use-package general :ensure t
    :config
    (general-evil-setup)
    (setq general-default-keymaps 'evil-normal-state-map)
    ; unbind space from dired map to allow for git status
    ;; (general-define-key
    ;;  :states '(normal visual)
    ;;  :keymaps 'notmuch-search-mode-map
    ;;  "RET" 'notmuch-search-show-thread
    ;;  "d" 'evil-collection-notmuch-search-toggle-delete
    ;;  "u" 'evil-collection-notmuch-search-toggle-unread
    ;;  "t" 'notmuch-search-add-tag
    ;;  "T" 'notmuch-search-remove-tag
    ;;  "f" 'notmuch-search-filter-by-tag)
    (general-define-key :keymaps 'dired-mode-map "SPC" nil)
    (general-define-key
     :keymaps 'visual
     "g c c"   'comment-or-uncomment-region
     "SPC a r" 'align-regexp)
    (general-define-key
     :keymaps 'normal
     "q" nil
     "g c c"   'comment-line
     "SPC n n"   'notmuch
     "SPC b d" 'kill-this-buffer
     "SPC b b" 'switch-to-buffer
     "SPC q"   'save-buffers-kill-terminal
     "SPC a d" 'dired
     "SPC TAB" 'switch-to-previous-buffer
     "SPC t f" 'display-fill-column-indicator-mode
     "SPC t e" 'ielm
     "SPC w s" 'evil-window-vsplit
     "SPC w w" 'windmove-swap-states-left
     "SPC t E" 'start-term
     "C-+" 'text-scale-increase
     "C--" 'text-scale-decrease
     "C-=" '(lambda () (interactive) (text-scale-set 0))
     "SPC f e d" 'cvlad-open-user-init-file
     "SPC o f" 'org-cycle-agenda-files
     "SPC o o" 'org-todo
     "SPC o a" 'org-agenda
     "SPC o c" 'calendar
     "SPC o C" 'org-capture 
     "SPC o w" 'org-refile
     "SPC o r" 'cvlad-open-org-refile-file
     ))
;; environment stuff
(use-package direnv
    :ensure t
    :config (direnv-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;;; projectile
(use-package projectile
 :ensure t
 :init
     (setq projectile-project-search-path '("~/code/" "~/Documents/" "~/code/hasura/" "~/code/tests/" "~/code/hasura/mono/"))
 :config
     (projectile-discover-projects-in-search-path)
     (projectile-mode +1)
 :general
  (general-define-key
   :keymaps 'normal
   "SPC p s" 'projectile-switch-project
   "SPC p f" 'projectile--find-file
   "SPC p r" 'projectile-ripgrep
   "SPC p d" 'projectile-dired-other-frame
   "SPC p b" 'projectile-switch-to-buffer
   "SPC p w" 'projectile-switch-to-buffer-other-frame
   "SPC p q" 'projectile-kill-buffers
   "SPC p i" 'projectile-project-info))


;; magit
(use-package magit
   :ensure t
   :general
   (general-define-key
    :keymaps 'normal
    "SPC g s" 'magit-status
    "SPC g b" 'magit-blame
    "SPC g c" 'magit-blame-cycle-style
    )
   :init
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
    :init
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 15)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-use-selectable-prompt t)
    :config
    (ivy-mode 1)
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
    :init
      (setq lsp-enable-folding nil)
      (setq lsp-enable-file-watchers nil)
      (setq lsp-enable-symbol-highlighting nil)
      (setq lsp-log-io nil)
      (setq lsp-modeline-code-actions-face '((t nil)))
      (setq lsp-modeline-code-actions-segments '(icon name))
      (setq lsp-modeline-diagnostics-enable nil)
    :hook ((haskell-mode . lsp)
           (lsp-mode . lsp-enable-which-key-integration))
    :general
    (general-define-key
     :keymaps 'normal
     "K" 'lsp-describe-thing-at-point
     ", i" 'interactive-haskell-mode
     ", c" 'lsp-execute-code-action
     ", l" 'haskell-process-load-file))
     ;; 'lsp-organize-imports
     ;; 'lsp-format-buffer

(use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :init
      (setq lsp-ui-doc-alignment 'window)
      (setq lsp-ui-doc-delay 2)
      (setq lsp-ui-doc-max-height 20)
      (setq lsp-ui-doc-max-width 200)
    :general
    (general-define-key
     :keymaps 'normal
     "SPC a c" 'lsp-ui-sideline-apply-code-actions))

;; error checking
(use-package flycheck
    :ensure t
    :init
      (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
      (setq flycheck-idle-change-delay 3)
      (setq flycheck-standard-error-navigation nil)
    :general
    (general-define-key
     :keymaps 'normal
     "[ g" 'flycheck-next-error
     "] g" 'flycheck-previous-error
     "SPC e e" 'flycheck-explain-error-at-point
     "SPC e l" 'flycheck-list-errors))

;; completion
(use-package company
    :ensure t
    :config
    (add-to-list 'company-backends 'company-capf)
    (setq company-idle-delay 0.25)
    (global-company-mode)
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
  :ensure t
  :init
    (setq haskell-stylish-on-save nil))

(use-package lsp-haskell
  :ensure t
  :init
  (setq lsp-haskell-hlint-on t)
  (setq lsp-haskell-max-number-of-problems 100)
  (setq lsp-haskell-diagnostics-on-change nil)
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
  :ensure t
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

;; org
(setq org-edit-src-content-indentation nil)

(use-package evil-org
  :ensure t
  :after (evil org)
  :hook ((org-mode . evil-org-mode)
         (org-agenda . evil-org-mode))
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :general
  (general-define-key
  :states  'normal
  :keymaps 'org-mode-map
   "RET"     'org-open-at-point
   "C-SPC"   'org-toggle-checkbox
   "SPC o s" 'org-schedule
   "SPC o d" 'org-deadline
   "SPC o t" 'org-set-tags-command))
   ;; :keymaps 'visual
   ;; ">"       'org-demote-subtree
   ;; "<"       'org-promote-subtree))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :ensure t
  :init
    (setq org-roam-capture-templates
	  '(("d" "default" plain
	     (function org-roam--capture-get-point)
	     "%?"
	     :file-name "%<%Y%m%d%H%M%S>-${slug}"
	     :head "#+title: ${title}\n#+created: %U\n#last_modified: %U\n#+roam_alias:\n#+roam_tags:\n#+roam_key:\n\n"
	     :unnarrowed t)))
    (setq org-roam-directory "~/Documents/wiki/roam")
    (setq org-roam-index-file "~/Documents/wiki/roam/index.org")
    (setq org-roam-update-method 'idle-timer)
    (setq org-roam-db-update-idle-seconds 5)
  :config (org-roam-db-build-cache)
  :hook (after-init . org-roam-mode)
  :general
  (general-define-key
   :keymaps 'normal
   "SPC r f" 'org-roam-find-file
   "SPC r d" 'org-roam-db-build-cache
   "SPC r i" 'org-roam-insert
   "SPC r r" 'org-roam
   "SPC r g" 'org-roam-graph
   "SPC r I" 'org-roam-jump-to-index))

;; (add-hook 'after-init-hook 'org-roam-mode)

;; term
(use-package vterm
  :ensure t
  :general
  (general-define-key
   :keymaps 'normal
   "SPC t t" 'vterm-other-window))

;; neotree
(use-package neotree
    :ensure t
    :general
    (general-define-key
      :keymaps 'normal
      "SPC t n" 'neotree-toggle)
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

;; colors
(use-package rainbow-mode
  :ensure t
  :general
  (general-define-key
   :keymaps 'normal
   "SPC c m" 'rainbow-mode))

(use-package zenity-color-picker
  :ensure t
  :general
  (general-define-key
   :keymaps 'normal
   "SPC c c" 'zenity-cp-color-at-point-dwim))

;; theme
(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-one t))

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package ranger
  :ensure t
  :init
  (setq helm-descbinds-window-style 'same-window)
  (setq ranger-cleanup-on-disable t)
  (setq ranger-cleanup-eagerly t)
  (setq ranger-show-hidden t)
  (setq ranger-modify-header t)
  (setq ranger-preview-file t)
  :config
  (ranger-override-dired-mode t))

(global-display-fill-column-indicator-mode)

(setq org-agenda-files (quote ("~/Documents/wiki/todo.org"
			       "~/Documents/wiki/todo-history.org")))
(setq org-default-notes-file "~/Documents/wiki/notes.org")

(custom-set-variables
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t))

(setq org-directory "~/Documents/wiki")
(setq org-default-notes-file "~/Documents/wiki/refile.org")


(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Documents/wiki/refile.org")
               "* TODO %?\n")
              ("r" "respond" entry (file "~/Documents/wiki/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/Documents/wiki/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/Documents/wiki/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/Documents/wiki/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/Documents/wiki/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/Documents/wiki/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/Documents/wiki/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
