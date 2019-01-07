(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq package-list '( evil
                      company
                      company-ghci
                      editorconfig
                      flycheck
                      idris-mode
                      magit
                      markdown-mode
                      rainbow-delimiters
                      use-package
                      yaml-mode ))

; rm -rf ~/.emacs.d/elpa to reload
(when (not package-archive-contents)
(package-refresh-contents))

; install if missing
(dolist (package package-list)
  (when (not (package-installed-p package))
(package-install package)))

(require 'use-package)

;; EditorConfig
(require 'editorconfig)
(editorconfig-mode 1)

(require 'magit)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'evil)
(evil-mode 1)

(global-linum-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "43b219a31db8fddfdc8fdbfdbd97e3d64c09c1c9fdd5dff83f3ffc2ddb8f0ba0" "aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (solarized-theme evil-surround blackboard-theme dracula-theme evil ## agda2-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
