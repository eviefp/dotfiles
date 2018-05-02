;;; xah-math-input.el --- a minor mode for inputting math and Unicode symbols. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2010-2017 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 2.2.20170905
;; Created: 08 Dec 2010
;; Package-Requires: ((emacs "24.1"))
;; Keywords: abbrev, convenience, unicode, math, LaTex
;; License: GPL v3
;; URL: http://ergoemacs.org/emacs/xmsi-math-symbols-input.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A minor mode for inputing math symbols and Unicode symbols.

;; Call `global-xah-math-input-mode' to toggle on/off for all buffers.
;; Call `xah-math-input-mode' to toggle on/off for current buffer.

;; In lisp code:
;; (global-xah-math-input-mode 1) ; turn on globally
;; (global-xah-math-input-mode 0) ; turn off globally
;; (xah-math-input-mode 1) or (xah-math-input-mode-on) ; turn on for current buffer
;; (xah-math-input-mode 0) or (xah-math-input-mode-off) ; turn off for current buffer

;; Type “inf”, then press 【Shift+Space】 `xah-math-input-change-to-symbol', then it becomes “∞”.
;; Other examples:
;; “a” ⇒ “α”.
;; “p” ⇒ “π”.
;; “/=” ⇒ “≠”.
;; “>=” ⇒ “≥”.
;; “=>” ⇒ “⇒”.
;; “->” ⇒ “→”.
;; “and” ⇒ “∧”.
;; etc.

;; Call `xah-math-input-list-math-symbols' to see all abbrevs.

;; Home page: http://ergoemacs.org/emacs/xmsi-math-symbols-input.html

;;; Install:

;; Manual install.
;; To have emacs automatically load the file when it restarts, follow these steps:

;; Place the file in the dir 〔~/.emacs.d/lisp/〕. Create the folder if you don't have it.

;; Put the following lines in your emacs init file:
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-math-input)
;; (global-xah-math-input-mode 1) ; activate the mode globally

;; References
;; http://xahlee.info/comp/unicode_index.html
;; http://xahlee.info/comp/unicode_math_operators.html
;; 〈How Mathematica does Unicode?〉 http://xahlee.info/math/mathematica_unicode.html



;;; Code:

(defvar xah-math-input-abrvs nil "A abbreviation hash table that maps a string to unicode char.")
(setq xah-math-input-abrvs (make-hash-table :test 'equal))

(defun xah-math-input--add-to-hash (φpairs)
  "Add φpairs to the hash table `xah-math-input-abrvs'.
φpairs is a sequence of pairs. Each element is a sequence of 2 items, [key, value]."
  (mapc
   (lambda (x) (puthash (elt x 0) (elt x 1) xah-math-input-abrvs))
   φpairs))

(xah-math-input--add-to-hash
 ;; xml entities http://xahlee.info/comp/unicode_html_entities.html
 [["::"     "∷"]
  ["forall" "∀"]
  ["->"     "→"]
  ["<-"     "←"]
  ["=>"     "⇒"]
  ["<="     "⇐"]
  ["."      "∘"]
  ])

(defun xah-math-input--hash-to-list (hashtable)
  "Return a list that represent the HASHTABLE."
  (let (mylist)
    (maphash (lambda (kk vv) (setq mylist (cons (list vv kk) mylist))) hashtable)
    mylist
    ))

(defun xah-math-input-list-math-symbols ()
  "Print a list of math symbols and their input abbreviations.
See `xah-math-input-mode'."
  (interactive)
  (with-output-to-temp-buffer "*xah-math-input output*"
    (mapc (lambda (tt)
            (princ (concat (car tt) " " (car (cdr tt)) "\n")))
          (sort
           (xah-math-input--hash-to-list xah-math-input-abrvs)
           (lambda
             (a b)
             (string< (car a) (car b)))))))

(defvar xah-math-input-keymap nil "Keymap for xah-math-input mode.")

(progn
  (setq xah-math-input-keymap (make-sparse-keymap))
  (define-key xah-math-input-keymap (kbd "S-SPC") 'xah-math-input-change-to-symbol))

(defun xah-math-input--abbr-to-symbol (inputString)
  "Returns a char corresponding to inputString."
  (let (resultSymbol charByNameResult)
    (setq resultSymbol (gethash inputString xah-math-input-abrvs))
    (cond
     (resultSymbol resultSymbol)
     ;; decimal. 「945」 or 「#945」
     ((string-match "\\`#?\\([0-9]+\\)\\'" inputString) (char-to-string (string-to-number (match-string 1 inputString))))
     ;; e.g. decimal with html entity markup. 「&#945;」
     ((string-match "\\`&#\\([0-9]+\\);\\'" inputString) (char-to-string (string-to-number (match-string 1 inputString))))
     ;; hex number. e.g. 「x3b1」 or 「#x3b1」
     ((string-match "\\`#?x\\([0-9a-fA-F]+\\)\\'" inputString) (char-to-string (string-to-number (match-string 1 inputString) 16)))
     ;; html entity hex number. e.g. 「&#x3b1;」
     ((string-match "\\`&#x\\([0-9a-fA-F]+\\);\\'" inputString) (char-to-string (string-to-number (match-string 1 inputString) 16)))
     ;; unicode full name. e.g. 「GREEK SMALL LETTER ALPHA」
     ((and (string-match "\\`\\([- a-zA-Z0-9]+\\)\\'" inputString) (setq charByNameResult (assoc-string inputString (ucs-names) t))) (char-to-string (cdr charByNameResult)))
     (t nil))))

(defun xah-math-input-change-to-symbol (&optional print-message-when-no-match)
  "Change text selection or word to the left of cursor into a Unicode character.

A valid input can be any abbreviation listed by the command `xah-math-input-list-math-symbols', or, any of the following form:

 945     ← decimal
 #945    ← decimal with prefix #
 &#945;  ← XML entity syntax

 x3b1    ← hexadimal with prefix x
 #x3b1   ← hexadimal with prefix #x
 &#x3b1; ← XML entity syntax

Full Unicode name can also be used, e.g. 「greek small letter alpha」.

If preceded by `universal-argument', print error message when no valid abbrev found.

See also: `xah-math-input-mode'."
  (interactive "P")
  (let (p1 p2 inputStr resultSymbol)
    (if (region-active-p)
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          (setq inputStr (buffer-substring-no-properties p1 p2))
          (setq resultSymbol (xah-math-input--abbr-to-symbol inputStr))
          (when resultSymbol (progn (delete-region p1 p2) (insert resultSymbol))))
      ;; if there's no text selection, grab all chars to the left of cursor point up to whitespace, try each string until there a valid abbrev found or none char left.
      (progn
        (setq p2 (point))
        (skip-chars-backward "^ \t\n" -20)
        (setq p1 (point))
        (while (and (not resultSymbol) (>= (- p2 p1) 1))
          (setq inputStr (buffer-substring-no-properties p1 p2))
          (setq resultSymbol (xah-math-input--abbr-to-symbol inputStr))
          (when resultSymbol (progn (goto-char p2) (delete-region p1 p2) (insert resultSymbol)))
          (setq p1 (1+ p1)))))
    (when (not resultSymbol)
      (when print-message-when-no-match
        (xah-math-input-list-math-symbols)
        (user-error "「%s」 no match found for that abbrev/input. Call “xah-math-input-list-math-symbols” for a list. Or use a decimal e.g. 「945」 or hexadecimal e.g. 「x3b1」, or full Unicode name e.g. 「greek small letter alpha」."  inputStr)))))

;;;###autoload
(define-globalized-minor-mode global-xah-math-input-mode xah-math-input-mode xah-math-input-mode-on)

;;;###autoload
(defun xah-math-input-mode-on ()
  "Turn on `xah-math-input-mode' in current buffer."
  (interactive)
  (xah-math-input-mode 1))

;;;###autoload
(defun xah-math-input-mode-off ()
  "Turn off `xah-math-input-mode' in current buffer."
  (interactive)
  (xah-math-input-mode 0))

;;;###autoload
(define-minor-mode xah-math-input-mode
  "Toggle xah-math-input minor mode.

A mode for inputting a math and Unicode symbols.

Type “inf”, then press \\[xah-math-input-change-to-symbol], then it becomes “∞”.

Other examples:
 a ⇒ α
 p ⇒ π
 /= ⇒ ≠
 >= ⇒ ≥
 => ⇒ ⇒
 -> ⇒ →
 and ⇒ ∧
etc.

If you have a text selection, then selected word will be taken as input. For example, type 「extraterrestrial alien」, select the phrase, then press 【Shift+Space】, then it becomse 「👽」.

For the complete list of abbrevs, call `xah-math-input-list-math-symbols'.

Decimal and hexadecimal can also be used. Example:

 945     ← decimal
 #945    ← decimal with prefix #
 &#945;  ← XML entity syntax

 x3b1    ← hexadimal with prefix x
 #x3b1   ← hexadimal with prefix #x
 &#x3b1; ← XML entity syntax

Full Unicode name can also be used, e.g. 「greek small letter alpha」.

If you wish to enter a symbor by full unicode name but do not
know the full name, call `ucs-insert'. Asterisk “*” can be used
as a wildcard to find the char. For example, call
“ucs-insert”, then type 「*arrow」 then Tab, then emacs will list
all unicode char names that has “arrow” in it. (this feature is
part of Emacs 23)

Home page at: URL `http://ergoemacs.org/emacs/xah-math-input-math-symbols-input.html'"
  nil
  :global nil
  :lighter " ∑α"
  :keymap xah-math-input-keymap
  )
(provide 'xah-math-input)
;;; xah-math-input.el ends here
