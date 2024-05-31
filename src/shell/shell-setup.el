;; shell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

;; set up shell scripting files and shell interaction mode

(eval-when-compile
  (require 'set-up-platform))

(require 'common)
(require 'comint-setup)
(require 'dirtrack)
(require 'folding-setup)
(require 'shell-script-abbrev+)
(require 'xterm-color)

(declare-function msys-directory-name-to-emacs "windows-setup")
(declare-function cygwin-directory-name-to-emacs "windows-setup")

;;;###autoload
(unless (getenv "SHELL")
  (setenv "SHELL" shell-file-name))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xbindkeysrc$" . shell-script-mode))

;;;###autoload
(add-to-list 'display-buffer-alist
             '("^\\*Async Shell Command\\*" . (display-buffer-no-window)))

;;;###autoload
(add-to-list 'display-buffer-alist
             '("^\\*shell.*\\*" . (display-buffer-same-window)))

;;;###autoload
(defun shell-run-file ()
  "Run buffer's script file."
  (interactive)
  (compilation-start (concat "./"
                             (file-name-nondirectory
                              (shell-quote-argument buffer-file-name)))
                     t))

;;;###autoload
(defun shell-script-setup ()
  (init-common :use-yasnippet t
               :use-whitespace 'tabs-only
               :use-fci nil)
  (setup-folding t '(:header-symbol "#" :length-min 4))
  (add-hook 'after-save-hook #'make-script-file-exec nil t)

  (setq-local yas-indent-line 'fixed)
  (which-function-mode -1)
  (bind-tab-keys #'indent-for-tab-command
                 nil
                 :enable-yasnippet t)

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    (("C-m" "<f9>") shell-run-file)
    ("<return>"     newline-and-indent))
  (shell-script-abbrev+-setup))

;;;###autoload
(dolist (mode '(cmake-mode-hook
                shell-script-mode-hook
                sh-mode-hook
                sh-script-mode-hook))
  (add-hook mode #'shell-script-setup))

;;;###autoload
(defun make-dirtrack-windows-msys-directory-function (internal-func)
  "Wrapper around `dirtrack-directory-function' that canonicalizes
MSYS-style drives, e.g. \"/c/foo/bar.txt\" -> \"c:/foo/bar.txt\"."
  (lambda (dir)
    (fold-platform-os-type
     (error "The result of `make-dirtrack-windows-msys-directory-function' must be used only in Windows environment.")
     nil)
    (save-match-data
      (funcall
       internal-func
       (if (string-match "^\\(/\\([a-zA-Z]\\)\\)/.*$" dir)
           (let ((drive (match-string-no-properties 2 dir)))
             (replace-match (concat drive ":") nil t dir 1))
         dir)))))

;;;###autoload
(fold-platform-os-type
 nil
 (setf dirtrack-directory-function
       (make-dirtrack-windows-msys-directory-function dirtrack-directory-function)))

;;;###autoload
(add-to-list
 'hs-special-modes-alist
 (list 'shell-mode
       ;; start regex
       (rx (or "[" "(" "{"))
       ;; end regex
       nil
       ;; comment-start regex
       (rx (or (+ "#")
               (>= 2 "/")))
       ;; forward-sexp function
       nil
       ;; adjust beg function
       nil))

(eval-after-load "shell"
  '(progn
     (let ((st shell-mode-syntax-table))
       (modify-syntax-entry ?\" ".   " st)
       (modify-syntax-entry ?\\ ".   " st)
       ;; We add `p' so that M-c on 'hello' leads to 'Hello' rather than 'hello'.
       (modify-syntax-entry ?' "w p" st)
       ;; UAX #29 says HEBREW PUNCTUATION GERESH behaves like a letter
       ;; for the purposes of finding word boundaries.
       (modify-syntax-entry #x5f3 "w   ")  ; GERESH
       ;; UAX #29 says HEBREW PUNCTUATION GERSHAYIM should not be a word
       ;; boundary when surrounded by letters.  Our infrastructure for
       ;; finding a word boundary doesn't support 3-character
       ;; definitions, so for now simply make this a word-constituent
       ;; character.  This leaves a problem of having GERSHAYIM at the
       ;; beginning or end of a word, where it should be a boundary;
       ;; FIXME.
       (modify-syntax-entry #x5f4 "w   ")  ; GERSHAYIM
       ;; These all should not be a word boundary when between letters,
       ;; according to UAX #29, so they again are prone to the same
       ;; problem as GERSHAYIM; FIXME.
       (modify-syntax-entry #xb7 "w   ")   ; MIDDLE DOT
       (modify-syntax-entry #x2027 "w   ") ; HYPHENATION POINT
       (modify-syntax-entry #xff1a "w   ") ; FULLWIDTH COLON
       )))

(when-windows
 (require 'windows-setup)

 (when (memq system-type '(ms-dos windows-nt))
   (setf dirtrack-directory-function #'msys-directory-name-to-emacs))

 (when (eq system-type 'cygwin)
   (setf dirtrack-directory-function #'cygwin-directory-name-to-emacs)))

(defun shell-backward-up-indentation-or-sexp ()
  "Haskell brother of ‘paredit-backward-up’ that considers both
sexps and indentation levels."
  (interactive)
  (indent-backward-up-indentation-or-sexp #'indent-on-blank-line?))

(vimmize-motion shell-backward-up-indentation-or-sexp
                :name vim:shell-backward-up-indentation-or-sexp
                :exclusive t
                :unadjusted t
                :raw-result t)

(defconst shell-dirtrack-regexp
  (rx bol
      (* (any ?\r))
      (? (+ (not (or ?: ?\s ?\r ?\n)))
         ":")
      (group (+ (not (or ?$ ?\r ?\n))))
      (any ?$ ?#)))

(defconst shell-dirtrack-entry (list shell-dirtrack-regexp 1))

;;;###autoload
(defun shell-setup ()
  (init-repl :show-directory t
             :create-keymaps t)
  (hl-line-mode +1)

  ;; Simplest config that works with this: PS1="\w$"
  (setq-local dirtrack-list shell-dirtrack-entry)
  (dirtrack-mode +1)

  (setq-local comment-start "#"
              comment-end   "")
  (setup-folding t nil)
  (setup-hideshow-yafolding t nil)

  (setq-local vim-bounds-of-string-guess-start #'vim--bounds-of-string--guess-via-comint-prompt)

  (with-editor-export-editor)
  (with-editor-export-git-editor)

  (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter nil t)
  (setq-local comint-scroll-to-bottom-on-input t)

  (vim-local-emap "clear" #'vim:comint-clear-buffer-above-prompt)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("SPC SPC" comint-clear-prompt))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap
                     shell-mode-map)
    ("C-SPC"     vim:comint-clear-buffer-above-prompt:interactive)
    ("<tab>"     completion-at-point)

    ("M-p"       browse-comint-input-history)

    ("C-w"       backward-delete-word)
    ("C-S-w"     backward-delete-word*)

    ("C-t"       comint-previous-prompt)
    ("C-h"       comint-next-prompt)
    ("<up>"      comint-previous-input-or-bwd-paragraph)
    ("<down>"    comint-next-input-or-fwd-paragraph)
    ("C-<up>"    comint-previous-prompt)
    ("C-<down>"  comint-next-prompt)

    ;; ("C-c C-k"   comint-kill-subjob)
    )

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-visual-mode-local-keymap
                     vim-motion-mode-local-keymap
                     vim-operator-pending-mode-local-keymap)
    ("'" vim:shell-backward-up-indentation-or-sexp:interactive)))

;;;###autoload
(add-hook 'shell-mode-hook #'shell-setup)

(provide 'shell-setup)

;; Local Variables:
;; End:

;; shell-setup.el ends here
