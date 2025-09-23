;; emacs-lisp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 December 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl)
  (require 'macro-util)
  (require 'comment-util))

(require 'company-mode-setup)
(require 'elisp-slime-nav)
(require 'emacs-lisp-abbrev+)
(require 'emacs-lisp-highlight)
(require 'eproj)
(require 'find-func)
(require 'general-lisp-setup)
(require 'hydra-setup)
(require 'set-up-paths)

;;; elisp fontification and indentation

(font-lock-add-keywords
 'emacs-lisp-mode
 (-map (lambda (x)
         (list (concat "(\\(" x "\\)\\_>")
               1
               'font-lock-keyword-face))
       '("aif"
         "awhen"
         "if-let"
         "when-let"
         "def-keys-for-map"
         "defvar-local"
         "with-disabled-undo"
         "with-current-frame"
         "with-preserved-buffer-modified-p"
         "with-inhibited-modification-hooks"
         "with-inhibited-read-only"
         "with-hidden-comments"
         "with-hidden-cloze-hints"
         "with-hidden-cloze-text")))

(defvar *emacs-lisp-indent-specs*
  '((autoload nil)
    (loop 0)
    (cond 0)
    (awhen 1)
    (def-keys-for-map 1)
    (condition-case 2)
    (define-derived-mode 3)
    (with-disabled-undo nil)
    (with-current-frame 1)
    (with-preserved-buffer-modified-p nil)
    (with-inhibited-modification-hooks nil)
    (with-inhibited-read-only nil)
    (with-hidden-comments nil)
    (with-hidden-cloze-hints nil)
    (with-hidden-cloze-text nil)
    (c-lang-defconst 1))
  "Indentation specifications for emacs lisp.")

(dolist (entry *emacs-lisp-indent-specs*)
  (cl-destructuring-bind (symb indent-spec) entry
    (put symb 'lisp-indent-function indent-spec)))

;;;;

(autoload 'pp-last-sexp "pp")

(defun expand-last-macro ()
  (interactive)
  ;; taken from pp.el
  (insert (pp-to-string (macroexpand (pp-last-sexp)))))

(defun expand-last-macro-all ()
  (interactive)
  ;; taken from pp.el
  (insert (pp-to-string (macroexpand-all (pp-last-sexp)))))

(defun emacs-lisp-newline-and-indent ()
  (interactive)
  (newline-and-indent))
(comment-util-auto-comment-advice emacs-lisp-newline-and-indent)

(defhydra-derive hydra-emacs-lisp-vim-normal-j-ext hydra-lisp-vim-normal-j-ext (:exit t :foreign-keys nil :hint nil)
  "
_j_: eval"
  ("j" eval-last-sexp))

(defhydra-ext hydra-emacs-lisp-dash (:exit t :foreign-keys nil :hint nil)
  "
expand _m_acro  _M_: fully expand macro

_e_val
realign _l_et
"
  ("m" expand-last-macro)
  ("M" expand-last-macro-all)

  ("e" eval-last-sexp)
  ("l" realign-let))

;;;###autoload
(defun emacs-lisp-setup ()
  (lisp-setup)
  (eldoc-mode -1)
  (company-mode +1)
  (prepare-paredit :indent-sexp #'indent-sexp
                   :indent-line #'lisp-indent-line
                   :calc-indent #'calculate-lisp-indent
                   :indent-region #'indent-region
                   :in-char-p #'paredit-in-lisp-char-p
                   :space-before-open-paren t)
  (setq-local company-backends '(company-elisp))

  (emacs-lisp-highlight-keywords)

  (def-keys-for-map vim-normal-mode-local-keymap
    ("j"       hydra-emacs-lisp-vim-normal-j-ext/body)
    ("-"       hydra-emacs-lisp-dash/body)

    ("M-:"     nil)
    ("C-:"     pp-eval-expression)
    ("C-."     elisp-slime-nav-find-elisp-thing-at-point)
    ("C-,"     eproj-symbnav/go-back)

    ("<tab>"        indent-for-tab-command)
    (("C-m" "<f9>") elisp-compile-and-move))

  (def-keys-for-map vim-insert-mode-local-keymap
    ("<tab>"   indent-for-tab-command))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-SPC"    company-complete)
    ("<return>" emacs-lisp-newline-and-indent))

  ;; should use global after-save-hook because of
  ;; backups
  (add-hook 'after-save-hook #'elisp-compile-and-move)
  (emacs-lisp-abbrev+-setup))

;;;###autoload
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-setup)

(eval-after-load "edebug"
  '(progn
     (def-keys-for-map edebug-mode-map
       ("<f5>" edebug-stop)
       ("<f6>" edebug-step-mode)
       ("<f7>" edebug-step-in)
       ("<f8>" edebug-step-out))))

(defun eval-print-last-sexp-unlimited-length ()
  (interactive)
  (let ((eval-expression-print-length nil)
        (print-length nil)
        (eval-expression-print-level nil))
    (eval-print-last-sexp)))

;;;;

(defun elisp-compile-get-elc-destination (path)
  (concat
   +emacs-config-path+
   "/compiled/"
   (file-name-sans-extension (file-name-nondirectory path)) ".elc"))

(defun elisp-compile-and-move ()
  (interactive)
  (save-buffer-if-modified)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (not no-byte-compile)
             (not (string= ".eproj-info"
                           (file-name-nondirectory buffer-file-name))))
    (let ((window-config (current-window-configuration))
          (byte-compile-dest-file-function #'elisp-compile-get-elc-destination))
      (if (byte-compile-file buffer-file-name)
          (progn
            (ignore-errors
              (kill-buffer "*Compile-Log*"))
            ;; restore window config
            (set-window-configuration window-config))
        (message "Compilation errors, check out *Compile-log*")))))

;;; elisp debugger

;;;###autoload
(defun debugger-setup ()
  (def-keys-for-map debugger-mode-map
    +vi-essential-keys+
    +vim-search-keys+
    +vim-special-keys+
    +vim-word-motion-keys+
    ("<escape>" exit-recursive-edit)
    ("m"        vim:motion-jump-item:interactive)))

;;;###autoload
(add-hook 'debugger-mode-hook #'debugger-setup)

(defun eval-last-sexp--save-mark-around (old-func &rest args)
  ;; I don’t want to issue ‘save-excursion’ around ‘eval-last-sexp’
  ;; call which ‘save-mark-and-excursion’ unfortunately does.
  ;;
  ;; Keep in sync with ‘save-mark-and-excursion’.
  (let ((saved-mark (save-mark-and-excursion--save)))
    (unwind-protect
        (apply old-func args)
      (save-mark-and-excursion--restore saved-mark))))

(advice-add 'eval-last-sexp :around #'eval-last-sexp--save-mark-around)

(provide 'emacs-lisp-setup)

;; Local Variables:
;; End:

;; emacs-lisp-setup.el ends here
