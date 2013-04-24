;; emacs-lisp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'general-lisp-setup)
(require 'find-func)
(require 'emacs-lisp-abbrev+)
(require 'emacs-lisp-highlight)
(require 'elisp-slime-nav)

;;;; elisp fontification and indentation

(font-lock-add-keywords 'emacs-lisp-mode
                        (map (lambda (x)
                               (list (concat "(\\(" x "\\)\\_>")
                                     1
                                     'font-lock-keyword-face))
                             '("aif"
                               "awhen"
                               "begin"
                               "if-let"
                               "when-let"
                               "def-keys-for-map"
                               "defvar-local"
                               "rxx"
                               "redefun"
                               "with-disabled-undo"
                               "with-current-frame"
                               "with-preserved-buffer-modified-p"
                               "with-inhibited-modification-hooks"
                               "with-inhibited-read-only"
                               "with-hidden-comments"
                               "with-hidden-cloze-hints"
                               "with-hidden-cloze-text")))


(put 'redefun 'doc-string-elt 3)

(defvar *emacs-lisp-indent-specs*
  '((autoload nil)
    (begin 0)
    (if 1)
    (loop 0)
    (cond 0)
    (aif 1)
    (awhen 1)
    (if-let 1)
    (when-let 1)
    (def-keys-for-map 1)
    (condition-case 2)
    (rxx 1)
    (define-print-info-skeleton 1)
    (define-lisp-print-info-skeleton 1)
    (define-derived-mode 3)
    (redefun defun)
    (with-disabled-undo nil)
    (with-current-frame 1)
    (with-preserved-buffer-modified-p nil)
    (with-inhibited-modification-hooks nil)
    (with-inhibited-read-only nil)
    (with-hidden-comments nil)
    (with-hidden-cloze-hints nil)
    (with-hidden-cloze-text nil))
  "Indentation specifications for emacs lisp.")

(dolist (entry *emacs-lisp-indent-specs*)
  (destructuring-bind (symb indent-spec) entry
    (put symb 'lisp-indent-function indent-spec)))

;;;;

(defun expand-last-macro ()
  (interactive)
  ;; taken from pp.el
  (insert (pp-to-string (macroexpand (pp-last-sexp)))))

(defun expand-last-macro-all ()
  (interactive)
  ;; taken from pp.el
  (insert (pp-to-string (macroexpand-all (pp-last-sexp)))))


(defun emacs-lisp-setup ()
  (lisp-setup)

  (emacs-lisp-highlight-keywords)

  (setf autopair-extra-pairs
        '(:comment
          ((?` . ?'))
          :string
          ((?` . ?'))))
  (autopair-mode t)

  (eldoc-mode 1)
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("j"     eval-last-sexp)
    ("g J"   vim:cmd-join-lines)
    (", m"   expand-last-macro)
    (", M"   expand-last-macro-all)

    ("M-:"   icicle-pp-eval-expression)
    ("M-."   elisp-slime-nav-find-elisp-thing-at-point)
    ("M-,"   pop-tag-mark)
    ("M-/"   lisp-complete-symbol)

    ("<tab>" indent-for-tab-command)
    ("<f9>"  elisp-compile-and-move))

  (def-keys-for-map vim:visual-mode-local-keymap
    ("j"   eval-region))

  (def-keys-for-map vim:insert-mode-local-keymap
    ;; ("C-SPC" lisp-complete-symbol)
    ("<tab>" indent-for-tab-command)
    ("M-/"   lisp-complete-symbol)
    ("("     paredit-open-round)
    (")"     paredit-close-round)
    ("["     paredit-open-square)
    ("]"     paredit-close-square)
    ("\""    paredit-doublequote))

  (def-keys-for-map read-expression-map
    ("<tab>" lisp-complete-symbol)
    ("M-/"   lisp-complete-symbol)
    ("C-w"   backward-delete-word)
    ("C-S-w" backward-delete-word*))

  ;; should use global after-save-hook because of
  ;; backups
  (add-hook 'after-save-hook #'elisp-compile-and-move)
  (emacs-lisp-abbrev+-setup))

(eval-after-load "edebug"
  '(progn
     (def-keys-for-map edebug-mode-map
       ("<f6>" edebug-step-mode))))

(defun eval-print-last-sexp-unlimited-length ()
  (interactive)
  (let ((eval-expression-print-length nil)
        (print-length nil)
        (eval-expression-print-level nil))
    (eval-print-last-sexp)))

;;;;

(defun elisp-compile-and-move ()
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (when (and (eq major-mode 'emacs-lisp-mode)
             (not no-byte-compile))
    (let ((file (buffer-file-name))
          (window-config (current-window-configuration)))
      (if (byte-compile-file file)
        (progn
          (kill-buffer "*Compile-Log*")
          ;; restore window config
          (set-window-configuration window-config))
        (message "Compilation errors, check out *Compile-log*")))))

;;;; elisp debugger

(defun debugger-setup ()
  (def-keys-for-map debugger-mode-map
    +vi-essential-keys+
    +control-x-prefix+
    +vim-special-keys+
    +vim-word-motion-keys+
    ("<escape>" exit-recursive-edit)))

(add-hook 'debugger-mode-hook #'debugger-setup)

(provide 'emacs-lisp-setup)

;; Local Variables:
;; End:

;; emacs-lisp-setup.el ends here
