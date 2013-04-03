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
(require 'ansi-lisp-highlight)
(require 'elisp-slime-nav)


(font-lock-add-keywords 'emacs-lisp-mode
                        '("defvar-local"))


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

  (ansi-lisp-highlight-emacs-keywords)

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
    ("J"     eval-print-last-sexp-unlimited-length)
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
    ("M-/"   lisp-complete-symbol))

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
