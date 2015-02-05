;; shell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


;; set up shell scripting files and shell interaction mode

(require 'custom)
(require 'comint-setup)
(require 'shell-script-abbrev+)

(autoload 'shell-command-on-region+ "shell-command+" nil t)
(fset 'shell-command-on-region 'shell-command-on-region+)


(defun shell-run-file ()
  "Run buffer's script file."
  (interactive)
  (compilation-start (concat "./"
                             (file-name-nondirectory
                              (shell-quote-argument
                               (buffer-file-name))))
                     t))

(defun shell-script-setup ()
  (init-common :use-yasnippet t :use-whitespace nil)
  (add-hook 'after-save-hook #'make-script-file-exec nil t)

  (setq-local yas-indent-line 'fixed)
  (which-function-mode -1)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f9>" shell-run-file)
    ("`"    shell-run-file))
  (shell-script-abbrev+-setup))

(defun shell-setup ()
  (init-repl :show-directory t :create-keymaps t)
  (linum-mode +1)
  (smartparens-mode +1)
  (ansi-color-for-comint-mode-on)

  (def-keys-for-map vim:normal-mode-local-keymap
    ;; clear all previous output
    ("SPC SPC" comint-clear-prompt))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     shell-mode-map)
    ("C-SPC"    comint-clear-buffer-above-prompt)
    ("<tab>"    pcomplete)
    ("M-/"      pcomplete)

    ("M-p"      browse-comint-input-history)

    ("C-w"      backward-delete-word)
    ("C-S-w"    backward-delete-word*)

    ("<up>"     comint-previous-input)
    ("<down>"   comint-next-input)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)

    ("C-c C-k"  comint-kill-subjob)

    ("C-<left>"  vim:sp-backward-slurp-sexp)
    ("C-<right>" vim:sp-forward-slurp-sexp)
    ("M-<left>"  sp-absorb-sexp)
    ("M-<right>" sp-emit-sexp))

  (setf comint-scroll-to-bottom-on-input t))

(cond ((and (platform-os-type? 'windows)
            (platform-use? 'work)
            (file-exists? "C:/GnuWin32/bin/bash.exe"))
       (setf shell-file-name "C:/GnuWin32/bin/bash.exe"))
      ((null? (getenv "SHELL"))
       (setenv "SHELL" shell-file-name)))

(provide 'shell-setup)

;; Local Variables:
;; End:

;; shell-setup.el ends here
