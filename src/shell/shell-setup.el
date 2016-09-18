;; shell-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

;; set up shell scripting files and shell interaction mode

(require 'common)
(require 'comint-setup)
(require 'shell-script-abbrev+)

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


(autoload 'shell-command+ "common-heavy" nil t)
(autoload 'shell-command-on-region+ "shell-command+" nil t)
(fset 'shell-command-on-region 'shell-command-on-region+)

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
  (add-hook 'after-save-hook #'make-script-file-exec nil t)

  (setq-local yas-indent-line 'fixed)
  (which-function-mode -1)
  (bind-tab-keys #'indent-for-tab-command
                 nil
                 :enable-yasnippet t)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<f9>" shell-run-file))
  (shell-script-abbrev+-setup))

;;;###autoload
(dolist (mode '(cmake-mode-hook
                shell-script-mode-hook
                sh-mode-hook
                sh-script-mode-hook
                conf-space-mode-hook
                conf-mode-hook
                conf-xdefaults-mode-hook))
  (add-hook mode #'shell-script-setup))

;;;###autoload
(defun shell-setup ()
  (init-repl :show-directory t :create-keymaps t)
  (smartparens-mode +1)
  (hl-line-mode +1)
  (ansi-color-for-comint-mode-on)
  (setq-local comint-scroll-to-bottom-on-input t)

  (vim:local-emap "clear" #'vim:comint-clear-buffer-above-prompt)

  (def-keys-for-map vim:normal-mode-local-keymap
    ;; clear all previous output
    ("SPC SPC" comint-clear-prompt))

  (def-keys-for-map (vim:normal-mode-local-keymap
                     vim:insert-mode-local-keymap
                     shell-mode-map)
    ("C-SPC"     vim:comint-clear-buffer-above-prompt)
    ("<tab>"     pcomplete)

    ("M-p"       browse-comint-input-history)

    ("C-w"       backward-delete-word)
    ("C-S-w"     backward-delete-word*)

    ("C-t"       comint-previous-prompt)
    ("C-h"       comint-next-prompt)
    ("<up>"      comint-previous-input)
    ("<down>"    comint-next-input)
    ("C-<up>"    comint-previous-prompt)
    ("C-<down>"  comint-next-prompt)

    ("C-c C-k"   comint-kill-subjob)

    ("C-<left>"  vim:sp-backward-slurp-sexp)
    ("C-<right>" vim:sp-forward-slurp-sexp)
    ("M-<left>"  sp-absorb-sexp)
    ("M-<right>" sp-emit-sexp)))

;;;###autoload
(add-hook 'shell-mode-hook #'shell-setup)

(provide 'shell-setup)

;; Local Variables:
;; End:

;; shell-setup.el ends here
