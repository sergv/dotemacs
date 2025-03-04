;; isabelle-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  1 August 2023
;; Description:

(require 'common-font)
(require 'flycheck-setup)
(require 'hydra-setup)
(require 'isar-mode)
(require 'lsp-setup)

(require 'isabelle-abbrev+)

(defconst lsp-isar-path-to-isabelle-exe (executable-find "isabelle-emacs-lsp"))

(defun lsp-full-isabelle-path-override ()
  (append (list lsp-isar-path-to-isabelle-exe
                "vscode_server")
          lsp-vscode-options
          lsp-isabelle-options))

(advice-add 'lsp-full-isabelle-path :override #'lsp-full-isabelle-path-override)

(defhydra-derive hydra-isabelle-lsp-toggle hydra-lsp-toggle (:exit t :foreign-keys nil :hint nil)
  "")

(defhydra-ext hydra-isabelle-lsp (:exit t :foreign-keys warn :hint nil)
  "
_a_ctions  _d_ocumentation  toggle some _o_ptions
_r_ename
"
  ("a" lsp-execute-code-action)
  ("r" lsp-rename)

  ("d" lsp-doc-other-window)

  ("o" hydra-haskell-lsp-toggle/body))

(defun isar-lsp-status ()
  (when-let (buf-file (buffer-file-name))
    (lsp-isar-progress--get buf-file)))

;;;###autoload
(defun isar-setup ()
  (unless (memq buffer-file-coding-system '(utf-8 utf-8-unix utf-8-dos))
    (set-buffer-file-coding-system 'utf-8))
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-fci nil ;; Doesn’t play well with subscripts
               :use-whitespace 'tabs-only)
  (setup-indent-size 2)
  (lsp-isar-define-client-and-start)
  (setq-local mode-line-format
              (apply #'default-mode-line-format
                     (list " " '(:eval (isar-lsp-status))))
              lsp-ui-sideline-enable t
              lsp-ui-sideline-show-diagnostics nil
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-show-code-actions t)

  (setup-lsp-symbnav :bind-keybindings nil)
  (def-keys-for-map vim-normal-mode-local-keymap
    ("C-." lsp-symbnav/go-to-symbol-home-no-regexp)
    ("C-," lsp-symbnav/go-back)
    ("C-?" lsp-symbnav/find-references))

  (def-keys-for-map vim-normal-mode-local-keymap
    ("\\"  flycheck-force-run)
    ("-"   hydra-isabelle-lsp/body))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-h" flycheck-enhancements-next-error-with-wraparound)
    ("C-t" flycheck-enhancements-previous-error-with-wraparound))

  (isabelle-abbrev+-setup))

;;;###autoload
(add-hook 'isar-mode-hook #'isar-setup)

(defun isar-goal-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-fci nil
               :use-whitespace nil
               :use-render-formula nil
               :use-hl-line t
               :enable-backup nil
               :hl-parens-backend 'hl-paren
               :typography t
               :smerge nil))

;;;###autoload
(add-hook 'isar-goal-mode-hook #'isar-goal-setup)

(provide 'isabelle-setup)

;; Local Variables:
;; End:

;; isabelle-setup.el ends here
