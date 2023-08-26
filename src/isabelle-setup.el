;; isabelle-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  1 August 2023
;; Description:

(defvar lsp-isar-progress-theory-name-map)
(defvar lsp-isar-split-pattern)

(require 'isar-mode)

(setq lsp-isar-split-pattern 'lsp-isar-split-pattern-two-columns)

(defun isar-lsp-status ()
  (when-let (buf-file (buffer-file-name))
    (lsp-isar-progress--get buf-file)))

;;;###autoload
(defun isar-setup ()
  (init-common :use-yasnippet t
               :use-comment t
               :use-render-formula t
               :use-fci t
               :use-whitespace 'tabs-only)
  (setup-indent-size 2)
  (lsp-isar-define-client-and-start)
  (setq-local mode-line-format
              (apply #'default-mode-line-format
                     (list " " '(:eval (isar-lsp-status)))))

  (def-keys-for-map (vim-normal-mode-local-keymap
                     vim-insert-mode-local-keymap)
    ("C-h" flycheck-enhancements-next-error-with-wraparound)
    ("C-t" flycheck-enhancements-previous-error-with-wraparound)))

;;;###autoload
(add-hook 'isar-mode-hook #'isar-setup)

;;;###autoload
(add-hook 'lsp-isar-init-hook #'lsp-isar-open-output-and-progress-right-spacemacs)

(provide 'isabelle-setup)

;; Local Variables:
;; End:

;; isabelle-setup.el ends here
