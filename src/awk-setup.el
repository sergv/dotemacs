;; awk-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 20 August 2011
;; Keywords: awk
;; Requirements:
;; Status:

(require 'common)

(declare-function c-awk-beginning-of-defun "cc-awk")
(declare-function c-awk-end-of-defun "cc-awk")
(declare-function server-edit "server")

(defhydra-derive hydra-awk-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_t_: beginning of defun
_h_: end of defun"
  ("t" c-awk-beginning-of-defun)
  ("h" c-awk-end-of-defun))

;;;###autoload
(defun awk-setup ()
  (init-common :use-yasnippet nil
               :use-whitespace 'tabs-only)
  (modify-syntax-entry ?\/ "\"")
  (add-hook 'after-save-hook #'make-script-file-exec)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("g" hydra-awk-vim-normal-g-ext/body)))

;;;###autoload
(add-hook 'awk-mode-hook #'awk-setup)

(provide 'awk-setup)

;; Local Variables:
;; End:

;; awk-setup.el ends here
