;; awk-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 20 August 2011
;; Keywords: awk
;; Requirements:
;; Status:

(require 'custom)

(autoload 'awk-start "awk+" nil t)
;; (defalias 'awk 'awk-start)

(defun awk-setup ()
  (init-common :use-yasnippet nil)
  (modify-syntax-entry ?\/ "\"")
  (add-hook 'after-save-hook #'make-script-file-exec)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("C-SPC" icicle-comint-dynamic-complete-filename)
    ("g t"   c-awk-end-of-defun)
    ("g n"   c-awk-beginning-of-defun)
    ("g <up>"   c-awk-beginning-of-defun)
    ("g <down>" c-awk-end-of-defun))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("C-SPC" icicle-comint-dynamic-complete-filename)))


(add-hook 'awk-mode-hook #'awk-setup)

;; Local Variables:
;; End:

;; awk-setup.el ends here
