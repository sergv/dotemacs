;;; awk-setup.el ---

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
  (init-common)
  (autopair-mode)
  (modify-syntax-entry ?\/ "\"")
  (add-hook 'after-save-hook #'make-script-file-exec)

  (setq vim:insert-mode-local-keymap (make-sparse-keymap)
        vim:normal-mode-local-keymap (make-sparse-keymap))

  (def-keys-for-map2 vim:normal-mode-local-keymap
    ("C-SPC" icicle-comint-dynamic-complete-filename)
    ("g t"   c-awk-end-of-defun)
    ("g n"   c-awk-beginning-of-defun))

  (def-keys-for-map2 vim:insert-mode-local-keymap
    ("C-SPC" icicle-comint-dynamic-complete-filename)))


(add-hook 'awk-mode-hook #'awk-setup)

;;
;; Local Variables:
;; lexical-binding: t
;; End:

;;; awk-setup.el ends here
