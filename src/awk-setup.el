;; awk-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 20 August 2011
;; Keywords: awk
;; Requirements:
;; Status:

(require 'common)

;;;###autoload
(defun awk-setup ()
  (init-common :use-yasnippet nil
               :use-whitespace 'tabs-only)
  (modify-syntax-entry ?\/ "\"")
  (add-hook 'after-save-hook #'make-script-file-exec)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("g t" c-awk-beginning-of-defun)
    ("g h" c-awk-end-of-defun)))

;;;###autoload
(add-hook 'awk-mode-hook #'awk-setup)

(provide 'awk-setup)

;; Local Variables:
;; End:

;; awk-setup.el ends here
