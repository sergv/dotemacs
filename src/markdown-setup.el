;; markdown-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 26 November 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile (require 'cl-lib))

(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(defun markdown-setup ()
  (init-common :use-yasnippet nil))

(add-hook 'markdown-mode-hook #'markdown-setup)

(provide 'markdown-setup)

;; Local Variables:
;; End:

;; markdown-setup.el ends here
