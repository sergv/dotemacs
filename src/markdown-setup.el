;;; markdown-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 26 November 2011
;; Keywords:
;; Requirements:
;; Status:


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))


;;; markdown-setup.el ends here
