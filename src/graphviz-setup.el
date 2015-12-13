;; graphviz-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 January 2012
;; Keywords:
;; Requirements:
;; Status:


(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)

(defun graphviz-setup ()
  (init-common :use-whitespace 'tabs-only)
  (local-set-key (kbd "<f9>") #'compile)
  (local-set-key (kbd "S-<f9>") #'graphviz-dot-preview))

(add-hook 'graphviz-dot-mode-hook #'graphviz-setup)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'" . graphviz-dot-mode))

(provide 'graphviz-setup)

;; Local Variables:
;; End:

;; graphviz-setup.el ends here
