;; diff-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 28 August 2014
;; Description:

(require 'common)

;;;###autoload
(defun diff-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-whitespace 'tabs-only)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<up>"     diff-hunk-prev)
    ("<down>"   diff-hunk-next)
    ("C-<up>"   diff-file-prev)
    ("C-<down>" diff-file-next)
    ("SPC"      diff-goto-source)
    ("<return>" diff-goto-source)
    ("g ,"      diff-hunk-kill)))

;;;###autoload
(add-hook 'diff-mode-hook #'diff-mode-setup)

(provide 'diff-mode-setup)

;; Local Variables:
;; End:

;; diff-mode-setup.el ends here
