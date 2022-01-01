;; diff-mode-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 28 August 2014
;; Description:

(eval-when-compile
  (require 'macro-util))

(declare-function diff-hunk-kill "diff-mode")
(declare-function server-edit "server")

(require 'common)
(require 'folding-setup)
(require 'hydra-setup)

;;;###autoload
(add-to-list
 'hs-special-modes-alist
 (list 'diff-mode
       ;; start regex
       (rx (or "[" "(" "{"))
       ;; end regex
       nil
       ;; comment-start regex
       (rx bol
           (repeat 3 (or "-" "+"))
           " ")
       ;; forward-sexp function
       nil
       ;; adjust beg function
       nil))

(defhydra-derive hydra-diff-vim-normal-g-ext hydra-vim-normal-g-ext (:exit t :foreign-keys nil :hint nil)
  "
_,_: kill hunk"
  ("," diff-hunk-kill))

;;;###autoload
(defun diff-mode-setup ()
  (init-common :use-yasnippet nil
               :use-comment nil
               :use-hl-line t
               :use-whitespace 'tabs-only)
  (setup-folding t nil)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("<up>"     diff-hunk-prev)
    ("<down>"   diff-hunk-next)
    ("C-<up>"   diff-file-prev)
    ("C-<down>" diff-file-next)
    ("SPC"      diff-goto-source)
    ("<return>" diff-goto-source)
    ("g"        hydra-diff-vim-normal-g-ext/body)))

;;;###autoload
(add-hook 'diff-mode-hook #'diff-mode-setup)

(provide 'diff-mode-setup)

;; Local Variables:
;; End:

;; diff-mode-setup.el ends here
