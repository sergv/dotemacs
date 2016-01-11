;; haskell-outline.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'search)
(require 'yafolding)
(require 'haskell-misc)
(require 'search)

(defadvice yafolding-go-parent-element
    (after
     yafolding-go-parent-element/skip-whitespace
     activate
     compile)
  (skip-to-indentation))

(setf yafolding-show-fringe-marks nil)

(search-def-autoexpand-advices
 (yafolding-show-element)
 (haskell-mode))

(defun haskell-setup-folding ()
  (hs-minor-mode +1)
  (yafolding-mode +1)
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ...\n"))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("z C" yafolding-hide-all)
    ("z c" haskell-hide-indented-or-sexp)
    ("z O" yafolding-show-all)
    ("z o" haskell-show-indented-or-sexp)
    ("z T" yafolding-toggle-all))
  (def-keys-for-map vim:visual-mode-local-keymap
    ("z c" yafolding-hide-region)))

(defun haskell-hide-indented-or-sexp ()
  (interactive)
  (if (haskell-outline-on-sexp?)
    (hs-hide-block)
    (yafolding-hide-element)))

(defun haskell-show-indented-or-sexp ()
  (interactive)
  (if (haskell-outline-on-sexp?)
    (hs-show-block)
    (yafolding-show-element)))

(defun haskell-outline-on-sexp? ()
  (let ((synt (char-syntax (char-after))))
    (or (char=? synt ?\()
        (char=? synt ?\)))))

(provide 'haskell-outline)

;; Local Variables:
;; End:

;; haskell-outline.el ends here
