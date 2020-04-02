;; haskell-outline.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(require 'haskell-misc)
(require 'search)
(require 'yafolding)

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

;;;###autoload
(defun* haskell-setup-folding (&key (enable-hs-minor-mode t))
  "Configure folding for Haskell. ENABLE-HS-MINOR-MODE controls whether
to enable folding of balanced S-expressions."
  (setup-folding enable-hs-minor-mode '(:header-symbol "-" :length-min 3))
  (yafolding-mode +1)
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ..."))

  (def-keys-for-map vim:normal-mode-local-keymap
    ("z C" yafolding-hide-all)
    ("z c" haskell-hide-indented-or-sexp)
    ("z O" yafolding-show-all)
    ("z o" haskell-show-indented-or-sexp)
    ("z T" yafolding-toggle-all))
  (def-keys-for-map vim:visual-mode-local-keymap
    ("z c" yafolding-hide-region)))

;;;###autoload
(defun haskell-hide-indented-or-sexp ()
  (interactive)
  (if (and hs-minor-mode
           (haskell-outline-on-sexp?))
      (hs-hide-block)
    (yafolding-hide-element)))

;;;###autoload
(defun haskell-show-indented-or-sexp ()
  (interactive)
  (if (and hs-minor-mode
           (haskell-outline-on-sexp?))
      (hs-show-block)
    (yafolding-show-element)))

;;;###autoload
(defun haskell-outline-on-sexp? ()
  (let ((next-char (char-after)))
    (and next-char
         (let ((synt (char-syntax next-char)))
           (or (char-equal synt ?\()
               (char-equal synt ?\)))))))

(provide 'haskell-outline)

;; Local Variables:
;; End:

;; haskell-outline.el ends here
