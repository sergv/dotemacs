;; haskell-outline.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'haskell-misc)
(require 'search)
(require 'yafolding)
(require 'vim-setup)

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

(defhydra-derive hydra-haskell-vim-normal-z hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: hide indented or sexp  _f_: hide outline block
_o_: show indented or sexp  _u_: show outline block
_C_: hide all indented      _F_: hide all outline blocks leaving all headings visible
_O_: show all indented      _U_: show all outline blocks
_T_: toggle all indented"
  ("c" haskell-hide-indented-or-sexp)
  ("o" haskell-show-indented-or-sexp)
  ("C" yafolding-hide-all)
  ("O" yafolding-show-all)
  ("T" yafolding-toggle-all)

  ("F" outline-hide-body)
  ("f" outline-hide-subtree)
  ("U" outline-show-all)
  ("u" outline-show-subtree))

(defhydra-derive hydra-haskell-vim-visual-z hydra-vim-visual-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_c_: yafolding hide region"
  ("c" yafolding-hide-region))

;;;###autoload
(cl-defun haskell-setup-folding (&key (enable-hs-minor-mode t))
  "Configure folding for Haskell. ENABLE-HS-MINOR-MODE controls whether
to enable folding of balanced S-expressions."
  (setup-folding enable-hs-minor-mode '(:header-symbol "-" :length-min 3))
  (yafolding-mode +1)
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ..."))

  (def-keys-for-map vim-normal-mode-local-keymap
    ("z" hydra-haskell-vim-normal-z/body))
  (def-keys-for-map vim-visual-mode-local-keymap
    ("z" hydra-haskell-vim-visual-z/body)))

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
