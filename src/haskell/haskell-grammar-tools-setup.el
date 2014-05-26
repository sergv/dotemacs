;; haskell-grammar-tools-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 13 May 2014
;; Description:

(require 'common)
(require 'mmm-auto)

(setf mmm-global-mode 'maybe
      mmm-submode-decoration-level 0)

(setf auto-mode-alist (cdr auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.x\\'" . alex-mode))
(add-to-list 'auto-mode-alist '("\\.happy\\'" . happy-mode))
(add-to-list 'auto-mode-alist '("\\.ly\\'" . happy-mode))

(mmm-add-mode-ext-class 'alex-mode "\\.x\\'" 'haskell-blocks)
(mmm-add-mode-ext-class 'happy-mode "\\.y\\'" 'haskell-blocks)

(defconst haskell-blocks-default-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}1nb" tbl)
    (modify-syntax-entry ?\} "){4nb" tbl)
    (modify-syntax-entry ?\' "."     tbl)
    (modify-syntax-entry ?\" "\""    tbl)
    (modify-syntax-entry ?-  ". 123" tbl)
    (modify-syntax-entry ?\n ">"     tbl)
    tbl))

(defun haskell-blocks-verify-location ()
  (with-syntax-table haskell-blocks-default-syntax-table
    (let* ((state (parse-partial-sexp (point-min)
                                      (point)))
           (parens-depth (nth 0 state))
           (inside-string? (nth 3 state)))
      (and (= 1 parens-depth)
           (not inside-string?)))))

(defun haskell-blocks-verify-front ()
  (save-excursion
    (goto-char (match-end 0))
    (when (not (and (= ?\{ (char-before))
                    (= ?- (char-after))))
      (haskell-blocks-verify-location))))

(defun haskell-blocks-verify-back ()
  (save-excursion
    (goto-char (match-beginning 0))
    (unless (= ?- (char-before))
      (haskell-blocks-verify-location))))

(defun haskell-blocks-find-back (bound)
  (with-syntax-table
      ;; haskell-mode-syntax-table
      haskell-blocks-default-syntax-table
    (let ((p (point)))
      (goto-char (match-beginning 0))
      (forward-sexp)
      (backward-char)
      (if (< bound (point))
        nil
        (looking-at "")))))

(mmm-add-classes
 '((haskell-blocks
    :submode haskell-mode
    ;;:face mmm-output-submode-face
    :front "{%?"
    :front-verify haskell-blocks-verify-front
    :back haskell-blocks-find-back
    :back-verify haskell-blocks-verify-back
    ;;:include-front t
    ;;:front-offset 5
    )))

(defun haskell-grammar-tools-setup ()
  (init-common :use-yasnippet nil :use-render-formula nil)
  (hs-minor-mode 1))

(add-hook 'alex-mode-hook #'haskell-grammar-tools-setup)
(add-hook 'happy-mode-hook #'haskell-grammar-tools-setup)

(provide 'haskell-grammar-tools-setup)

;; Local Variables:
;; End:

;; haskell-grammar-tools-setup.el ends here
