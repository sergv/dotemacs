;; uuag-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 17 March 2018
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'haskell-mode)
(require 'haskell-syntax-table)

(require 'common)
(require 'solarized)

(defvar uuag-mode-syntax-table
  (let ((tbl (make-syntax-table haskell-mode-syntax-table)))
    ;; (modify-syntax-entry ?\n ">"    tbl)
    tbl)
  "Syntax table in use in alex-mode buffers.")

(defgroup uuag nil
  "UUAG mode"
  :group 'uuag)

(defface uuag-meta-keyword-face
  `((t (:inherit font-lock-keyword-face)))
  "Face to highlight keywords specific to UUAG."
  :group 'uuag)

(defface uuag-field-name-face
  `((t (:foreground ,+solarized-orange+ :bold t)))
  "Face to highlight references to other attributes (i.e. things that start with @)."
  :group 'uuag)

(defvar uuag-mode-font-lock-keywords
  `(("@[[:lower:]_][[:alnum:]'_]*"
     (0 'uuag-field-name-face))

    (,(rx bol
          (or "attr"
              "data"
              "deriving"
              "extends"
              "include"
              "imports"
              "optpragmas"
              "pragma"
              "sem"
              "set"
              "type"

              "ATTR"
              "EXTENDS"
              "DATA"
              "DERIVING"
              "INCLUDE"
              "PRAGMA"
              "SEM"
              "SET"
              "TYPE")
          symbol-end)

     (0 'uuag-meta-keyword-face))

    (,(rx symbol-start
          (or "inh" ;; Inherited attirbute.
              "chn" ;; Threaded attribute.
              "syn" ;; Synthesized attribute.
              "lhs"
              "loc"
              "USE")
          symbol-end)

     (0 'uuag-meta-keyword-face))

    ,@(haskell-font-lock-keywords))
  "Highlight distinctive UUAG constructs via font-lock.")

;;;###autoload
(define-derived-mode uuag-mode prog-mode "UUAG"
  "Major mode for editing Alex files."
  (setq-local font-lock-defaults
              '(uuag-mode-font-lock-keywords
                nil ;; perform syntactic fontification
                nil ;; do not ignore case
                nil ;; no special syntax provided
                )

              paragraph-start (concat "^$\\|" page-delimiter)
              paragraph-separate paragraph-start
              paragraph-ignore-fill-prefix t
              require-final-newline t
              comment-start "--"
              comment-end ""
              comment-column 32
              comment-start-skip "--+ *"
              parse-sexp-ignore-comments t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ag\\'" . uuag-mode))

(provide 'uuag-mode)

;; Local Variables:
;; End:

;; uuag-mode.el ends here
