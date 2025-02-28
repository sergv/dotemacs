;; alex-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 12 May 2014
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)

(require 'polymode)
(require 'alex-happy-utils)

(defun poly-alex-find-front (direction)
  (poly-alex-happy-find-front direction t))

(define-innermode poly-alex-haskell-innermode
  :mode 'haskell-mode
  :head-matcher #'poly-alex-find-front
  :tail-matcher #'poly-alex-happy-find-tail
  :head-mode 'host
  :tail-mode 'host
  :allow-nested nil
  :can-nest nil
  :protect-syntax t)

(define-hostmode poly-alex-hostmode
  :mode 'alex-grammar-mode)

;;;###autoload (autoload 'alex-mode "alex-mode" nil t)
(define-polymode alex-mode
  :hostmode 'poly-alex-hostmode
  :innermodes '(poly-alex-haskell-innermode)
  (setq-local poly-lock-allow-background-adjustment nil))

(defconst alex-colon-column 16 "\
*The column in which to place a colon separating a token from its definition.")

(defconst alex-percent-column 41 "\
*The column in which to place a percent introducing a modifier (e.g. %prec).")

(defvar alex-grammar-mode-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Keymap used in alex mode.")

(defvar alex-grammar-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14"  tbl)
    (modify-syntax-entry ?* ". 23"  tbl)
    (modify-syntax-entry ?\{ "(}  " tbl)
    (modify-syntax-entry ?\} "){  " tbl)
    (modify-syntax-entry ?\\ "\\  " tbl)
    (modify-syntax-entry ?\' "."    tbl)
    (modify-syntax-entry ?\" "\""   tbl)
    (modify-syntax-entry ?\: "."    tbl)
    (modify-syntax-entry ?\; "."    tbl)
    (modify-syntax-entry ?-  ". 12" tbl)
    (modify-syntax-entry ?\n ">"    tbl)
    tbl)
  "Syntax table in use in alex-grammar-mode buffers.")

(defvar alex-grammar-mode-font-lock-keywords
  `((,(rx (or "%wrapper"
              ":-"))
     (0 'font-lock-keyword-face))
    (,(rx bol
          "<"
          (+ (regexp "[^>]"))
          ">")
     (0 'font-lock-function-name-face))
    (,(rx (or (seq (or "$"
                       "@")
                   (* (regexp "[a-zA-Z_]")))
              "["
              "]"
              "\\"
              "^"))
     (0 'font-lock-constant-face))
    ;; regexp metacharacters
    (,(rx (or "*"
              "+"
              "."
              "|"
              "?"
              "\\"
              "("
              ")"))
     (0 'font-lock-negation-char-face)))
  "Highlight definitions of alex distinctive constructs for font-lock.")

;;;###autoload
(define-derived-mode alex-grammar-mode prog-mode "Alex"
  "Major mode for editing Alex files."
  (setq-local font-lock-defaults '(alex-grammar-mode-font-lock-keywords))

  (setq-local paragraph-start (concat "^$\\|" page-delimiter)
              paragraph-separate paragraph-start
              paragraph-ignore-fill-prefix t
              indent-line-function 'alex-indent-line
              require-final-newline t
              comment-start "--"
              comment-end ""
              comment-column 32
              comment-start-skip "--+ *"
              parse-sexp-ignore-comments t
              ;; selective-display t
              ;; selective-display-ellipses t
              )

  (make-local-variable 'block-indent-level)
  (make-local-variable 'auto-fill-hook)

  (setq-local parse-sexp-lookup-properties t))

;;;;

(provide 'alex-mode)

;; alex-mode.el ends here
