;; blueprint-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 12 December 2012, 12.12.2012 (!)
;; Description:

(eval-when-compile
  (require 'cl))

;; (require 'sexpy-highlight)

(defgroup blueprint-faces nil
  "Blueprint highlighting."
  :group 'faces)

(defface blueprint-keyword-face
    '((t (:inherit font-lock-keyword-face)))
  "Face to highlight keywords."
  :group 'blueprint-faces)

(defface blueprint-constant-face
    '((t (:inherit font-lock-constant-face)))
  "Face to highlight constants."
  :group 'blueprint-faces)

(defface blueprint-global-variable-face
    '((t (:inherit font-lock-variable-name-face)))
  "Face to highlight global variables."
  :group 'blueprint-faces)

(defface blueprint-variable-face
    '((t (:inherit font-lock-variable-name-face)))
  "Face to highlight variables"
  :group 'blueprint-faces)

(defface blueprint-function-face
    '((t (:inherit font-lock-function-name-face)))
  "Face to highlight functions."
  :group 'blueprint-faces)

(defface blueprint-predicate-face
    '((t (:inherit font-lock-function-name-face)))
  "Face to highlight predicates and other ending with question mark"
  :group 'blueprint-faces)

(defface blueprint-mutating-op-face
    '((t (:inherit font-lock-function-name-face)))
  "Face to highlight mutating operations and other ending with exclamation mark"
  :group 'blueprint-faces)

(defface blueprint-doc-face
    '((t (:inherit font-lock-doc-face)))
  "Face to highlight documentation."
  :group 'blueprint-faces)

(defconst +blueprint-mode-font-lock-keywords+
  `((,(rxx ((int (seq (? (regexp "[+-]"))
                      (+ digit)))
            ;; (binary      (seq "#b" (+ (regexp "[01]"))))
            ;; (octal       (seq "#o" (+ (regexp "[0-7]"))))
            ;; (hexadecimal (seq "#x" (+ (regexp "[0-9abcdefABCDEF]"))))
            (float-point (seq (? (regexp "[+-]"))
                              (or (seq (+ digit)
                                       (? "." (* digit)))
                                  (seq (* digit)
                                       "." (+ digit)))))
            (float       (seq float-point
                              (? (regexp "[eE]")
                                 float-point)))
            (number (seq (or (seq symbol-start
                                  (or int
                                      float))
                             ;; binary
                             ;; octal
                             ;; hexadecimal
                             )
                         symbol-end)))
           number)
     (0 'blueprint-constant-face))

    (,(rx symbol-start
          (or "nil"
              "t")
          symbol-end)
     (0 'blueprint-constant-face))

    (,(rx "("
          symbol-start
          (group
           (or "lambda"
               "quote"
               "if"
               "cond"
               "begin"
               "and"
               "or"
               "let"))
          symbol-end)
     (1 'blueprint-keyword-face))

    (,(rx "("
          (group "define")
          (+ whitespace)
          symbol-start
          (group (+ (or (syntax word) (syntax symbol))))
          symbol-end)
     (1 'blueprint-keyword-face)
     (2 'blueprint-variable-face))

    (,(rx symbol-start
          "+" (+ (or (syntax word) (syntax symbol))) "+"
          symbol-end)
     (0 'blueprint-constant-face))
    (,(rx symbol-start
          "*" (+ (or (syntax word) (syntax symbol))) "*"
          symbol-end)
     (0 'blueprint-global-variable-face))
    (,(rx symbol-start
          (+ (or (syntax word) (syntax symbol))) "?"
          symbol-end)
     (0 'blueprint-predicate-face))
    (,(rx symbol-start
          (+ (or (syntax word) (syntax symbol))) "!"
          symbol-end)
     (0 'blueprint-mutating-op-face))
    (,(rx symbol-start
          (+ (or (syntax word) (syntax symbol))) ":"
          symbol-end)
     (0 'blueprint-keyword-face))

    ;; make pretty lambdas
    ("(\\(lambda\\)\\_>"
     (0 (prog1 nil
          (compose-region (match-beginning 1)
                          (match-end 1)
                          ,(make-char 'greek-iso8859-7 107)))))
    ;; ensure that pretty lambda goes away as soon as we type something after it
    ("(lambda[^ \n\t\v\f]"
     (0 (unless (memq (get-text-property (match-beginning 0)
                                         'face)
                      '(font-lock-comment-face
                        font-lock-string-face))
          (decompose-region (match-beginning 0)
                            (match-end 0))
          nil)))))

(defconst blueprint-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\t "-" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\f "-" table)
    (modify-syntax-entry ?\r "-" table)
    (modify-syntax-entry ?\s "-" table)

    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\] "(]" table)
    (modify-syntax-entry ?\[ ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    (dolist (c (string->list ":<>.-_+=!%^&*|/?~"))
      (modify-syntax-entry c "_" table))

    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\\ "\\" table)

    (modify-syntax-entry ?#  "'" table)
    (modify-syntax-entry ?$  "'" table)
    (modify-syntax-entry ?'  "'" table)
    (modify-syntax-entry ?,  "'" table)
    (modify-syntax-entry ?@  "'" table)
    (modify-syntax-entry ?`  "'" table)

    (modify-syntax-entry (cons ?0 ?9) "w" table)
    (modify-syntax-entry (cons ?A ?Z) "w" table)
    (modify-syntax-entry (cons ?a ?z) "w" table)
    table))

(define-derived-mode blueprint-mode prog-mode "Blueprint"
  "Major mode for Blueprint files"
  :syntax-tabel blueprint-mode-syntax-table
  (setq-local comment-style 'indent)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-padding " ")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-use-global-state t)

  (setq-local indent-line-function 'lisp-indent-line)

  (setf font-lock-defaults
        '(+blueprint-mode-font-lock-keywords+
          nil
          ;; be case-sensetive
          nil
          (("+-*/.<>=!?$%_&~^:@" . "w"))
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function))))

(provide 'blueprint-mode)

;; Local Variables:
;; End:

;; blueprint-mode.el ends here
