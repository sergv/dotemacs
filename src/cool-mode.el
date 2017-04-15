;; cool-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  8 May 2014
;; Description:

(require 'common)

(defparameter cool-mode-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\" "\""    tbl)
    ;; ensure that (*/*) are comments
    (modify-syntax-entry ?\( "()1nb" tbl)
    (modify-syntax-entry ?\) ")(4nb" tbl)
    (modify-syntax-entry ?* ". 23"   tbl)
    (modify-syntax-entry ?- ". 123"  tbl)
    (modify-syntax-entry ?! "."      tbl)
    (modify-syntax-entry ?~ "."      tbl)
    (modify-syntax-entry ?+ "."      tbl)
    (modify-syntax-entry ?- "."      tbl)
    (modify-syntax-entry ?= "."      tbl)
    (modify-syntax-entry ?% "."      tbl)
    (modify-syntax-entry ?< "."      tbl)
    (modify-syntax-entry ?> "."      tbl)
    (modify-syntax-entry ?& "."      tbl)
    (modify-syntax-entry ?| "."      tbl)
    (modify-syntax-entry ?@ "."      tbl)
    (modify-syntax-entry ?_ "_"      tbl)
    tbl))

(setf cool-font-lock-keywords
      `((,(rx (or "isvoid"
                  "not"))
         (0 font-lock-builtin-face))
        (,(rx (or (group-n 1 "SELF_TYPE")
                  (seq (or ":"
                           "@"
                           "class"
                           "inherits"
                           "new")
                       (* whitespace)
                       (group-n 1 (regex "[A-Z][A-Za-z0-9_]*")))))
         (1 font-lock-type-face))
        (,(rx bow
              (or
               ;; keywords
               "class"
               "inherits"
               "new"
               ;; statements
               "self"
               "let"
               "in"
               ;; conditionals
               "if"
               "then"
               "else"
               "fi"
               "case"
               "esac"
               "of"
               ;; looping
               "while"
               "loop"
               "pool"
               ;; booleans
               "true"
               "false")
              eow)
         (0 font-lock-keyword-face))
        (,(rx symbol-start
              (? (or "-" "+"))
              (+ (regex "[0-9]"))
              symbol-end)
         (0 font-lock-constant-face))))

;;;###autoload
(define-derived-mode cool-mode prog-mode "Cool"
  "Major mode to edit .gitconfig files"
  (setq-local comment-start "--+")
  ;; (setq-local comment-end "\\*)")
  (setq-local comment-start-skip "(\\*+ *\\|--+ *")

  (setq-local indent-tabs-mode nil)
  (setq-local font-lock-defaults
              '(cool-font-lock-keywords
                nil ;; perform syntactic fontification (e.g. strings, comments)
                )))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx (or ".cl"
                           ".cool")
                       eol)
                   'cool-mode))

(provide 'cool-mode)

;; Local Variables:
;; End:

;; cool-mode.el ends here
