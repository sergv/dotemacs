;; scheme-highlight.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  5 January 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'common)


(defface scheme-constant-face
    '((t (:inherit font-lock-constant-face)))
  "Face to highlight conventional constants,
i.e. names matching regexp \\+.*\\+."
  :group 'scheme-faces)

(defface scheme-global-variable-face
    '((t (:inherit font-lock-variable-name-face)))
  "Face to highlight conventional global variables,
i.e. names matching regexp \\*.*\\*."
  :group 'scheme-faces)

(defface scheme-declaration-face
    '((t (:inherit font-lock-constant-face)))
  "Face to highlight ansi cl declarations."
  :group 'scheme-faces)

(defface scheme-predicate-face
    '((t (:inherit font-lock-function-name-face)))
  "Face to highlight ansi cl declarations."
  :group 'scheme-faces)

(defface scheme-mutating-op-face
    '((t (:inherit font-lock-function-name-face)))
  "Face to highlight ansi cl declarations."
  :group 'scheme-faces)


(defun scheme-highlight (&optional mode)
  (font-lock-remove-keywords
   mode
   `(("\\<<\\sw+>\\>" . font-lock-type-face)
     ("\\<#?:\\sw+\\>" . font-lock-builtin-face)
     ("(let\\s-+\\(\\sw+\\)"
      (1 font-lock-function-name-face))))

  (font-lock-add-keywords
   mode
   `(;; support nearly full numeric tower
     (,(rxx ((int-or-rat (seq (? (regexp "[+-]"))
                              (+ digit)
                              ;; now rationals go here
                              (? "/"
                                 (+ digit))))
             (binary      (seq "#b" (+ (regexp "[01]"))))
             (octal       (seq "#o" (+ (regexp "[0-7]"))))
             (hexadecimal (seq "#x" (+ hex-digit)))
             ;; (rational    (seq integer "/" integer))
             (float       (seq (? (regexp "[+-]"))
                               (or (seq (+ digit)
                                        (? "." (* digit)))
                                   (seq (* digit)
                                        "." (+ digit)))
                               (? (regexp "[deDE]")
                                  (? (regexp "[+-]"))
                                  (or (seq (+ digit)
                                           (? "." (* digit)))
                                      (seq (* digit)
                                           "." (+ digit)))))))
         (seq bow
              (or int-or-rat
                  binary
                  octal
                  hexadecimal
                  ;; rational
                  float)
              eow)) 0 'scheme-constant-face)

     ("\\_<\\+\\(?:\\sw\\|\\s_\\)+\\+\\_>"
      (0 'scheme-constant-face))
     ("\\_<\\*\\(?:\\sw\\|\\s_\\)+\\*\\_>"
      (0 'scheme-global-variable-face))

     ("\\_<\\(?:#f\\|#t\\)\\_>"
      (0 'scheme-constant-face))

     ("\\_<\\(?:\\sw\\|\\s_\\)+\\?\\_>"
      (0 'scheme-predicate-face))
     ("\\_<\\(?:\\sw\\|\\s_\\)+!\\_>"
      (0 'scheme-mutating-op-face))

     ;; dsssl named constants arguments
     ("\\_<#!\\(?:optional\\|key\\|rest\\)\\_>"
      (0 'font-lock-keyword-face))

     (,(rx symbol-start
           (or (seq "wrong-type-arg"
                    (? "ument"))
               "misc-error"
               "out-of-range"
               "system-error")
           symbol-end)
      (0 'font-lock-constant-face))

     ;; fixed scheme-mode highlights
     ("(let\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 'font-lock-function-name-face))
     ("\\_<<\\(?:\\sw\\|\\s_\\)+>\\_>"
      (0 'font-lock-type-face))
     (,(rxx ((symbol-regexp
               (regexp "\\(?:\\sw\\|\\s_\\)+"))
             (keyword-prefix (regexp "\\(?:#?:\\)"))
             (keyword-suffix ":"))
         symbol-start
         (or (seq keyword-prefix
                  symbol-regexp
                  (? keyword-suffix))
             (seq symbol-regexp
                  keyword-suffix))
         symbol-end)
      (0 'font-lock-keyword-face))

     (,(rx "("
           (group
            (or "scm-error"
                "error"
                "false-if-exception"
                "assert"
                (seq "assert-"
                     (+ (or (syntax word)
                            (syntax symbol)))))
            symbol-end))
      (1 'font-lock-warning-face))

     ;; make pretty lambdas
     ("(\\(lambda\\_>\\)"
      (0 (begin
           (compose-region (match-beginning 1)
                           (match-end 1)
                           ,(make-char 'greek-iso8859-7 107))
           nil)))

     ;; ensure that pretty lambda goes away as soon
     ;; as we type something after it
     ("(lambda[^ \n\t\v\f]"
      (0 (begin
           (decompose-region (match-beginning 0)
                             (match-end 0))
           nil))))))


(provide 'scheme-highlight)

;; Local Variables:
;; End:

;; scheme-highlight.el ends here
