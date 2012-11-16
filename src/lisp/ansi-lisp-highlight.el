;; ansi-lisp-highlight.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 29 December 2011
;; Keywords:
;; Requirements:
;; Status:

(require 'common)

(defgroup ansi-lisp nil
  "Ansi Common Lisp highlighting."
  :group 'faces)

(defface ansi-lisp-constant-face
    '((t (:inherit font-lock-constant-face)))
  "Face to highlight conventional and other constants,
i.e. names matching regexp \\+.*\\+., self-evaluating symbols, numbers."
  :group 'ansi-lisp)

(defface ansi-lisp-keyword-face
    '((t (:inherit font-lock-keyword-face)))
  "Face to highlight both :keywodrs and function calls to standard functions."
  :group 'ansi-lisp)

(defface ansi-lisp-warning-face
    '((t (:inherit font-lock-warning-face)))
  "Face to highlight e.g. error, signal, assert."
  :group 'ansi-lisp)

(defface ansi-lisp-global-variable-face
    '((t (:inherit font-lock-variable-name-face)))
  "Face to highlight conventional global variables,
i.e. names matching regexp \\*.*\\*."
  :group 'ansi-lisp)

(defface ansi-lisp-declaration-face
    '((t (:foreground "#6c71c4")))
  "Face to highlight ansi cl declarations."
  :group 'ansi-lisp)

(defface ansi-lisp-type-face
    '((t (:inherit font-lock-type-face)))
  "Face to highlight ansi cl types."
  :group 'ansi-lisp)


(defface ansi-lisp-expression-face
    '((t (:inherit font-lock-builtin-face)))
  "Face to highlight ansi cl expressions
\(I'm wrong or is declare is the only expression defined?\)."
  :group 'ansi-lisp)

(defface ansi-lisp-special-form-face
    '((t (:inherit font-lock-builtin-face)))
  "Face to highlight ansi cl special forms."
  :group 'ansi-lisp)

(defface ansi-lisp-macro-face
    '((t (:inherit font-lock-builtin-face)))
  "Face to highlight ansi cl builtin macro."
  :group 'ansi-lisp)

(defface ansi-lisp-generic-function-face
    '((t (:inherit font-lock-builtin-face)))
  "Face to highlight ansi cl builtin generic functions."
  :group 'ansi-lisp)

(defface ansi-lisp-function-face
    '((t (:inherit font-lock-builtin-face)))
  "Face to highlight ansi cl builtin functions."
  :group 'ansi-lisp)

(defface ansi-lisp-predicate-face
    '((t (:inherit font-lock-function-name-face)))
  "Face to highlight Scheme-like predicates: eq? and other ending with
question mark"
  :group 'ansi-lisp)

(defface ansi-lisp-mutating-op-face
    '((t (:inherit font-lock-function-name-face)))
  "Face to highlight Scheme-like mutating operations: set! and other ending
with exclamation mark"
  :group 'ansi-lisp)


(defface ansi-lisp-format-directive-face
    '((t (:inherit font-lock-negation-char-face)))
  "Face to highlight ansi cl builtin functions."
  :group 'ansi-lisp)

(defface ansi-lisp-loop-keyword-face
    '((t (:inherit font-lock-builtin-face)))
  "Face to highlight ansi cl loop keywords."
  :group 'ansi-lisp)

(defface ansi-lisp-symbols-without-home-package-face
    '((t (:inherit ansi-lisp-constant-face)))
  "Face to highlight #:symbols which have to home package deliberately
or because the're uninterned yet."
  :group 'ansi-lisp)


(defface ansi-lisp-defined-name-face
    '((t (:inherit font-lock-function-name-face)))
  "Face to highlight names of entities that are being defined by form
such as defun, defmacro, etc."
  :group 'ansi-lisp)

(defface ansi-lisp-defined-data-name-face
    '((t (:foreground "#cb4b16")))
  "Face to highlight names of data entities being defined, e.g. by defstruct,
defclass etc."
  :group 'ansi-lisp)



;; (defface ansi-lisp-package-name-face
;;     '((t (:foreground "#cb4b16")))
;;   "Face to highlight package names."
;;   :group 'ansi-lisp)

(defface ansi-lisp-doc-face
    '((t (:inherit font-lock-doc-face)))
  "Face to highlight documentation."
  :group 'ansi-lisp)

(defface ansi-lisp-exported-symbols-face
    '((t (:foreground "#2aa198")))
  "Face to highlight exported, shadowed, interned, imported etc names in
defpackage."
  :group 'ansi-lisp)

;;;;

(defconst +ansi-lisp-format-directive-regexp+
  (rxx ((digit-argument (+ digit))
        (char-argument (seq "'"
                            (in graph print alnum)))
        (other-argument (regexp "[v#]"))
        (prefix-param (or digit-argument
                          char-argument
                          other-argument))
        (modifier (or "@"
                      ":"))
        ;; ~C takes to parameters but may take modifiers
        (character-directive
          (seq (* modifier)
               (or "c" "C")))
        (integer-directive
          (seq
           (? (or digit-argument other-argument))
           (? ","
              (? (or char-argument other-argument))
              (? ","
                 (? (or char-argument other-argument))
                 (? ","
                    (? (or digit-argument other-argument))
                    )))
           (* modifier)
           (regexp "[dDxXoObB]")))
        (number-between-2-and-36
          (or "2" "3" "4" "5" "6" "7" "8" "9" "10"
              "11" "12" "13" "14" "15" "16" "17" "18" "19" "20"
              "21" "22" "23" "24" "25" "26" "27" "28" "29" "30"
              "31" "32" "33" "34" "35" "36"))
        (radix-directive
          (seq
           (? number-between-2-and-36)
           (? ","
              (? (or digit-argument other-argument))
              (? ","
                 (? (or char-argument other-argument))
                 (? ","
                    (? (or char-argument other-argument))
                    (? ","
                       (? (or digit-argument other-argument))
                       ))))
           (* modifier)
           (regexp "[rR]")))
        (conditional-formatting-directive
          (seq (? (or digit-argument other-argument))
               (* modifier)
               (or "[" "]" ";")))
        (iteration-directive
          (seq (* modifier)
               (or "{" "}" "^")))
        (general-directive
          (seq (? prefix-param)
               (* ","
                  (? prefix-param))
               (* modifier)
               (regexp "[aAsSfFeEgG$pP()%&~*?/\n]"))))
    "~"
    (or character-directive
        integer-directive
        radix-directive
        conditional-formatting-directive
        iteration-directive
        general-directive)))

(make-highlight-procedure
 ansi-lisp-highlight-format-directives
 +ansi-lisp-format-directive-regexp+
 #'lisp-point-inside-format-or-error-stringp)


(defconst +ansi-lisp-loop-keywords+
  (rx symbol-start
      (? ":")
      (or "above"
          "across"
          "always"
          "and"
          "append"
          "appending"
          "being"
          "below"
          "by"
          "collect"
          "collecting"
          "count"
          "counting"
          "do"
          "downfrom"
          "downto"
          "else"
          "finally"
          "for"
          "from"
          "if"
          "in"
          "initially"
          "into"
          "maximize"
          "maximizing"
          "minimize"
          "minimizing"
          "named"
          "nconc"
          "nconcing"
          "never"
          "on"
          "repeat"
          "return"
          "sum"
          "summing"
          "the"
          "then"
          "thereis"
          "to"
          "unless"
          "until"
          "upfrom"
          "upto"
          "using"
          "where"
          "while"
          "with")
      symbol-end))

(make-highlight-procedure
 ansi-lisp-highlight-loop-keywords
 +ansi-lisp-loop-keywords+
 #'lisp-point-inside-loop-formp)


;; (defconst +ansi-lisp-declarations+
;;   (rx "("
;;       symbol-start
;;       (group
;;        (or "type"
;;            "compilation-speed"
;;            "debug"
;;            "declaration"
;;            "dynamic-extent"
;;            "ftype"
;;            "ignorable"
;;            "ignore"
;;            "inline"
;;            "notinline"
;;            "optimize"
;;            "safety"
;;            "space"
;;            "special"
;;            "speed"))
;;       symbol-end))

;; (make-highlight-procedure
;;  ansi-lisp-highlight-declarations
;;  +ansi-lisp-declarations+
;;  #'lisp-point-nested-inside-declaration-formp)


;;; Type handling

(defconst +ansi-lisp-types+
  (rx symbol-start
      (or "array"
          "base-char"
          "base-string"
          "bignum"
          "bit-vector"
          "boolean"
          "broadcast-stream"
          "built-in-class"
          "class"
          "compiled-function"
          "concatenated-stream"
          "double-float"
          "echo-stream"
          "extended-char"
          "file-stream"
          "fixnum"
          "generic-function"
          "hash-table"
          "integer"
          "keyword"
          "long-float"
          "method"
          "number"
          "package"
          "random-state"
          "ratio"
          "readtable"
          "real"
          "restart"
          "satisfies"
          "sequence"
          "short-float"
          "signed-byte"
          "simple-array"
          "simple-base-string"
          "simple-bit-vector"
          "simple-string"
          "simple-vector"
          "single-float"
          "standard-char"
          "standard-class"
          "standard-generic-function"
          "standard-method"
          "standard-object"
          "stream"
          "string-stream"
          "structure-class"
          "structure-object"
          "symbol"
          "synonym-stream"
          "two-way-stream"
          "unsigned-byte")
      symbol-end))

(make-highlight-procedure
 ansi-lisp-highlight-types
 +ansi-lisp-types+
 #'lisp-point-nested-inside-type-using-formp)

;;;;

(defconst +ansi-lisp-basic-keywords+
  (eval-when-compile
   `(;; support nearly full numeric tower
     ;; upd: full numeric tower is supported (? needs tests)
     ;; upd upd: rationals were not handled properly, merged with integers
     ;; - everything else seems to work fine
     (,(rxx ((int-or-rat (seq (? (regexp "[+-]"))
                              (+ digit)
                              ;; now rationals go here
                              (? "/"
                                 (+ digit))))
             (binary      (seq "#b" (+ (regexp "[01]"))))
             (octal       (seq "#o" (+ (regexp "[0-7]"))))
             (hexadecimal (seq "#x" (+ (regexp "[0-9abcdefABCDEF]"))))
             ;; (rational    (seq integer "/" integer))
             (float-point (seq (? (regexp "[+-]"))
                               (or (seq (+ digit)
                                        (? "." (* digit)))
                                   (seq (* digit)
                                        "." (+ digit)))))
             (float       (seq float-point
                               (? (regexp "[deDE]")
                                  float-point)))
             (number (seq (or (seq symbol-start
                                   (or int-or-rat
                                       float))
                              binary
                              octal
                              hexadecimal)
                          symbol-end))
             ;; (number (seq (or int-or-rat
             ;;                  binary
             ;;                  octal
             ;;                  hexadecimal
             ;;                  ;; rational
             ;;                  float)
             ;;              symbol-end))
             (complex (seq "#c("
                           (* whitespace)
                           number
                           (+ whitespace)
                           number
                           (* whitespace)
                           ")")))
         (or number
             complex))
      (0 'ansi-lisp-constant-face))

     ("\\_<\\(?:\\(?:[^: \t\n\r]+:\\)?\\(\\+[^+ \n\t\r\f]+\\+\\)\\)\\_>"
      (1 'ansi-lisp-constant-face))
     ("\\_<\\(?:\\(?:[^: \t\n\r]+:\\)?\\(\\*[^* \n\t\r\f]+\\*\\)\\)\\_>"
      (1 'ansi-lisp-global-variable-face))

     ;; handle both :keywords and #:sybols-without-home-package
     ("\\(#\\)?\\_<\\(\\:\\(?:\\s_\\|\\sw\\)+\\)\\_>"
      ;; symbols without home package, as special kind of constant
      (0 (when (re-group-matchedp 1)
           'ansi-lisp-symbols-without-home-package-face))
      ;; keywords
      (2 (unless (re-group-matchedp 1)
           'ansi-lisp-keyword-face)))

     ("\\_<\\(?:[^ \n\t]+\\)\\?\\_>" 0 'ansi-lisp-predicate-face)
     ("\\_<\\(?:[^ \n\t]+\\)!\\_>"   0 'ansi-lisp-mutating-op-face)

     ;; ("\\_<&\\(?:\\s_\\|\\sw\\)+\\_>" (0 'ansi-lisp-declaration-face))

     ;;make pretty lambdas
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
           nil)))

     ("\\_<\\(?:t\\|nil\\)\\_>" (0 'ansi-lisp-constant-face)))))


(defconst +ansi-lisp-advanced-keywords+
  (eval-when-compile
   `((,(rx "("
           (group
            (or "catch"
                "throw"
                "featurep"
                "provide"
                "require"))
           symbol-end
           (* (regexp "[ \t']"))
           (? (group symbol-start
                     (+ (or (syntax word) (syntax symbol)))
                     symbol-end)))
      (1 'ansi-lisp-keyword-face)
      (2 'ansi-lisp-constant-face t t))

     ;;;
     ;; (,(rx "("
     ;;       (group "defpackage")
     ;;       (+ (regexp "[ \t\n]"))
     ;;       (? symbol-start
     ;;          (group
     ;;           (+ (or (syntax word) (syntax symbol))))
     ;;          symbol-end))
     ;;  (1 'ansi-lisp-keyword-face)
     ;;  (2 'ansi-lisp-defined-data-name-face t))

     (,(rx "("
           (group
            (or "defclass"
                ;; "defstruct"
                "deftype"))
           (+ (regexp "[ \t\n]"))
           (? (? "(") ; imho for defstruct only
              symbol-start
              (group
               (+ (or (syntax word) (syntax symbol))))
              symbol-end))
      (1 'ansi-lisp-keyword-face)
      (2 'ansi-lisp-defined-data-name-face))

     (,(rx "("
           (group
            (or (group
                 ;; note: delte this group and fix group referencec below
                 "foobar"
                 ;; (or "defconstant"
                 ;;     "defparameter"
                 ;;     "defvar")
                 )
                (group
                 (or "defgeneric"
                     "define-compiler-macro"
                     "define-condition"
                     "define-method-combination"
                     "define-modify-macro"
                     "define-setf-expander"
                     "define-symbol-macro"
                     ;; "defmacro"
                     "defmethod"
                     "defsetf"
                     ;; "defun"
                     ))))
           (+ (regexp "[ \t\n]"))
           (? symbol-start
              (group (+ (or (syntax word) (syntax symbol))))
              symbol-end))

      (1 'ansi-lisp-keyword-face)
      (4 (cond ((re-group-matchedp 2)
                ;; here variables should go
                'ansi-lisp-defined-name-face)
               ((re-group-matchedp 3)
                ;; here function face ought to be
                'ansi-lisp-defined-name-face))))


     ;;;


     (,(rx "("
           (group
            (or "abort"
                "assert"
                "warn"
                "check-type"
                "cerror"
                "error"
                "signal"))
           symbol-end)
      (1 'ansi-lisp-warning-face))

     (,(rx "("
           (group
            (or "function"
                "block"
                "catch"
                ;; "flet"
                "go"
                "if"
                ;; "labels"
                ;; "let"
                ;; "let*"
                "load-time-value"
                "locally"
                ;; "macrolet"
                "multiple-value-call"
                "multiple-value-prog1"
                "progn"
                "progv"
                "quote"
                "return-from"
                "setq"
                ;; "symbol-macrolet"
                "tagbody"
                "the"
                "throw"
                "unwind-protect"))
           symbol-end)
      (1 'ansi-lisp-special-form-face))

     ;; (,(rx "("
     ;;       (group
     ;;        "eval-when"
     ;;        symbol-end)
     ;;       (+ whitespace)
     ;;       "("
     ;;       (group (* whitespace)
     ;;              (* ;; symbol-start
     ;;               (or "compile"
     ;;                   "load"
     ;;                   "eval"
     ;;                   ":compile-toplevel"
     ;;                   ":load-toplevel"
     ;;                   ":execute")
     ;;               symbol-end
     ;;               (* whitespace)))
     ;;       ")")
     ;;  (1 'ansi-lisp-special-form-face)
     ;;  ;; I like here to be orange, not blue as in ansi-lisp-declaration-face
     ;;  (2 'ansi-lisp-defined-data-name-face t))

     (,(rx "("
           symbol-start
           (group
            (or "lambda"
                "and"
                "or"
                "setf"
                "call-method"
                "case"
                "ccase"
                "cond"
                "ctypecase"
                "decf"
                ;; "destructuring-bind"
                ;; "do"
                "do-all-symbols"
                "do-external-symbols"
                "do-symbols"
                ;; "do*"
                ;; "dolist"
                ;; "dotimes"
                "ecase"
                "etypecase"
                "formatter"
                "handler-bind"
                "handler-case"
                "ignore-errors"
                ;; "in-package"
                "incf"
                "loop"
                "loop-finish"
                "make-method"
                ;; "multiple-value-bind"
                "multiple-value-list"
                "multiple-value-setq"
                "nth-value"
                "otherwise"
                "pop"
                "pprint-exit-if-list-exhausted"
                "pprint-logical-block"
                "pprint-pop"
                "print-unreadable-object"
                "prog"
                "prog1"
                "prog2"
                "prog*"
                "psetf"
                "psetq"
                "push"
                "pushnew"
                "remf"
                "restart-bind"
                "restart-case"
                "return"
                "rotatef"
                "shiftf"
                "step"
                "time"
                "trace"
                "typecase"
                "unless"
                "untrace"
                "when"
                "with-accessors"
                "with-compilation-unit"
                "with-condition-restarts"
                "with-hash-table-iterator"
                "with-input-from-string"
                "with-open-file"
                "with-open-stream"
                "with-output-to-string"
                "with-package-iterator"
                "with-simple-restart"
                "with-slots"
                "with-standard-io-syntax")
            symbol-end))
      (1 'ansi-lisp-macro-face))

     (,(rx "("
           symbol-start
           (group
            (or "add-method"
                "allocate-instance"
                "change-class"
                "class-name"
                "compute-applicable-methods"
                "find-method"
                "function-keywords"
                "initialize-instance"
                "make-instance"
                "make-instances-obsolete"
                "make-load-form"
                "method-qualifiers"
                "no-applicable-method"
                "no-next-method"
                "reinitialize-instance"
                "remove-method"
                "shared-initialize"
                "slot-missing"
                "slot-unbound"
                "update-instance-for-different-class"
                "update-instance-for-redefined-class"))
           symbol-end)
      (1 'ansi-lisp-generic-function-face))


     (,(rx (or "(" "#'")
           symbol-start
           (group
            (or "locally"
                "-"
                "/"
                "*"
                "+"
                "atom"
                "bit"
                "character"
                "complex"
                "cons"
                "continue"
                "eql"
                "float"
                "list"
                "logical-pathname"
                "member"
                "mod"
                "muffle-warning"
                "not"
                "null"
                "pathname"
                "rational"
                "store-value"
                "string"
                "use-value"
                "values"
                "vector"
                "/="
                "1-"
                "1+"
                "<"
                "<="
                "="
                ">"
                ">="
                "abs"
                "acons"
                "acos"
                "acosh"
                "adjoin"
                "adjust-array"
                "adjustable-array-p"
                "alpha-char-p"
                "alphanumericp"
                "append"
                "apply"
                "apropos"
                "apropos-list"
                "aref"
                "arithmetic-error-operands"
                "arithmetic-error-operation"
                "array-dimension"
                "array-dimensions"
                "array-displacement"
                "array-element-type"
                "array-has-fill-pointer-p"
                "array-in-bounds-p"
                "array-rank"
                "array-row-major-index"
                "array-total-size"
                "arrayp"
                "ash"
                "asin"
                "asinh"
                "assoc"
                "assoc-if"
                "assoc-if-not"
                "atan"
                "atanh"
                "bit-and"
                "bit-andc1"
                "bit-andc2"
                "bit-eqv"
                "bit-ior"
                "bit-nand"
                "bit-nor"
                "bit-not"
                "bit-orc1"
                "bit-orc2"
                "bit-vector-p"
                "bit-xor"
                "boole"
                "both-case-p"
                "boundp"
                "break"
                "broadcast-stream-streams"
                "butlast"
                "byte"
                "byte-position"
                "byte-size"
                "caaaar"
                "caaadr"
                "caaar"
                "caadar"
                "caaddr"
                "caadr"
                "caar"
                "cadaar"
                "cadadr"
                "cadar"
                "caddar"
                "cadddr"
                "caddr"
                "cadr"
                "call-next-method"
                "car"
                "cdaaar"
                "cdaadr"
                "cdaar"
                "cdadar"
                "cdaddr"
                "cdadr"
                "cdar"
                "cddaar"
                "cddadr"
                "cddar"
                "cdddar"
                "cddddr"
                "cdddr"
                "cddr"
                "cdr"
                "ceiling"
                "cell-error-name"
                "char"
                "char-code"
                "char-downcase"
                "char-equal"
                "char-greaterp"
                "char-int"
                "char-lessp"
                "char-name"
                "char-not-equal"
                "char-not-greaterp"
                "char-not-lessp"
                "char-upcase"
                "char/="
                "char<"
                "char<="
                "char="
                "char>"
                "char>="
                "characterp"
                "cis"
                "class-of"
                "clear-input"
                "clear-output"
                "close"
                "clrhash"
                "code-char"
                "coerce"
                "compile"
                "compile-file"
                "compile-file-pathname"
                "compiled-function-p"
                "compiler-macro"
                "compiler-macro-function"
                "complement"
                "complexp"
                "compute-restarts"
                "concatenate"
                "concatenated-stream-streams"
                "conjugate"
                "consp"
                "constantly"
                "constantp"
                "copy-alist"
                "copy-list"
                "copy-pprint-dispatch"
                "copy-readtable"
                "copy-seq"
                "copy-structure"
                "copy-symbol"
                "copy-tree"
                "cos"
                "cosh"
                "count"
                "count-if"
                "count-if-not"
                "decode-float"
                "decode-universal-time"
                "delete"
                "delete-duplicates"
                "delete-file"
                "delete-if"
                "delete-if-not"
                "delete-package"
                "denominator"
                "deposit-field"
                "describe"
                "describe-object"
                "digit-char"
                "digit-char-p"
                "directory"
                "directory-namestring"
                "disassemble"
                "documentation"
                "dpb"
                "dribble"
                "echo-stream-input-stream"
                "echo-stream-output-stream"
                "ed"
                "eighth"
                "elt"
                "encode-universal-time"
                "endp"
                "enough-namestring"
                "ensure-directories-exist"
                "ensure-generic-function"
                "eq"
                "equal"
                "equalp"
                "eval"
                "evenp"
                "every"
                "exp"
                "export"
                "expt"
                "fboundp"
                "fceiling"
                "fdefinition"
                "ffloor"
                "fifth"
                "file-author"
                "file-error-pathname"
                "file-length"
                "file-namestring"
                "file-position"
                "file-string-length"
                "file-write-date"
                "fill"
                "fill-pointer"
                "find"
                "find-all-symbols"
                "find-class"
                "find-if"
                "find-if-not"
                "find-package"
                "find-restart"
                "find-symbol"
                "finish-output"
                "first"
                "float-digits"
                "float-precision"
                "float-radix"
                "float-sign"
                "floatp"
                "floor"
                "fmakunbound"
                "force-output"
                "format"
                "fourth"
                "fresh-line"
                "fround"
                "ftruncate"
                "funcall"
                "function-lambda-expression"
                "functionp"
                "gcd"
                "gensym"
                "gentemp"
                "get"
                "get-decoded-time"
                "get-dispatch-macro-character"
                "get-internal-real-time"
                "get-internal-run-time"
                "get-macro-character"
                "get-output-stream-string"
                "get-properties"
                "get-setf-expansion"
                "get-universal-time"
                "getf"
                "gethash"
                "graphic-char-p"
                "hash-table-count"
                "hash-table-p"
                "hash-table-rehash-size"
                "hash-table-rehash-threshold"
                "hash-table-size"
                "hash-table-test"
                "host-namestring"
                "identity"
                "imagpart"
                "import"
                "input-stream-p"
                "inspect"
                "integer-decode-float"
                "integer-length"
                "integerp"
                "interactive-stream-p"
                "intern"
                "intersection"
                "invalid-method-error"
                "invoke-debugger"
                "invoke-restart"
                "invoke-restart-interactively"
                "isqrt"
                "keywordp"
                "last"
                "lcm"
                "ldb"
                "ldb-test"
                "ldiff"
                "length"
                "lisp-implementation-type"
                "lisp-implementation-version"
                "list-all-packages"
                "list-length"
                "list*"
                "listen"
                "listp"
                "load"
                "load-logical-pathname-translations"
                "log"
                "logand"
                "logandc1"
                "logandc2"
                "logbitp"
                "logcount"
                "logeqv"
                "logical-pathname-translations"
                "logior"
                "lognand"
                "lognor"
                "lognot"
                "logorc1"
                "logorc2"
                "logtest"
                "logxor"
                "long-site-name"
                "lower-case-p"
                "machine-instance"
                "machine-type"
                "machine-version"
                "macro-function"
                "macroexpand"
                "macroexpand-1"
                "make-array"
                "make-broadcast-stream"
                "make-concatenated-stream"
                "make-condition"
                "make-dispatch-macro-character"
                "make-echo-stream"
                "make-hash-table"
                "make-list"
                "make-load-form-saving-slots"
                "make-package"
                "make-pathname"
                "make-random-state"
                "make-sequence"
                "make-string"
                "make-string-input-stream"
                "make-string-output-stream"
                "make-symbol"
                "make-synonym-stream"
                "make-two-way-stream"
                "makunbound"
                "map"
                "map-into"
                "mapc"
                "mapcan"
                "mapcar"
                "mapcon"
                "maphash"
                "mapl"
                "maplist"
                "mask-field"
                "max"
                "member-if"
                "member-if-not"
                "merge"
                "merge-pathnames"
                "method-combination-error"
                "min"
                "minusp"
                "mismatch"
                "name-char"
                "namestring"
                "nbutlast"
                "nconc"
                "next-method-p"
                "nintersection"
                "ninth"
                "notany"
                "notevery"
                "nreconc"
                "nreverse"
                "nset-difference"
                "nset-exclusive-or"
                "nstring-capitalize"
                "nstring-downcase"
                "nstring-upcase"
                "nsublis"
                "nsubst"
                "nsubst-if"
                "nsubst-if-not"
                "nsubstitute"
                "nsubstitute-if"
                "nsubstitute-if-not"
                "nth"
                "nthcdr"
                "numberp"
                "numerator"
                "nunion"
                "oddp"
                "open"
                "open-stream-p"
                "output-stream-p"
                "package-error-package"
                "package-name"
                "package-nicknames"
                "package-shadowing-symbols"
                "package-use-list"
                "package-used-by-list"
                "packagep"
                "pairlis"
                "parse-integer"
                "parse-namestring"
                "pathname-device"
                "pathname-directory"
                "pathname-host"
                "pathname-match-p"
                "pathname-name"
                "pathname-type"
                "pathname-version"
                "pathnamep"
                "peek-char"
                "phase"
                "plusp"
                "position"
                "position-if"
                "position-if-not"
                "pprint"
                "pprint-dispatch"
                "pprint-fill"
                "pprint-indent"
                "pprint-linear"
                "pprint-newline"
                "pprint-tab"
                "pprint-tabular"
                "prin1"
                "prin1-to-string"
                "princ"
                "princ-to-string"
                "print"
                "print-not-readable-object"
                "print-object"
                "probe-file"
                "provide"
                "random"
                "random-state-p"
                "rassoc"
                "rassoc-if"
                "rassoc-if-not"
                "rationalize"
                "rationalp"
                "read"
                "read-byte"
                "read-char"
                "read-char-no-hang"
                "read-delimited-list"
                "read-from-string"
                "read-line"
                "read-preserving-whitespace"
                "read-sequence"
                "readtable-case"
                "readtablep"
                "realp"
                "realpart"
                "reduce"
                "rem"
                "remhash"
                "remove"
                "remove-duplicates"
                "remove-if"
                "remove-if-not"
                "remprop"
                "rename-file"
                "rename-package"
                "replace"
                "require"
                "rest"
                "restart-name"
                "revappend"
                "reverse"
                "room"
                "round"
                "row-major-aref"
                "rplaca"
                "rplacd"
                "sbit"
                "scale-float"
                "schar"
                "search"
                "second"
                "set"
                "set-difference"
                "set-dispatch-macro-character"
                "set-exclusive-or"
                "set-macro-character"
                "set-pprint-dispatch"
                "set-syntax-from-char"
                "seventh"
                "shadow"
                "shadowing-import"
                "short-site-name"
                "signum"
                "simple-bit-vector-p"
                "simple-condition-format-arguments"
                "simple-condition-format-control"
                "simple-string-p"
                "simple-vector-p"
                "sin"
                "sinh"
                "sixth"
                "sleep"
                "slot-boundp"
                "slot-exists-p"
                "slot-makunbound"
                "slot-value"
                "software-type"
                "software-version"
                "some"
                "sort"
                "special-operator-p"
                "sqrt"
                "stable-sort"
                "standard-char-p"
                "stream-element-type"
                "stream-error-stream"
                "stream-external-format"
                "streamp"
                "string-capitalize"
                "string-downcase"
                "string-equal"
                "string-greaterp"
                "string-left-trim"
                "string-lessp"
                "string-not-equal"
                "string-not-greaterp"
                "string-not-lessp"
                "string-right-trim"
                "string-trim"
                "string-upcase"
                "string/="
                "string<"
                "string<="
                "string="
                "string>"
                "string>="
                "stringp"
                "structure"
                "sublis"
                "subseq"
                "subsetp"
                "subst"
                "subst-if"
                "subst-if-not"
                "substitute"
                "substitute-if"
                "substitute-if-not"
                "subtypep"
                "svref"
                "sxhash"
                "symbol-function"
                "symbol-name"
                "symbol-package"
                "symbol-plist"
                "symbol-value"
                "symbolp"
                "synonym-stream-symbol"
                "tailp"
                "tan"
                "tanh"
                "tenth"
                "terpri"
                "third"
                "translate-logical-pathname"
                "translate-pathname"
                "tree-equal"
                "truename"
                "truncate"
                "two-way-stream-input-stream"
                "two-way-stream-output-stream"
                "type-error-datum"
                "type-error-expected-type"
                "type-of"
                "typep"
                "unbound-slot-instance"
                "unexport"
                "unintern"
                "union"
                "unread-char"
                "unuse-package"
                "upgraded-array-element-type"
                "upgraded-complex-part-type"
                "upper-case-p"
                ;; "use-package"
                "user-homedir-pathname"
                "values-list"
                "variable"
                "vector-pop"
                "vector-push"
                "vector-push-extend"
                "vectorp"
                "wild-pathname-p"
                "write"
                "write-byte"
                "write-char"
                "write-line"
                "write-sequence"
                "write-string"
                "write-to-string"
                "y-or-n-p"
                "yes-or-no-p"
                "zerop"))
           symbol-end)
      (1 'ansi-lisp-function-face)))))


(defconst +ansi-lisp-special-forms+
  '((ansi-lisp-highlight-format-directives
     (0 'ansi-lisp-format-directive-face prepend))
    (ansi-lisp-highlight-loop-keywords
     (0 'ansi-lisp-loop-keyword-face))
    ;; (ansi-lisp-highlight-declarations
    ;;  (1 'ansi-lisp-declaration-face))
    ;; (ansi-lisp-highlight-types
    ;;  (0 'ansi-lisp-type-face))
    ))

(defvar cl-font-lock-keywords
  ;; +ansi-lisp-basic-keywords+
  (append +ansi-lisp-special-forms+
          +ansi-lisp-basic-keywords+
          ;; +ansi-lisp-advanced-keywords+
          ))


(defun ansi-lisp-highlight-keywords (&optional mode)
  (font-lock-remove-keywords
   mode
   `(("\\<:\\sw+\\>"
      (0 font-lock-builtin-face))
     ("\\<:\\sw+\\>"
      0 font-lock-builtin-face)
     ("\\<\\&\\sw+\\>"
      (0 font-lock-type-face))
     ("\\<\\&\\sw+\\>"
      0 font-lock-type-face)))

  (font-lock-add-keywords
   mode
   (append cl-font-lock-keywords)))

(defun ansi-lisp-highlight-minimum (&optional mode)
  (font-lock-remove-keywords
   mode
   `(("\\<:\\sw+\\>"
      (0 font-lock-builtin-face))
     ("\\<:\\sw+\\>"
      0 font-lock-builtin-face)
     ("\\<\\&\\sw+\\>"
      (0 font-lock-type-face))
     ("\\<\\&\\sw+\\>"
      0 font-lock-type-face)))

  (font-lock-add-keywords
   mode
   (append +ansi-lisp-basic-keywords+
           +ansi-lisp-special-forms+)))

(defun ansi-lisp-highlight-emacs-keywords (&optional mode)
  (font-lock-remove-keywords
   mode
   `(("\\<:\\sw+\\>"
      (0 font-lock-builtin-face))
     ("\\<:\\sw+\\>"
      0 font-lock-builtin-face)
     ("\\<\\&\\sw+\\>"
      (0 font-lock-type-face))
     ("\\<\\&\\sw+\\>"
      0 font-lock-type-face)))

  (font-lock-add-keywords
   mode
   (append +ansi-lisp-basic-keywords+
           (list
            ;; loop highlighting is nice to have in Emacs
            '(ansi-lisp-highlight-loop-keywords
              0 'ansi-lisp-loop-keyword-face)))))



;; some safety compile-time checks
(eval-when-compile
 (letrec ((check-font-lock-keywords
            (lambda (keywords)
              (unless (every #'listp keywords)
                (error "Non-list entry: %s"
                       (find-if-not #'listp keywords)))
              (mapc (lambda (entry)
                      (unless (or (stringp (car entry))
                                  (symbolp (car entry)))
                        (error "Neither string nor symbol first value %s of entry: %s"
                               (car entry)
                               entry))
                      (cond
                        ((funcall highlight-entryp (cdr entry))
                         (funcall check-highlight-entry (cdr entry)))
                        ((every highlight-entryp (cdr entry))
                         (mapc check-highlight-entry (cdr entry)))
                        (t
                         (error "Non-highlight directive(s) found: %s" (cdr entry)))))
                    keywords)
              (values)))

          (check-highlight-entry
            (lambda (entry)
              (let ((face-entry (cadr entry)))
                (when (and (listp face-entry)
                           (symbolp (cadr face-entry))
                           (= 2 (length face-entry))
                           (not (eq 'quote (car face-entry))))
                  (error "Unquoted face: %s" face-entry))
                (when (symbolp face-entry)
                  (error "Unquoted face: %s" face-entry)))))

          (highlight-entryp
            (lambda (form)
              (and (listp form)
                   (numberp (car form))
                   (<= 2 (length form))))))

   (funcall check-font-lock-keywords +ansi-lisp-basic-keywords+)
   (funcall check-font-lock-keywords +ansi-lisp-advanced-keywords+)
   (funcall check-font-lock-keywords +ansi-lisp-special-forms+)))



;; (,(rx symbol-start
;;       (or "//"
;;           "///"
;;           "**"
;;           "***"
;;           "*break-on-signals*"
;;           "*compile-file-pathname*"
;;           "*compile-file-truename*"
;;           "*compile-print*"
;;           "*compile-verbose*"
;;           "*debug-io*"
;;           "*debugger-hook*"
;;           "*default-pathname-defaults*"
;;           "*error-output*"
;;           "*features*"
;;           "*gensym-counter*"
;;           "*load-pathname*"
;;           "*load-print*"
;;           "*load-truename*"
;;           "*load-verbose*"
;;           "*macroexpand-hook*"
;;           "*modules*"
;;           "*package*"
;;           "*print-array*"
;;           "*print-base*"
;;           "*print-case*"
;;           "*print-circle*"
;;           "*print-escape*"
;;           "*print-gensym*"
;;           "*print-length*"
;;           "*print-level*"
;;           "*print-lines*"
;;           "*print-miser-width*"
;;           "*print-pprint-dispatch*"
;;           "*print-pretty*"
;;           "*print-radix*"
;;           "*print-readably*"
;;           "*print-right-margin*"
;;           "*query-io*"
;;           "*random-state*"
;;           "*read-base*"
;;           "*read-default-float-format*"
;;           "*read-eval*"
;;           "*read-suppress*"
;;           "*readtable*"
;;           "*standard-input*"
;;           "*standard-output*"
;;           "*terminal-io*"
;;           "*trace-output*"
;;           "++"
;;           "+++")
;;       symbol-end)
;;  0
;;  'ansi-lisp-global-variable-face t
;;  t)



(provide 'ansi-lisp-highlight)

;; Local Variables:
;; End:

;; ansi-lisp-highlight.el ends here
