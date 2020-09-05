;; python-highlight.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday,  4 February 2012
;; Description:

(require 'common)
(require 'solarized)

(defface python-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face to highlight python keywords.")

(defface python-builtin-face
  '((t (:inherit font-lock-builtin-face)))
  "Face to highlight python builtin functions.")

(defface python-warnings-and-errors-face
  `((t (:foreground ,+solarized-orange+)))
  "Face to highlight warious python runtime warning and error objects.")

(defface python-decorator-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face to highlight python decorators.")

(defface python-function-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight python function names.")

(defface python-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Face to highlight python constants.")

(defface python-format-directive-face
  '((t (:inherit font-lock-negation-char-face)))
  "Face to highlight python format directives")


(defconst +python-plain-format-directive-regexp+
  (rx-let ((conversion-flag (regexp "[#0- +]"))
           (numbers-or-star (or (+ numeric)
                                "*"))
           (minimum-field-width numbers-or-star)
           (precision (seq "." numbers-or-star))
           (length-modifier (regexp "[hlL]"))

           (conversion-type (regexp "[diouxXeEfFgGcrs%]")))
    (rx "%"
        (? "(" (regexp "[^()]") ")")
        ;; (* (regexp "[#-+ 0-9*.hlL]"))
        (* conversion-flag)
        (? minimum-field-width)
        (? precision)
        (? length-modifier)
        conversion-type)))

(make-highlight-procedure
 python-highlight-format-plain-directives
 +python-plain-format-directive-regexp+
 #'python-point-inside-string-and-not-comment?)


(defconst +python-fancy-format-directive-regexp+
  (rx-let ((binary      (seq (regexp "[bB]")
                             (+ (regexp "[01]"))))
           (octal       (seq (? (regexp "[oO]"))
                             (+ (regexp "[0-7]"))))
           (hexadecimal (seq (regexp "[xX]")
                             (+ hex-digit)))
           (integer     (or (+ digit) (seq "0"
                                           (or binary
                                               octal
                                               hexadecimal))))
           (identifier  (regexp "[a-zA-Z_][a-zA-Z0-9_]*"))

           (index-string (+ (regexp "[^]]")))

           (element-index (or integer index-string))
           (attribute-name identifier)
           (arg-name (or identifier integer))
           (field-name (seq arg-name
                            (* (or (seq "."
                                        attribute-name)
                                   (seq "["
                                        element-index
                                        "]")))))

           (conversion (regexp "[rs]"))

           (fill (regexp "[^}]"))
           (align (regexp "[<>=^]"))
           (sign  (or "+" "-"))
           (format-width integer)
           (format-precision integer)
           (format-type (regexp "[bcdeEfFgGnosxX%]"))

           (format-spec (seq (? (? fill)
                                align)
                             (? sign)
                             (? "#")
                             (? "0")
                             (? format-width)
                             (? ",")
                             (? "." format-precision)
                             (? format-type)))

           (fancy-formatting (seq "{"
                                  ;; (*? (regexp "[^}]"))
                                  (? field-name)
                                  (? "!" conversion)
                                  (? ":" format-spec)
                                  "}")))
    (rx fancy-formatting)))

(make-highlight-procedure
 python-highlight-format-fancy-directives
 +python-fancy-format-directive-regexp+
 #'python-point-inside-string-and-not-comment?)



(defconst +python-standard-keywords+
  `((,(rx symbol-start
          (or
           "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
           "assert" "else" "if" "pass" "yield" "break" "except" "import" "class"
           "in" "raise" "continue" "finally" "is" "return" "def" "for" "lambda"
           "try"
           ;; Python 2:
           "print" "exec"
           ;; Python 3:
           ;; False, None, and True are listed as keywords on the Python 3
           ;; documentation, but since they also qualify as constants they are
           ;; fontified like that in order to keep font-lock consistent between
           ;; Python versions.
           "nonlocal"
           ;; Extra:
           "self")
          symbol-end)
     (0 'python-keyword-face))
    (,(rx symbol-start
          (or "__debug__"
              "__import__"
              "__name__"
              "abs"
              "all"
              "any"
              "apply"
              "basestring"
              "bin"
              "bool"
              "buffer"
              "bytearray"
              "callable"
              "chr"
              "classmethod"
              "cmp"
              "coerce"
              "compile"
              "complex"
              "copyright"
              "credits"
              "delattr"
              "dict"
              "dir"
              "divmod"
              "enumerate"
              "eval"
              "exec"
              "execfile"
              "exit"
              "file"
              "filter"
              "float"
              "format"
              "getattr"
              "globals"
              "hasattr"
              "hash"
              "help"
              "hex"
              "id"
              "input"
              "int"
              "intern"
              "isinstance"
              "issubclass"
              "iter"
              "len"
              "license"
              "list"
              "locals"
              "long"
              "map"
              "max"
              "memoryview"
              "min"
              "next"
              "object"
              "oct"
              "open"
              "ord"
              "pow"
              "print"
              ;; "property"
              "quit"
              "range"
              "raw_input"
              "reduce"
              "reload"
              "repr"
              "round"
              "set"
              "setattr"
              "slice"
              "sorted"
              "staticmethod"
              "str"
              "sum"
              "super"
              "tuple"
              "type"
              "unichr"
              "unicode"
              "vars"
              "xrange"
              "zip"
              "bin"
              "bytearray"
              "bytes"
              "format"
              "memoryview"
              "next"
              "print"


              ;; python 3
              "ascii"
              "bytearray"
              "bytes"
              "exec"

              ;; extra
              "__all__"
              "__doc__"
              "__name__"
              "__package__")
          symbol-end)
     (0 'python-builtin-face))
    (,(rx symbol-start
          (or "ArithmeticError"
              "AssertionError"
              "AttributeError"
              "BaseException"
              "BufferError"
              "BytesWarning"
              "DeprecationWarning"
              "EOFError"
              "EnvironmentError"
              "Exception"
              "FloatingPointError"
              "FutureWarning"
              "GeneratorExit"
              "IOError"
              "ImportError"
              "ImportWarning"
              "IndentationError"
              "IndexError"
              "KeyError"
              "KeyboardInterrupt"
              "LookupError"
              "MemoryError"
              "NameError"
              "NotImplemented"
              "NotImplementedError"
              "OSError"
              "OverflowError"
              "PendingDeprecationWarning"
              "ReferenceError"
              "RuntimeError"
              "RuntimeWarning"
              "StandardError"
              "StopIteration"
              "SyntaxError"
              "SyntaxWarning"
              "SystemError"
              "SystemExit"
              "TabError"
              "TypeError"
              "UnboundLocalError"
              "UnicodeDecodeError"
              "UnicodeEncodeError"
              "UnicodeError"
              "UnicodeTranslateError"
              "UnicodeWarning"
              "UserWarning"
              "ValueError"
              "Warning"
              "ZeroDivisionError")
          symbol-end)
     (0 'python-warnings-and-errors-face))

    (,(rx symbol-start
          (group (or "class" "def"))
          (+ (regexp "[ \t]"))
          (group
           (regexp "[a-zA-Z_][a-zA-Z0-9_]*")))
     (1 'python-keyword-face)
     (2 'python-function-name-face))

    (,(rx symbol-start
          (group (or "except" "raise"))
          (group (??
                  (+ (regexp "[ \t]"))
                  (regexp "[a-zA-Z_][a-zA-Z0-9_.]*"))))

     (1 'python-keyword-face)
     (2 'python-warnings-and-errors-face))

    ("^[ \t]*\\(@[a-zA-Z_][a-zA-Z_0-9.]+\\)\\(?:(.+)\\)?"
     (1 'python-decorator-face)))
  "Standard Python keywords and tokens highlighted with regexps")

(defconst +python-highlight-constants+
  `((,(rx-let ((sign        (or "-" "+"))
               (integer     (+ digit))
               (binary      (seq (regexp "[bB]")
                                 (+ (regexp "[01]"))))
               (octal       (seq (? (regexp "[oO]"))
                                 (+ (regexp "[0-7]"))))
               (hexadecimal (seq (or "x" "X")
                                 (+ hex-digit)))

               (float-point (seq (? sign)
                                 symbol-start
                                 (or (seq (+ digit)
                                          (? "." (* digit)))
                                     (seq (* digit)
                                          "." (+ digit)))))
               (float       (seq float-point
                                 (? (or "e" "E")
                                    float-point)))

               (complex     (seq (or integer float) (or "j" "J")))

               (number (or (seq (? sign)
                                symbol-start
                                (or integer
                                    (seq "0"
                                         (or binary
                                             octal
                                             hexadecimal)))
                                (? (or "l" "L")))
                           float
                           complex)))
        (rx number
            symbol-end))
     (0 'python-constant-face))

    (,(rx symbol-start
          (or
           "Ellipsis" "False" "None" "True"
           ;; copyright, license, credits, quit and exit are added by the site
           ;; module and they are not intended to be used in programs
           "copyright" "credits" "exit" "license" "quit")
          symbol-end)
     (0 'python-constant-face)))
  "Numbers, booleans, etc highligthed with regexps.")

(defconst +python-highlight-procedures+
  '((python-highlight-format-plain-directives
     (0 'python-format-directive-face prepend))
    (python-highlight-format-fancy-directives
     (0 'python-format-directive-face prepend)))
  "Routines that use user-defined procedures rather than regexps to
find parts to highlight.")


(defun python-highlight-disable-pretty-symbols? (&optional position)
  "Predicate that determines, if POSITION is eligible to be part (beginning) of
pretty symbol. Intended for use in `font-lock-keywords' and
`+python-pretty-symbols+'."
  (save-excursion
    (when position
      (goto-char position))
    (or (point-inside-string-or-comment?)
        (memq (get-text-property (point) 'face)
              '(font-lock-comment-face
                font-lock-string-face))
        (get-text-property (point) 'disable-pretty-symbols))))

(defconst +python-pretty-symbols+
  `((,(rx (or (seq
               (group symbol-start
                      "is"
                      symbol-end)
               (+ whitespace)
               (? (group symbol-start
                         "not"
                         symbol-end)))
              (seq
               (group symbol-start
                      "not"
                      symbol-end
                      (+ whitespace))
               (? (group symbol-start
                         "in"
                         symbol-end)))
              (seq (group symbol-start
                          "in"
                          symbol-end))))

     (0 (unless (python-highlight-disable-pretty-symbols?
                 (match-beginning 0))
          (let ((match-1 (re-group-matched? 1))
                (match-2 (re-group-matched? 2))
                (match-3 (re-group-matched? 3))
                (match-4 (re-group-matched? 4))
                (match-5 (re-group-matched? 5)))
            (if match-1
              (if match-2
                ;; 1 and 2
                (compose-region (match-beginning 0)
                                (match-end 0)
                                ?≢)
                ;; 1 and ~2
                (compose-region (match-beginning 1)
                                (match-end 1)
                                ?≡))
              (if match-3
                (if match-4
                  ;; 3 and 4
                  (compose-region (match-beginning 0)
                                  (match-end 0)
                                  ?∉)
                  ;; 3 and ~4
                  (compose-region (match-beginning 0)
                                  (match-end 0)
                                  ?¬))
                ;; 5
                (compose-region (match-beginning 0)
                                (match-end 0)
                                ?∈)))
            nil))))

    ;; Make pretty lambdas and other pretty symbols.
    ,@(-map (lambda (entry)
              (let ((re (car entry))
                    (char (cadr entry)))
                `(,re
                  (0 (unless (python-highlight-disable-pretty-symbols?
                              (match-beginning 0))
                       (compose-region (match-beginning 0)
                                       (match-end 0)
                                       ,char)
                       nil)))))
            '(("\\_<lambda\\_>"         ?λ)
              ("\\_<for\\_>"            ?∀)
              ("\\_<int\\_>"            ?ℤ)
              ("\\_<float\\_>"          ?ℝ)
              ("\\_<complex\\_>"        ?ℂ)
              ("[ \t]*\\*\\*[ \t]*2\\>" ?²)
              ("\\_<and\\_>"            ?∧)
              ("\\_<or\\_>"             ?∨)
              ("<="                     ?≤)
              (">="                     ?≥)
              ("\\_<sum\\_>"            ?∑)))

    ;; ensure that pretty symbols go away as soon as we type something after any of them
    (,(concat "\\(?:\\_<\\("
              (join-lines '("lambda"
                            "for"
                            ;; hack
                            "int"
                            "float"
                            "complex"
                            "and"
                            "or"
                            "not"
                            "[ \t]*\\*\\*[ \t]*2"
                            "is"
                            "sum")
                          "\\|")
              "\\)[a-zA-Z_0-9]\\)"
              "\\|"
              "\\(?:in[^t \t\n\r]\\)")
     (0 (unless (python-highlight-disable-pretty-symbols?
                 (match-beginning 0))
          (decompose-region (match-beginning 0)
                            (match-end 0))
          nil))))
  "Pretty symbols for various keywords, e.g. λ for lambda.")

;; small test suite
;; x = int(n)
;; int(b)
;; nt
;; z is bar
;; z in bar
;; not z
;; x not in foo
;; y is not quux
;;
;; for a in b:
;;     pass
;; x ** 2 + y**2 + z **2 - w** 2
;;

(defparameter *python-font-lock-keywords*
  (append +python-standard-keywords+
          +python-highlight-constants+
          +python-highlight-procedures+
          +python-pretty-symbols+)
  "Additional expressions to highlight in Python mode.")

(defparameter *python-repl-font-lock-keywords*
  (append +python-standard-keywords+
          +python-highlight-constants+
          ;; +python-pretty-symbols+
          ))

(provide 'python-highlight)

;; Local Variables:
;; End:

;; python-highlight.el ends her
