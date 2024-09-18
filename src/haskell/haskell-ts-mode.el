;; haskell-ts-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 February 2024
;; Description:

(eval-when-compile
  (require 'dash))

(defvar haskell-indent-offset)

(require 'common)
(require 'common-whitespace)
(require 'haskell-lexeme)
(require 'haskell-syntax-table)
(require 'treesit)

(require 'haskell-smart-operators-mode)

(declare-function treesit-parser-create "treesit.c")

(defconst haskell-ts-mode-syntax-table haskell-mode-syntax-table)

(defface haskell-ts-haddock-face
  '((t :inherit font-lock-comment-face :bold t))
  "How to fontify Haddock omments, e.g. ‘-- |’"
  :group 'haskell-appearance)

(defun haskell-ts-mode--name-not-within-infix? (node)
  (let ((p1 (treesit-node-parent node)))
    (if p1
        (and (not (equal "infix_id" (treesit-node-type p1)))
             (if-let ((p2 (treesit-node-parent p1)))
                 (not (equal "infix_id" (treesit-node-type p2)))
               t))
      t)))

(defconst haskell-ts-font-lock-rules
  (--mapcat
   (cons :language
         (cons 'haskell
               (cons :feature
                     (cons (car it)
                           (cons :override
                                 (cons nil
                                       (list (cdr it))))))))
   '((everyone

      ;; comment
      ((comment) @font-lock-comment-face)

      ((haddock) @haskell-ts-haddock-face)

      ;; constant
      ([(integer) (float)] @font-lock-constant-face)

      (quasiquote "|" @default)

      ;; string
      ([(char) (string) (quasiquote_body)] @font-lock-string-face)

      ;; preprocessor
      ([(pragma) (cpp)] @haskell-pragma-face)

      ;; keyword
      (lambda_cases
       "\\"
       ("cases" @haskell-keyword-face))

      (default_signature
       ("default" @haskell-keyword-face))

      (pattern_synonym
       ("pattern" @haskell-keyword-face))

      ((variable) @haskell-keyword-face
       (:equal "_" @haskell-keyword-face))

      ([
        "forall"
        "where"
        "let"
        "in"
        "class"
        "instance"
        "data"
        "newtype"
        "family"
        "type"
        "as"
        "hiding"
        "deriving"
        "via"
        "stock"
        "anyclass"
        "do"
        "mdo"
        "rec"
        "infix"
        "infixl"
        "infixr"

        "if"
        "then"
        "else"
        "case"
        "of"

        "import"
        "qualified"
        "module"
        (wildcard "_")
        ]
       @haskell-keyword-face)

      ;; operator
      ([
        (operator)
        (qualified (operator))
        (all_names)
        "="
        "|"
        "::"
        "=>"
        "->"
        "<-"
        "\\"
        "@"
        ]
       @haskell-operator-face)

      (forall "." @haskell-keyword-face)

      ;; (unboxed_tuple "(#" @haskell-keyword-face)
      ;; (unboxed_tuple "#)" @haskell-keyword-face)

      ((infix_id
        "`"
        [(name)
         (qualified (name))
         (variable)
         (qualified (variable))
         (constructor)
         (qualified (constructor))]
        "`")
       @haskell-operator-face)

      ;; module-name
      ;; Competes with (module)
      ;; ((qualified (variable)) @default)

      (import (module) @haskell-type-face)
      (header (module) @haskell-type-face)
      (module_export (module) @haskell-type-face)

      ;; type
      ;; ((signature name: (variable) @font-lock-type-face))

      ;; Handles all types
      (((name) @haskell-type-face)
       (:pred haskell-ts-mode--name-not-within-infix? @haskell-type-face))
      (((qualified (module) (name)) @haskell-type-face)
       (:pred haskell-ts-mode--name-not-within-infix? @haskell-type-face))

      ;; constructor
      ([(constructor) (unit) (list "[" !element "]") (constructor_operator)] @haskell-constructor-face)

      ((qualified (module) @haskell-type-face
                  [(constructor) (constructor_operator)] @haskell-constructor-face))

      ;; strictness
      ([(strict_field "!") (strict "!")] @haskell-ts-mode--fontify-bang)

      ;; laziness
      ([(lazy_field) (irrefutable "~")] @haskell-ts-mode--fontify-tilde)))))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind (node parent bol)
  (save-excursion
    (let ((prev2 nil)
          (prev1 node)
          (curr parent))
      (catch 'term
        (while curr
          (let ((curr-type (treesit-node-type curr)))
            (when (string= "function" curr-type)
              (throw 'term (treesit-node-start prev1)))
            (when (string= "infix" curr-type)
              (let ((left-child (treesit-node-child-by-field-name
                                 curr
                                 "left_operand"))
                    (right-child (treesit-node-child-by-field-name
                                  curr
                                  "right_operand")))
                (cond
                  ((equal prev1 left-child)
                   ;; Continue: we’re left operand of an infix operator,
                   ;; operator comes after us so if we’re not at bol then
                   ;; whe don’t care where operator is.
                   ;; (throw 'term (treesit-node-start prev1))
                   )
                  ((equal prev1 right-child)
                   ;; Operator may be on a line of its own, take it into account.
                   (let* ((op (treesit-node-child-by-field-name curr "operator"))
                          (start (treesit-node-start op)))
                     (goto-char start)
                     (skip-chars-backward " \t")
                     (when (eq (point) (line-beginning-position))
                       (throw 'term (treesit-node-start prev1))))
                   ;; Otherwise whole operator application may occupy
                   ;; its own line, i.e. its left child may be at the
                   ;; line start so continue processing current node
                   ;; as is.
                   )
                  (t
                   ;; Should not happen but just don’t do anything then.
                   (error "Unexpected infix field, node = %s, child = %s"
                          curr
                          prev1)))))
            (let ((start (treesit-node-start curr)))
              (if (string= "parens" curr-type)
                  (throw 'term start)
                (progn
                  (goto-char start)
                  (skip-chars-backward " \t")
                  (when (eq (point) (line-beginning-position))
                    (if (or (string= "let" curr-type)
                            (string= "let_in" curr-type))
                        (when prev2
                          (throw 'term (treesit-node-start prev2)))
                      (throw 'term start)))))))
          (setq prev2 prev1
                prev1 curr
                curr (treesit-node-parent curr)))))))

(defun haskell-ts-indent--standalone-parent-fast (node parent bol)
  (save-excursion
    (let ((curr parent))
      (catch 'term
        (while curr
          (let ((start (treesit-node-start curr)))
            (goto-char start)
            (skip-chars-backward " \t")
            (when (eq (point) (line-beginning-position))
              (throw 'term start)))
          (setq curr (treesit-node-parent curr)))))))

(defun haskell-ts-indent--prev-sib (node parent bol)
  (let ((n (treesit-node-prev-sibling node)))
    (while (string= "comment" (treesit-node-type n))
      (setq n (treesit-node-prev-sibling n)))
    (treesit-node-start n)))

(defconst haskell-ts-indent-rules
  '(((node-is "comment") prev-sibling 0)
    ((node-is "cpp") column-0 0)
    ((parent-is "comment") column-0 0)
    ((parent-is "imports") column-0 0)
    ;; Infix
    ((node-is "infix") haskell-ts-indent--standalone-non-infix-parent-or-let-bind haskell-indent-offset)
    ((parent-is "infix") haskell-ts-indent--standalone-parent-fast haskell-indent-offset)
    ;; Lambda
    ((parent-is "lambda") haskell-ts-indent--standalone-parent-fast haskell-indent-offset)

    ((parent-is "class_declarations") prev-sibling 0)

    ((node-is "^in$") parent 0)

    ;; list
    ((node-is "]") parent 0)
    ((parent-is "list") parent 1)

    ;; If then else
    ((node-is "then") parent haskell-indent-offset)
    ((node-is "^else$") parent haskell-indent-offset)

    ((parent-is "apply")
     haskell-ts-indent--standalone-non-infix-parent-or-let-bind
     haskell-indent-offset)

    ((node-is "quasiquote") grand-parent haskell-indent-offset)
    ((parent-is "quasiquote_body") (lambda (_ _ c) c) 0)

    ;; ((lambda (node parent bol)
    ;;    (let ((n (treesit-node-prev-sibling node)))
    ;;      (while (string= "comment" (treesit-node-type n))
    ;;        (setq n (treesit-node-prev-sibling n)))
    ;;      (string= "do" (treesit-node-type n))))
    ;;  haskell-ts-indent--standalone-parent-fast
    ;;  haskell-indent-offset)
    ;; ((parent-is "do") haskell-ts-indent--prev-sib 0)

    ((parent-is "do") haskell-ts-indent--standalone-non-infix-parent-or-let-bind haskell-indent-offset)


    ((node-is "alternatives")
     (lambda (_ b _)
       (treesit-node-start (treesit-node-child b 0)))
     haskell-indent-offset)
    ((parent-is "alternatives") haskell-ts-indent--prev-sib 0)

    (no-node prev-adaptive-prefix 0)

    ((parent-is "data_constructors") parent 0)

    ;; where
    ((lambda (node _ _)
       (let ((n (treesit-node-prev-sibling node)))
         (while (string= "comment" (treesit-node-type n))
           (setq n (treesit-node-prev-sibling n)))
         (string= "where" (treesit-node-type n))))
     (lambda (_ b _)
       (+ haskell-indent-offset (treesit-node-start (treesit-node-prev-sibling b))))
     haskell-indent-offset)
    ((parent-is "local_binds\\|instance_declarations") haskell-ts-indent--prev-sib 0)
    ((node-is "^where$") parent haskell-indent-offset)

    ;; Match
    ;; ((match "match" nil 2 2 nil) haskell-ts-indent--prev-sib 0)
    ((lambda (node _ _)
       (and (string= (treesit-node-type node) "match")
            (let ((pos 3)
                  (n node)
                  (ch (lambda () )))
              (while (and (not (null n))
                          (not (eq pos 0)))
                (setq n (treesit-node-prev-sibling n))
                (unless (string= "comment" (treesit-node-type n))
                  (setq pos (- pos 1))))
              (and (null n) (eq pos 0)))))
     parent
     haskell-indent-offset)
    ;; ((match "match" nil nil 3 nil) haskell-ts-indent--prev-sib 0)
    ((lambda (node _ _)
       (and (string= (treesit-node-type node) "match")
            (let ((pos 4)
                  (n node)
                  (ch (lambda () )))
              (while (and (not (null n))
                          (not (eq pos 0)))
                (setq n (treesit-node-prev-sibling n))
                (unless (string= "comment" (treesit-node-type n))
                  (setq pos (- pos 1))))
              (eq pos 0))))
     haskell-ts-indent--prev-sib 0)
    ((parent-is "match") haskell-ts-indent--standalone-parent-fast haskell-indent-offset)

    ((parent-is "haskell") column-0 0)
    ((parent-is "declarations") column-0 0)

    ((parent-is "record") haskell-ts-indent--standalone-parent-fast 2)

    ((parent-is "exports")
     (lambda (_ b _) (treesit-node-start (treesit-node-prev-sibling b)))
     0)
    ((n-p-gp nil "signature" "foreign_import") grand-parent haskell-indent-offset)

    ;; No backup - we would like to default to something else.
    ;; ;; Backup
    ;; (catch-all parent haskell-indent-offset)
    ))

(defun haskell-ts-mode--fontify-bang (node override start end &rest _)
  (haskell-ts-mode--fontify-first-char ?! node))

(defun haskell-ts-mode--fontify-tilde (node override start end &rest _)
  (haskell-ts-mode--fontify-first-char ?~ node))

(defun haskell-ts-mode--fontify-first-char (char node)
  (let* ((p (treesit-node-start node))
         (c (char-after p)))
    (while (and c
                (whitespace-char? c))
      (setf p (+ p 1)
            c (char-after p)))
    (when (eq (char-after p) char)
      (put-text-property p (1+ p) 'face 'font-lock-negation-char-face))))

(defconst haskell-ts-syntax-propertize--query
  (treesit-query-compile 'haskell
                         '(
                           ;; Need strings because multiline strings may not
                           ;; be handled correctly be Emacs.
                           ((string) @str-like)
                           ((char) @str-like)
                           (quasiquote
                            ;; start bracket not needed as we’re leaving them as is
                            ;; and make pipes carry quoting syntax so that parens matching
                            ;; will still work
                            ;; "[" @qq-start-bracket
                            "|" @qq-start-pipe
                            "|]" @qq-end
                            )
                           ;; ((comment) @comment)
                           ;; ((haddock) @comment)
                           ((operator) @operator
                            (:match "^--" @operator)))))

(defun haskell-ts-syntax-propertize (begin end)
  "Basically finds all operators (e.g. -->) that start with comment delimiter, -- that should
not be treated as comment start.

Also fix syntax of character quote delimiters because quote is a valid part of symbols as well
but when paired then it’s like a string."
  (save-match-data
    (let ((end-eol
           (save-excursion
             (goto-char end)
             (line-end-position))))
      (save-excursion
        (goto-char begin)

        (let ((beg-bol (line-beginning-position)))
          (dolist (entry
                   (treesit-query-capture (treesit-buffer-root-node 'haskell)
                                          haskell-ts-syntax-propertize--query
                                          beg-bol
                                          end-eol
                                          nil ;; want capture names
                                          ))
            (let* ((node (cdr entry))
                   (start (treesit-node-start node))
                   (end (treesit-node-end node)))
              (cl-assert (treesit-node-p node))
              (pcase (car entry)
                (`str-like
                 (save-excursion
                   (save-match-data
                     (goto-char start)
                     (while (search-forward "\"" end t)
                       (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (eval-when-compile (string-to-syntax "."))))))
                 (put-text-property start
                                    (1+ start)
                                    'syntax-table
                                    (eval-when-compile (string-to-syntax "\"")))
                 ;; If it was unboxed character literal, e.g. 'a'# then adjust point
                 ;; to make quotes the string delimiters.
                 (let ((butlast (1- end)))
                   (when (eq (char-after butlast) ?#)
                     (setf butlast (1- butlast)))
                   (put-text-property butlast
                                      (1+ butlast)
                                      'syntax-table
                                      (eval-when-compile (string-to-syntax "\"")))))
                ;; (`qq-start-bracket
                ;;  (put-text-property start
                ;;                     end
                ;;                     'syntax-table
                ;;                     (eval-when-compile (string-to-syntax "\""))))
                (`qq-start-pipe
                 (put-text-property start
                                    end
                                    'syntax-table
                                    ;; TODO: double check whether this should be a string or a
                                    ;; comment. String may imply unnecessary quoting!
                                    (eval-when-compile (string-to-syntax "|"))))
                (`qq-end
                 (put-text-property (- end 2)
                                    (- end 1)
                                    'syntax-table
                                    ;; TODO: double check whether this should be a string or a
                                    ;; comment. String may imply unnecessary quoting!
                                    (eval-when-compile (string-to-syntax "|"))))
                ;; (`comment
                ;;  (when (eq (char-after start) ?\{)
                ;;    (put-text-property (+ start 1)
                ;;                       (+ start 2)
                ;;                       'syntax-table
                ;;                       (eval-when-compile (string-to-syntax "!")))
                ;;    (put-text-property (- end 2)
                ;;                       (- end 1)
                ;;                       'syntax-table
                ;;                       (eval-when-compile (string-to-syntax "!"))))
                ;;  ;; (pcase (char-after start)
                ;;  ;;   (?-
                ;;  ;;    (let ((i start))
                ;;  ;;      (while (and (< i end-eol)
                ;;  ;;                  (eq (char-after i) ?-))
                ;;  ;;        (cl-incf i))
                ;;  ;;      (put-text-property start
                ;;  ;;                         i
                ;;  ;;                         'syntax-table
                ;;  ;;                         (eval-when-compile (string-to-syntax "<")))))
                ;;  ;;   (?\{
                ;;  ;;    (put-text-property start
                ;;  ;;                       (+ start 2)
                ;;  ;;                       'syntax-table
                ;;  ;;                       (eval-when-compile (string-to-syntax "<")))
                ;;  ;;    (put-text-property (- end 2)
                ;;  ;;                       end
                ;;  ;;                       'syntax-table
                ;;  ;;                       (eval-when-compile (string-to-syntax ">"))))
                ;;  ;;   (other
                ;;  ;;    (error "Invalid comment-like start: %s" other)))
                ;;  )
                (`operator
                 (put-text-property start
                                    end
                                    'syntax-table
                                    (eval-when-compile (string-to-syntax "."))))
                (other
                 (error "Invalid capture: %s" other))))))))))

;;;###autoload
(define-derived-mode haskell-ts-mode prog-mode "Haskell[ts]"
  "Major mode for Haskell that uses tree-sitter."

  ;; Important to disable long lines optimizations because they make font locking
  ;; operate in narrowed buffer which could make treesitter miss things.
  (setq-local long-line-optimizations-region-size 0)
  ;; Fast font lock mode is too imprecise and can also make treesitter miss things.
  (setq-local treesit--font-lock-fast-mode nil)

  (setq-local syntax-propertize-function
              #'haskell-ts-syntax-propertize)

  (setq-local font-lock-defaults nil
              treesit-font-lock-feature-list
              '((everyone)))

  (let ((res (treesit-language-available-p 'haskell t)))
    (unless (car res)
      (error "Haskell treesitter not available: %s" (cdr res))))

  ;; Associate parser with current buffer.
  (treesit-parser-create 'haskell (current-buffer))

  ;; Font locking
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules haskell-ts-font-lock-rules))

  ;; Indentation
  (setq-local treesit-simple-indent-rules
              (list (cons 'haskell haskell-ts-indent-rules)))

  (setq-local comment-start "--"
              comment-padding 1
              comment-start-skip "[-{]-[ \t]*"
              comment-end ""
              comment-end-skip "[ \t]*\\(-}\\|\\s>\\)"
              fill-paragraph-function #'haskell-fill-paragraph)

  (treesit-major-mode-setup))

(provide 'haskell-ts-mode)

;; Local Variables:
;; End:

;; haskell-ts-mode.el ends here
