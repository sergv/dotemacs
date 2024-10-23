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

(declare-function treesit-parser-create "treesit.c")

(defconst haskell-ts-mode-syntax-table haskell-mode-syntax-table)

(defface haskell-ts-haddock-face
  '((t :inherit font-lock-comment-face :bold t))
  "How to fontify Haddock omments, e.g. ‘-- |’"
  :group 'haskell-appearance)

(defun haskell-ts-mode--name-not-within-infix? (node)
  (let ((p1 (treesit-node-parent node)))
    (if p1
        (and (not (string= "infix_id" (treesit-node-type p1)))
             (if-let ((p2 (treesit-node-parent p1)))
                 (not (string= "infix_id" (treesit-node-type p2)))
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

      (namespace
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

(defun haskell-ts-indent--standalone-non-infix-parent--generic (node parent bol support-functions? support-field-update?)
  (save-excursion
    (let ((prev2 nil)
          (prev1 node)
          (curr parent))
      (catch 'term
        (while curr
          (let ((curr-type (treesit-node-type curr)))
            (when (and support-functions?
                       (string= "function" curr-type))
              (throw 'term curr))
            (when (and support-field-update?
                       (string= "field_update" curr-type))
              (when-let ((field-name (treesit-node-child-by-field-name curr "field")))
                (throw 'term field-name)))
            (when (string= "infix" curr-type)
              (let ((left-child (treesit-node-child-by-field-name
                                 curr
                                 "left_operand"))
                    (right-child (treesit-node-child-by-field-name
                                  curr
                                  "right_operand"))
                    (op-child (treesit-node-child-by-field-name
                               curr
                               "operator")))
                (cond
                  ((equal prev1 left-child)
                   ;; Continue: we’re left operand of an infix operator,
                   ;; operator comes after us so if we’re not at bol then
                   ;; whe don’t care where operator is.
                   ;; (throw 'term prev1)
                   )
                  ((or (equal prev1 right-child)
                       (equal prev1 op-child))
                   ;; Operator may be on a line of its own, take it into account.
                   (let ((start (treesit-node-start op-child)))
                     (goto-char start)
                     (skip-chars-backward " \t")
                     (when (eq (point) (line-beginning-position))
                       (throw 'term prev1)))
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
              (if (or (string= "parens" curr-type)
                      (string= "list" curr-type))
                  (throw 'term curr)
                (progn
                  (goto-char start)
                  (skip-chars-backward " \t")
                  (when (eq (point) (line-beginning-position))
                    (if (or (string= "let" curr-type)
                            (string= "let_in" curr-type))
                        (when prev2
                          (throw 'term prev2))
                      (throw 'term curr)))))))
          (setq prev2 prev1
                prev1 curr
                curr (treesit-node-parent curr)))))))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol t t))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol t nil))

(defun haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update (node parent bol)
  (haskell-ts-indent--standalone-non-infix-parent--generic node parent bol nil t))

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

(defun haskell-ts--positions-on-the-same-line? (pos1 pos2)
  (let ((end (max pos1 pos2)))
    (save-excursion
      (goto-char (min pos1 pos2))
      (skip-chars-forward "^\r\n" end)
      (eq (point) end))))

(defconst haskell-ts-indent-rules
  (eval-when-compile
    (let ((rules
           `(((node-is "comment") prev-sibling 0)
             ((node-is "cpp") column-0 0)
             ((parent-is "comment") column-0 0)
             ((parent-is "imports") column-0 0)

             ((parent-is "record")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update
              ,(lambda (node parent bol)
                 (lambda (matched-anchor)
                   (if (string= "field_name" (treesit-node-type matched-anchor))
                       0
                     haskell-indent-offset))))

             ((parent-is "field_update")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update
              0)

             ;; Infix
             ((node-is "infix") haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update haskell-indent-offset)

             ;; Assumes that this will only hit when "operator" node is at beginning of line.
             ((n-p-gp "operator" "infix" nil)
              haskell-ts-indent--standalone-parent-fast
              0)

             ;; Lambda
             ((parent-is "lambda") haskell-ts-indent--standalone-parent-fast haskell-indent-offset)

             ((parent-is "class_declarations") prev-sibling 0)

             ((node-is "in") parent 0)
             ((match nil "let_in" "expression" nil nil)
              ,(lambda (node parent bol)
                 (let* ((in-node (treesit-node-child parent 2))
                        (in-node-start (treesit-node-start in-node))
                        (parent-start (treesit-node-start parent)))
                   (if (haskell-ts--positions-on-the-same-line? in-node-start parent-start)
                       parent
                     in-node)))
              ,(lambda (node parent bol)
                 (lambda (matched-anchor)
                   (if (string= "let_in" (treesit-node-type matched-anchor))
                       0
                     haskell-indent-offset))))

             ;; list
             ((node-is "]") parent 0)
             ((n-p-gp "," "list" nil) parent 0)
             ((parent-is "list") parent haskell-indent-offset)

             ;; If then else
             ((node-is "then") parent haskell-indent-offset)
             ((node-is "else") parent haskell-indent-offset)

             ((parent-is "apply")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-function-or-field-update
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

             ((parent-is "do") haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update haskell-indent-offset)


             ((node-is "alternatives")
              haskell-ts-indent--standalone-non-infix-parent-or-let-bind-or-field-update
              haskell-indent-offset)
             ((parent-is "alternatives") haskell-ts-indent--prev-sib 0)

             (no-node prev-adaptive-prefix 0)

             ((parent-is "data_constructors") grand-parent haskell-indent-offset)
             ((node-is "gadt_constructors") parent haskell-indent-offset)
             ((parent-is "gadt_constructors") grand-parent haskell-indent-offset)

             ;; where
             ((lambda (node _ _)
                (let ((n (treesit-node-prev-sibling node)))
                  (while (string= "comment" (treesit-node-type n))
                    (setq n (treesit-node-prev-sibling n)))
                  (string= "where" (treesit-node-type n))))
              (lambda (_ b _)
                (+ haskell-indent-offset (treesit-node-start (treesit-node-prev-sibling b))))
              haskell-indent-offset)
             ((parent-is "local_binds" "instance_declarations") haskell-ts-indent--prev-sib 0)
             ((node-is "where") parent haskell-indent-offset)

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

             ((parent-is "exports")
              (lambda (_ b _) (treesit-node-start (treesit-node-prev-sibling b)))
              0)
             ((n-p-gp nil "signature" "foreign_import") grand-parent haskell-indent-offset)

             ((n-p-gp "," "tuple" nil) parent 0)

             ((node-is "deriving") parent haskell-indent-offset)

             ;; No backup - we would like to default to something else.
             ;; ;; Backup
             ;; (catch-all parent haskell-indent-offset)
             )))
      (dolist (rule rules)
        (unless (and (listp rule)
                     (= 3 (length rule)))
          (error "Malformed rule in haskell-ts-indent-rules: %s"
                 rule)))
      rules)))

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

(defun haskell-ts-beginning-of-defun ()
  (interactive)
  (vim-save-position)
  (haskell-ts-beginning-of-defun-impl (point)))

(defun haskell-ts-beginning-of-defun-impl (pos)
  (aif (haskell-ts--bounds-of-toplevel-entity pos t nil t nil)
      (goto-char (car it))
    (error "No toplevel entity at point")))

(defun haskell-ts-end-of-defun ()
  (interactive)
  (vim-save-position)
  (haskell-ts-end-of-defun-impl (point)))

(defun haskell-ts-end-of-defun-impl (pos)
  (aif (haskell-ts--bounds-of-toplevel-entity pos t t nil t)
      (goto-char (cdr it))
    (error "No toplevel entity at point")))

(defun haskell-ts--is-comment-node-type? (typ)
  (cl-assert (stringp typ))
  (or (string= typ "comment")
      (string= typ "haddock")
      (string= typ "pragma")))

(defun haskell-ts--is-string-node-type? (typ)
  (cl-assert (stringp typ))
  (or (string= typ "char")
      (string= typ "string")
      (string= typ "quasiquote_body")))

(defun haskell-ts--is-comment-node? (node)
  (haskell-ts--is-comment-node-type? (treesit-node-type node)))

(defun haskell-ts--is-toplevel-function-related-named-node-type? (typ)
  (cl-assert (stringp typ))
  (or (string= typ "signature")
      (string= typ "function")
      (string= typ "bind")))

(defun haskell-ts--is-toplevel-function-related-node? (node)
  (haskell-ts--is-toplevel-function-related-named-node-type? (treesit-node-type node)))

(defun haskell-ts--function-name (node)
  (cl-assert (haskell-ts--is-toplevel-function-related-node? node)
             nil
             "Unexpected non-function treesit node: %s"
             node)
  (if-let ((name-node (treesit-node-child-by-field-name node "name")))
      (treesit-node-text-no-properties-unsafe name-node)
    (if-let ((first-child (treesit-node-child node 0))
             ((string= "infix" (treesit-node-type first-child)))
             (op (treesit-node-child-by-field-name first-child "operator"))
             ((string= "infix_id" (treesit-node-type op)))
             (name (treesit-node-child op 1)))
        (treesit-node-text-no-properties-unsafe name)
      (error "Cannot obtain function nome from node: %s" node))))

(defun haskell-ts--search-non-comment-nodes
    (start search-forward? found-predicate stop-after-first-find?)
  "CONTINUE-PREDICATE should take treesit-node and return boolean whether to continue scanning."
  (let ((tmp start)
        (continue? t)
        (result nil)
        (continue-after-first-find? (not stop-after-first-find?)))
    (while (and continue?
                (setq tmp (if search-forward?
                              (treesit-node-next-sibling tmp)
                            (treesit-node-prev-sibling tmp))))
      (let ((tmp-type (treesit-node-type tmp)))
        (cond
          ((funcall found-predicate tmp tmp-type)
           (setf result tmp
                 continue? continue-after-first-find?))
          ((haskell-ts--is-comment-node-type? tmp-type)
           ;; Continue search.
           )
          (t
           (setf continue? nil)))))
    result))

(defun haskell-ts--bounds-of-toplevel-entity (pos do-scan-around? scan-forward? find-furthest-start? find-furthest-end?)
  "Recognizes functions spanning multiple cases as a single entity.

Classes and data declarations are atomic entities and their
indented block will be their bounds without any extra processing."
  (when-let ((node
              (let* ((current-node (treesit-node-at pos))
                     (n-typ (treesit-node-type current-node)))
                (cond
                  ((string= "declarations" n-typ)
                   (when do-scan-around?
                     (let ((next-pos
                            (save-excursion
                              (goto-char pos)
                              (if scan-forward?
                                  (progn
                                    (skip-whitespace-forward)
                                    (min (point) (point-max)))
                                (progn
                                  (skip-whitespace-backward)
                                  (forward-char -1)
                                  (max (point) (point-min)))))))
                       (unless (eq pos next-pos)
                         (treesit-node-at next-pos)))))
                  ((haskell-ts--is-comment-node-type? n-typ)
                   (let ((func-node-above (haskell-ts--search-non-comment-nodes
                                           current-node
                                           nil
                                           (lambda (_ typ)
                                             (haskell-ts--is-toplevel-function-related-named-node-type? typ))
                                           t))
                         (func-node-below (haskell-ts--search-non-comment-nodes
                                           current-node
                                           t
                                           (lambda (_ typ)
                                             (haskell-ts--is-toplevel-function-related-named-node-type? typ))
                                           t)))

                     (cond
                       ((and func-node-above
                             func-node-below
                             (string= (haskell-ts--function-name func-node-above)
                                      (haskell-ts--function-name func-node-below)))
                        (if scan-forward?
                            func-node-below
                          func-node-above))
                       (do-scan-around?
                        (if-let ((result (if scan-forward?
                                             func-node-below
                                           func-node-above)))
                            result
                          (haskell-ts--search-non-comment-nodes
                           current-node
                           scan-forward?
                           (lambda (_ typ)
                             (not (haskell-ts--is-comment-node-type? typ)))
                           t)))
                       (t
                        nil))))
                  (t
                   current-node)))))
    (let ((p nil))
      (while (and (setq p (treesit-node-parent node))
                  (not (string= (treesit-node-type p) "declarations")))
        (setf node p))
      (if (haskell-ts--is-toplevel-function-related-node? node)
          (let ((func-name (haskell-ts--function-name node))
                (first-node node)
                (last-node node))

            ;; Search backward as much as possible.
            (when find-furthest-start?
              (setf first-node
                    (or (haskell-ts--search-non-comment-nodes
                         first-node
                         nil
                         (lambda (x typ)
                           (and (haskell-ts--is-toplevel-function-related-named-node-type? typ)
                                (string= func-name (haskell-ts--function-name x))))
                         nil)
                        first-node)))

            ;; Search forward as much as possible.
            (when find-furthest-end?
              (setf last-node
                    (or (haskell-ts--search-non-comment-nodes
                         last-node
                         t
                         (lambda (x typ)
                           (and (haskell-ts--is-toplevel-function-related-named-node-type? typ)
                                (string= func-name (haskell-ts--function-name x))))
                         nil)
                        last-node)))

            (cons (treesit-node-start first-node)
                  (treesit-node-end last-node)))

        (cons (treesit-node-start node)
              (treesit-node-end node))))))

(defun haskell-ts-indent-defun (pos)
  "Indent the current function."
  (interactive "d")
  (if-let ((bounds (haskell-ts--bounds-of-toplevel-entity pos nil nil t t)))
      (indent-region (car bounds) (cdr bounds))
    (error "No function at point")))

(defconst haskell-ts--treesit-simple-indent-presets
  (append
   (list (cons 'match
               (lambda
                 (&optional node-type parent-type node-field
                            node-index-min node-index-max)
                 (lambda (node parent &rest _)
                   (and (pcase node-type
                          ('nil t)
                          ('null (null node))
                          (_ (awhen (treesit-node-type node)
                               (string= node-type it))))
                        (or (null parent-type)
                            (awhen (treesit-node-type parent)
                              (string= parent-type it)))
                        (or (null node-field)
                            (awhen (treesit-node-field-name node)
                              (string= node-field it)))
                        (or (null node-index-min)
                            (>= (treesit-node-index node)
                                node-index-min))
                        (or (null node-index-max)
                            (<= (treesit-node-index node)
                                node-index-max))))))
         (cons 'n-p-gp
               (lambda (node-t parent-t grand-parent-t)
                 (lambda (node parent &rest _)
                   (and (or (null node-t)
                            (awhen (treesit-node-type node)
                              (string= node-t it)))
                        (or (null parent-t)
                            (awhen (treesit-node-type parent)
                              (string= parent-t it)))
                        (or (null grand-parent-t)
                            (when-let ((gp (treesit-node-parent parent))
                                       (gpt (treesit-node-type gp)))
                              (string= grand-parent-t gpt)))))))
         (cons 'parent-is (lambda (&rest types)
                            (lambda (_n parent &rest _)
                              (member (treesit-node-type parent) types))))

         (cons 'node-is (lambda (type)
                          (lambda (node &rest _)
                            (awhen (treesit-node-type node)
                              (string= type it)))))
         (cons 'field-is (lambda (name)
                           (lambda (node &rest _)
                             (awhen (treesit-node-field-name node)
                               (string= name it))))))

   (--map (assq it treesit-simple-indent-presets)
          '(no-node
            comment-end
            catch-all
            query
            first-sibling
            nth-sibling
            parent
            comment-start
            prev-adaptive-prefix
            grand-parent
            great-grand-parent

            parent-bol

            standalone-parent
            prev-sibling
            no-indent
            prev-line
            column-0
            and
            or
            not
            list))))

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
  (setq-local treesit-simple-indent-presets
              haskell-ts--treesit-simple-indent-presets
              treesit-simple-indent-rules
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
