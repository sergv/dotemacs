;; haskell-ts-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 February 2024
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'dash))

(require 'common)
(require 'common-whitespace)
(require 'interval-with-margins)
(require 'haskell-lexeme)
(require 'haskell-syntax-table)
(require 'haskell-ts-indent)
(require 's)
(require 'treesit)
(require 'treesit-utils)

(declare-function treesit-parser-create "treesit.c")

(defconst haskell-ts-mode-syntax-table haskell-mode-syntax-table)

(defface haskell-ts-haddock-face
  '((t :inherit font-lock-comment-face :bold t))
  "How to fontify Haddock omments, e.g. ‘-- |’"
  :group 'haskell-appearance)

(defface haskell-ts-comment-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight Haskell comments."
  :group 'haskell-appearance)

(defface haskell-ts-constant-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight Haskell constants."
  :group 'haskell-appearance)

(defface haskell-ts-quasiquote-pipe-face
  '((t :inherit default))
  "Face used to highlight ‘|’ within Haskell quasiuqote syntax."
  :group 'haskell-appearance)

(defface haskell-ts-string-face
  '((t :inherit font-lock-string-face))
  "Face used to highlight Haskell strings."
  :group 'haskell-appearance)

(defface haskell-ts-pragma-face
  '((t :inherit haskell-pragma-face))
  "Face used to highlight Haskell pragmas ({-# ... #-})."
  :group 'haskell-appearance)

(defface haskell-ts-keyword-face
  '((t :inherit haskell-keyword-face))
  "Face used to highlight Haskell keywords."
  :group 'haskell-appearance)

(defface haskell-ts-operator-face
  '((t :inherit haskell-operator-face))
  "Face used to highlight Haskell operators."
  :group 'haskell-appearance)

(defface haskell-ts-type-face
  '((t :inherit haskell-type-face))
  "Face used to highlight Haskell types"
  :group 'haskell-appearance)

(defface haskell-ts-constructor-face
  '((t :inherit haskell-constructor-face))
  "Face used to highlight Haskell constructors."
  :group 'haskell-appearance)

(defface haskell-ts-strictness-face
  '((t :inherit font-lock-negation-char-face))
  "How to fontify ! and ~ in strictness contexts."
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
      ((comment) @haskell-ts-comment-face)

      ((haddock) @haskell-ts-haddock-face)

      ;; constant
      ([(integer) (float)] @haskell-ts-constant-face)

      (quasiquote "|" @haskell-ts-quasiquote-pipe-face)

      ;; string
      ([(char) (string) (quasiquote_body)] @haskell-ts-string-face)

      ;; preprocessor
      ([(pragma) (cpp)] @haskell-ts-pragma-face)

      ;; keyword
      (lambda_cases
       "\\"
       ("cases" @haskell-ts-keyword-face))

      (default_signature
       ("default" @haskell-ts-keyword-face))

      (pattern_synonym
       ("pattern" @haskell-ts-keyword-face))

      (namespace
       ("pattern" @haskell-ts-keyword-face))

      ((variable) @haskell-ts-keyword-face
       (:equal "_" @haskell-ts-keyword-face))

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
        "foreign"
        (wildcard "_")
        ]
       @haskell-ts-keyword-face)

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
       @haskell-ts-operator-face)

      (forall "." @haskell-ts-keyword-face)

      ;; (unboxed_tuple "(#" @haskell-ts-keyword-face)
      ;; (unboxed_tuple "#)" @haskell-ts-keyword-face)

      ((foreign_import
        [(calling_convention)
         (safety)]
        @haskell-ts-keyword-face))

      ((infix_id
        "`"
        [(name)
         (qualified (name))
         (variable)
         (qualified (variable))
         (constructor)
         (qualified (constructor))]
        "`")
       @haskell-ts-operator-face)

      ;; module-name
      ;; Competes with (module)
      ;; ((qualified (variable)) @default)

      (import (module) @haskell-ts-type-face)
      (header (module) @haskell-ts-type-face)
      (module_export (module) @haskell-ts-type-face)

      ;; type
      ;; ((signature name: (variable) @haskell-ts-type-face))

      ;; Handles all types
      (((name) @haskell-ts-type-face)
       (:pred haskell-ts-mode--name-not-within-infix? @haskell-ts-type-face))
      (((qualified (module) (name)) @haskell-ts-type-face)
       (:pred haskell-ts-mode--name-not-within-infix? @haskell-ts-type-face))

      ;; constructor
      (((constructor) @haskell-ts-constructor-face)
       (:pred haskell-ts-mode--name-not-within-infix? @haskell-ts-constructor-face))
      ((qualified (module) @haskell-ts-type-face
                  [(constructor) (constructor_operator)] @haskell-ts-constructor-face)
       (:pred haskell-ts-mode--name-not-within-infix? @haskell-ts-constructor-face))

      ([(unit) (list "[" !element "]") (constructor_operator)] @haskell-ts-constructor-face)

      ;; strictness
      ([(strict_field "!") (strict "!")] @haskell-ts-mode--fontify-bang)

      ;; laziness
      ([(lazy_field) (irrefutable "~")] @haskell-ts-mode--fontify-tilde)))))

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
      (put-text-property p (1+ p) 'face 'haskell-ts-strictness-face))))

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
  (save-restriction
    (widen)
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
                (treesit-node-end node)))))))

(defun haskell-ts-indent-defun (pos)
  "Indent the current function."
  (interactive "d")
  (if-let ((bounds (haskell-ts--bounds-of-toplevel-entity pos nil nil t t)))
      (indent-region (car bounds) (cdr bounds))
    (error "No function at point")))

(defun haskell-ts-convert-to-multiline-string (&optional preserve-newline-at-end?)
  "Convert string literal to multiline syntax."
  (interactive "P")
  (when-let* ((node (treesit-node-at (point))))
    (let ((qq-node nil)
          (triple-delim "\"\"\""))
      (cond
        ((setf qq-node (treesit-utils-find-topmost-parent
                        node
                        (lambda (x)
                          (string= "quasiquote" (treesit-node-type x)))))
         (if-let* ((body (treesit-node-child-by-field-name qq-node "body")))
             (save-position-marker-unsafe
               (let ((contents (treesit-node-text-no-properties-unsafe body)))
                 (goto-char (treesit-node-end body))
                 (delete-region (treesit-node-end body)
                                (treesit-node-end qq-node))
                 (insert triple-delim)
                 (goto-char (treesit-node-start qq-node))
                 (delete-region (treesit-node-start qq-node)
                                (treesit-node-start body))
                 (insert triple-delim)
                 (haskell-misc--ensure-language-pragma "MultilineStrings")))
           (error "Point is at quasiquoter node without body")))
        ((string= "string" (treesit-node-type node))
         (let ((start (treesit-node-start node))
               (end (treesit-node-end node)))
           (if (not (or (text-after-pos-matches? start triple-delim)
                        (text-before-pos-matches? end triple-delim)))
               (save-match-data
                 (save-position-marker-unsafe
                   (goto-char start)
                   (let* ((indent (make-string (- (point) (line-beginning-position)) ?\s))
                          (newline-indent (concat "\n" indent))
                          (double-delim "\"\""))
                     (goto-char end)
                     ;; Remove last newline if present
                     (forward-char -1)

                     (if (and (not (when (text-before-matches? "\\n")
                                     (delete-char -2)
                                     t))
                              preserve-newline-at-end?)
                         (insert "\\" newline-indent "\\" double-delim)
                       (insert newline-indent double-delim))

                     ;; After this we’ll be right before the final triple """.
                     (forward-char -2)
                     (with-marker (end-mark (copy-marker (point)))
                       (goto-char (+ start 1))
                       (insert-before-markers double-delim newline-indent)

                       ;; Fix old style multiline strings.
                       (goto-char start)
                       (while (re-search-forward
                               (rx (or bos (not ?\\))
                                   (* (seq ?\\ ?\\))
                                   (group-n 1
                                     ?\\ ?n
                                     ?\\
                                     (? ?\r) ?\n
                                     (* (any ?\s)) ?\\))
                               end-mark
                               t)
                         (replace-match-insert-before-markers newline-indent 1)
                         (goto-char (match-beginning 0)))

                       ;; Fix regular newlines.
                       (goto-char start)
                       (while (re-search-forward
                               (rx (or bos (not ?\\))
                                   (* (seq ?\\ ?\\))
                                   (group-n 1 ?\\ ?n))
                               end-mark
                               t)
                         (replace-match-insert-before-markers newline-indent 1))

                       ;; Fix escaped double quotes
                       (goto-char start)
                       (while (re-search-forward
                               (rx (or bos
                                       (not (any ?\"))
                                       (seq (or bos
                                                (not (any ?\\)))
                                            (+ (seq ?\\ ?\\))))
                                   (group-n 1 (** 1 2 (seq ?\\ ?\")))
                                   (or (seq ?\\ (not ?\"))
                                       (not ?\\)
                                       eos))
                               end-mark
                               t)
                         (replace-match-insert-before-markers
                          (cl-remove-if (lambda (c) (eq c ?\\)) (match-string 1))
                          1)))

                     (haskell-misc--ensure-language-pragma "MultilineStrings"))))
             (error "String literal at point is already multiline"))))
        (t
         (error "Node at point is neither string nor quasiquote: %s"
                node))))))

(defconst haskell-ts--imported-name-query
  (treesit-query-compile 'haskell
                         '(((import_name) @name))))

(defun haskell-ts-remove-from-import-statement-at (pos names)
  (cl-assert (listp names))
  (cl-assert (-all? #'stringp names))
  (let ((import-node (treesit-utils-find-topmost-parent
                      (treesit-node-at pos)
                      (lambda (x) (string= "import" (treesit-node-type x))))))
    (unless (treesit-node-p import-node)
      (error "Cannot find import node at point"))

    (let ((import-list (treesit-node-child-by-field-name import-node "names")))
      (unless (and (treesit-node-p import-list)
                   (string= "import_list"
                            (treesit-node-type import-list)))
        (error "Cannot find import list in the import node at point: %s, ‘%s’"
               import-node
               (treesit-node-text-no-properties-unsafe import-node)))

      (let ((imported-names (treesit-node-children import-list t))

            (constructors (make-hash-table :test #'equal))
            (other (make-hash-table :test #'equal)))
        (dolist (name imported-names)
          (unless (and (treesit-node-p name)
                       (string= "import_name" (treesit-node-type name)))
            (error "Invalid imported name node: %s" name))

          (if-let* ((variable (treesit-node-child-by-field-name name "variable")))
              (let ((str (treesit-node-text-no-properties-unsafe variable)))
                ;; Use whole import_name so that we’ll be able to
                ;; detect commas before and after it. There would be
                ;; no commas around its inner ‘variable’.
                (puthash str name other))
            (when-let* ((type (treesit-node-child-by-field-name name "type")))
              (let ((str (treesit-node-text-no-properties-unsafe type)))
                (puthash str
                         ;; NB whone import_name is added here to be
                         ;; removed with its children if we’re going
                         ;; to remove it.
                         name
                         other))
              (when-let* ((children (treesit-node-child-by-field-name name "children"))
                          (constructor-nodes (treesit-node-children children t)))
                (dolist (constructor constructor-nodes)
                  (let ((typ (treesit-node-type constructor)))
                    (unless (member typ '("constructor" "all_names"))
                      (error "Unexpected constructor node: %s" constructor))
                    (when (string= "constructor" typ)
                      (let ((str (treesit-node-text-no-properties-unsafe constructor)))
                        (puthash str constructor constructors)))))))))

        (let ((remove-nodes nil))
          (dolist (name names)

            ;; Transform Executable(buildInfo) -> buildInfo for when
            ;; we imported a type’s accessor but GHC reports it together
            ;; with parent type name.
            (when-let (m (s-match (rx
                                   (* (any ?_))
                                   (any (?A . ?Z))
                                   (* alphanumeric)
                                   "("
                                   (group-n 1 (+ (not ?\))))
                                   ")")
                                  name))
              (setf name (nth 1 m)))

            (let ((tmp nil))
              (cond
                ((setf tmp (gethash name constructors))
                 (remhash name constructors)
                 (push tmp remove-nodes))
                ((setf tmp (gethash name other))
                 (remhash name constructors)
                 (push tmp remove-nodes)))))

          (let ((remove-intervals nil))
            (dolist (node remove-nodes)
              (let ((margin-before (when-let* ((prev (treesit-node-prev-sibling node))
                                               ((string= "," (treesit-node-type prev)))
                                               (prev2 (treesit-node-prev-sibling prev)))
                                     (- (treesit-node-start node)
                                        (treesit-node-end prev2))))
                    (margin-after (when-let* ((next (treesit-node-next-sibling node))
                                              ((string= "," (treesit-node-type next)))
                                              (next2 (treesit-node-next-sibling next)))
                                    (- (treesit-node-start next2) (treesit-node-end node)))))
                (push (mk-interval-with-margins (treesit-node-start node)
                                                (treesit-node-end node)
                                                ;; If surrounded by commas then remove the node
                                                ;; itself, margins will be nil.
                                                margin-before
                                                margin-after)
                      remove-intervals)))

            (let ((merged (nreverse (interval-with-margins-merge-overlapping! remove-intervals))))
              (dolist (interval merged)
                (delete-region (interval-with-margins-resolve-start interval)
                               (interval-with-margins-resolve-end interval))))))))))

;;;###autoload
(define-derived-mode haskell-ts-base-mode prog-mode "Haskell[ts]"
  "Bare-bones major mode for Haskell that uses tree-sitter."

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
  (haskell-ts-indent-setup)

  (setq-local comment-start "--"
              comment-padding 1
              comment-start-skip "[-{]-[ \t]*"
              comment-end ""
              comment-end-skip "[ \t]*\\(-}\\|\\s>\\)"
              fill-paragraph-function #'haskell-fill-paragraph)

  (treesit-major-mode-setup))

(define-derived-mode haskell-ts-mode haskell-ts-base-mode "Haskell[ts]"
  "Major mode for Haskell that uses tree-sitter.")

(provide 'haskell-ts-mode)

;; Local Variables:
;; End:

;; haskell-ts-mode.el ends here
