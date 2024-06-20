;; haskell-ts-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 February 2024
;; Description:

(eval-when-compile
  (require 'dash))

(require 'common-whitespace)
(require 'haskell-lexeme)
(require 'haskell-syntax-table)
(require 'treesit)

(require 'haskell-smart-operators-mode)

(declare-function treesit-parser-create "treesit.c")

(defconst haskell-ts-mode-syntax-table haskell-mode-syntax-table)

(defface haskell-ts-hadddock-face
  '((t :inherit font-lock-comment-face :bold t))
  "How to fontify Haddocck omments, e.g. ‘-- |’"
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

      ((haddock) @haskell-ts-hadddock-face)

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

(defconst haskell-ts-indent-rules
  `(((parent-is ,(rx bos "exp_do" eos)) parent-bol 2)))

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

  (treesit-major-mode-setup))

(provide 'haskell-ts-mode)

;; Local Variables:
;; End:

;; haskell-ts-mode.el ends here
