;; haskell-ts-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 21 February 2024
;; Description:

(eval-when-compile
  (require 'dash))

(require 'haskell-lexeme)
(require 'haskell-syntax-table)
(require 'treesit)

(require 'haskell-smart-operators-mode)

(declare-function treesit-parser-create "treesit.c")

(defconst haskell-ts-mode-syntax-table haskell-mode-syntax-table)

(defconst haskell-ts-font-lock-rules
  (--mapcat
   (cons :language
         (cons 'haskell
               (cons :feature
                     (cons (car it)
                           (list (cdr it))))))
   '((everyone

      ;; comment
      ((comment) @font-lock-comment-face)

      ;; constant
      ([(integer) (float)] @font-lock-constant-face)

      ;; string
      ([(char) (string) (quasiquote_body)] @font-lock-string-face)

      ;; preprocessor
      ([(pragma) (cpp)] @haskell-pragma-face)

      ;; keyword
      (exp_lambda_cases
       "\\"
       ("cases" @haskell-keyword-face))

      (default_signature
       ("default" @haskell-keyword-face))

      (pattern_synonym
       ("pattern" @haskell-keyword-face))

      (namespace
       ("pattern" @haskell-keyword-face))

      ([
        "forall"
        (where)
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
        ]
       @haskell-keyword-face)

      ;; module-name
      ;; Must come before (module) to override it.
      ((qualified_variable) @default)
      ([(module) (qualified_module)] @haskell-type-face)

      ;; operator
      ([
        (operator)
        ;; Must come before (module) to override it.
        (qualified_operator)
        (type_operator)
        (tycon_arrow)
        ;; (qualified_module) ; grabs the `.` (dot), ex: import System.IO
        (all_names)
        ;; (strict_type) ;; ! inside data declaration
        (wildcard)
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
      ("`" @haskell-operator-face
       [(variable) (qualified_variable) (constructor) (qualified_constructor)] @haskell-operator-face
       "`" @haskell-operator-face)

      ;; type
      ;; ((signature name: (variable) @font-lock-type-face))
      ([(qualified_type) (type)] @haskell-type-face)

      ;; constructor
      ([(constructor) (qualified_constructor) (con_unit) (constructor_operator)] @haskell-constructor-face)


      ;; strictness
      ([(strict_type) (pat_strict)] @haskell-ts-mode--fontify-bang)

      ;; laziness
      ([(lazy_type) (pat_irrefutable)] @haskell-ts-mode--fontify-tilde)

      ;; ;; quasiquote
      ;; (quoter) @injection.language
      ;; (quasiquote_body) @injection.content
      ))))

(defconst haskell-ts-indent-rules
  `(((parent-is ,(rx bos "exp_do" eos)) parent-bol 2)))

(defun haskell-ts-mode--fontify-bang (node override start end &rest _)
  (haskell-ts-mode--fontify-first-char ?! node))

(defun haskell-ts-mode--fontify-tilde (node override start end &rest _)
  (haskell-ts-mode--fontify-first-char ?~ node))

(defun haskell-ts-mode--fontify-first-char (char node)
  (let ((p (treesit-node-start node)))
    (when (eq (char-after p) char)
      (put-text-property p (1+ p) 'face 'font-lock-negation-char-face))))

(defvar haskell-ts---syntax-propertize-nonoperator-node
  (alist->hash-table (--map (cons it t) '("string" "quasiquote_body" "pragma" "cpp" "comment"))))

(defun haskell-ts-syntax-propertize (begin end)
  "Basically finds all operators (e.g. -->) that start with comment delimiter, -- that should
not be treated as comment start.

Also fix syntax of character quote delimiters because quote is a valid part of symbols as well
but when paired then it’s like a string."
  (save-match-data
    (save-excursion
      (goto-char begin)
      (while (re-search-forward
              (eval-when-compile
                (concat "\\(?:\\(?:^\\|[^!#$%&*+./:<=>?@\\^|~]\\)\\(?1:--[!#$%&*+./:<=>?@\\^|~-]*[!#$%&*+./:<=>?@\\^|~]\\)\\)"
                        "\\|"
                        "\\(?2:'\\)\\(?:"
                        haskell-lexeme-string-literal-inside-item
                        "\\)\\(?3:'\\)"))
              end
              t)
        (cond
          ;; Fix syntax of operators that start with --
          ((match-beginning 1)
           (let (node (treesit-node-at (point)))
             (when (or (not node)
                       (not (gethash (treesit-node-type node) haskell-ts---syntax-propertize-nonoperator-node)))
               (put-text-property (match-beginning 1)
                                  (match-end 1)
                                  'syntax-table
                                  (eval-when-compile (string-to-syntax "."))))))
          ;; Adjust syntax of character delimiter quotes
          ((and (match-beginning 2)
                (match-beginning 3))
           (put-text-property (match-beginning 2)
                              (match-end 2)
                              'syntax-table
                              (eval-when-compile (string-to-syntax "\"")))
           (put-text-property (match-beginning 3)
                              (match-end 3)
                              'syntax-table
                              (eval-when-compile (string-to-syntax "\"")))))))))

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
