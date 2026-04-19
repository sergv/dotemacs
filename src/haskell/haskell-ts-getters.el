;; haskell-ts-getters.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  1 November 2025
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'dash)
  (require 'macro-util))

(defvar haskell-indent-offset)

(require 'common)
(require 'common-whitespace)
(require 'haskell-lexeme)
(require 'treesit)
(require 'treesit-utils)

(defun haskell-ts-getters--get-opening-paren (node)
  (cl-assert (member (treesit-node-type node) '("parens" "tuple")))
  (let ((result (treesit-node-child node 0)))
    (cl-assert (string= "(" (treesit-node-type result))
               nil
               "Not an opening paren: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-getters--get-opening-generic-paren (node)
  (cl-assert (member (treesit-node-type node) '("parens" "tuple" "unboxed_tuple" "list")))
  (let ((result (treesit-node-child node 0)))
    (cl-assert (member (treesit-node-type result) '("(" "(#" "[" "{"))
               nil
               "Not an generic opening paren: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-getters--get-quasiquote-opening-bracket (node)
  (cl-assert (string= (treesit-node-type node) "quasiquote"))
  (let ((result (treesit-node-child node 0)))
    (cl-assert (string= "[" (treesit-node-type result))
               nil
               "Not an opening bracket %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-getters--get-parens-content (node)
  (cl-assert (string= (treesit-node-type node) "parens"))
  (if (= (treesit-node-child-count node) 3)
      (treesit-node-child node 1)
    (error "Unexpected parens node: %s" node)))


(defun haskell-ts-getters--get-closing-paren (node)
  (cl-assert (member (treesit-node-type node) '("parens" "tuple")))
  (let ((result (treesit-node-child node -1)))
    (cl-assert (string= ")" (treesit-node-type result))
               nil
               "Not a closing paren: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-record-or-fields-open-brace (node)
  (cl-assert (member (treesit-node-type node) '("record" "fields")))
  (let* ((typ (treesit-node-type node))
         (open-brace-idx-candidates
          (cond
            ((string= "record" typ)
             '(0 1))
            ((string= "fields" typ)
             '(0))
            (t
             (error "Inexpected record-like node: %s" node))))
         (result
          (--some (when-let ((open-brace (treesit-node-child node it)))
                    (when (string= "{" (treesit-node-type open-brace))
                      open-brace))
                  open-brace-idx-candidates)))
    (cl-assert (or (null result)
                   (string= "{" (treesit-node-type result)))
               nil
               "Not an open brace node: %s, node = %s, parent = %s"
               open-brace
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-signature-double-colon (node)
  "Get ‘::’ from a signature NODE."
  (cl-assert (string= "signature" (treesit-node-type node)))
  (let ((result (treesit-node-child node 1)))
    (cl-assert (or (null result)
                   (string= "::" (treesit-node-type result)))
               nil
               "Not a double colon: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-signature-name (node)
  "Get ‘name’ field from a signature NODE."
  (cl-assert (string= "signature" (treesit-node-type node)))
  (let ((result (treesit-node-child-by-field-name node "name")))
    (cl-assert (not (null result))
               nil
               "No ‘name’ in signature node: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-context-arrow (node)
  "Get ‘=>’ from a context NODE."
  (cl-assert (string= "context" (treesit-node-type node)))
  (let ((result (treesit-node-child-by-field-name node "arrow")))
    (cl-assert (or (null result)
                   (string= "=>" (treesit-node-type result)))
               nil
               "Not ‘=>’: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-context-context (node)
  "Get ‘context’ context from a context NODE."
  (cl-assert (string= "context" (treesit-node-type node)))
  (let ((result (treesit-node-child-by-field-name node "context")))
    (cl-assert (not (null result))
               nil
               "Empty ‘context’ field of context node: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-match-equals (node)
  (cl-assert (string= "match" (treesit-node-type node)))
  (let ((result (treesit-node-child node 0)))
    (cl-assert (or (null result)
                   (string= "=" (treesit-node-type result)))
               nil
               "Not an equals %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-match-guard-pipe (node)
  (cl-assert (string= "match" (treesit-node-type node)))
  (let ((result (treesit-node-child node 0)))
    (cl-assert (string= "|" (treesit-node-type result))
               nil
               "Not a pipe: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-match-guard-pipe-opt (node)
  (cl-assert (string= "match" (treesit-node-type node)))
  (let ((result (treesit-node-child node 0)))
    (cl-assert (or (null result)
                   (string= "|" (treesit-node-type result)))
               nil
               "Not a pipe: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-function-arrow (node)
  "Get ‘->’ from a function NODE."
  (cl-assert (string= "function" (treesit-node-type node)))
  (let ((result (treesit-node-child-by-field-name node "arrow")))
    (cl-assert (or (null result)
                   (string= "->" (treesit-node-type result)))
               nil
               "Not ‘->’: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-function-parameter (node)
  "Get ‘parameter’ field from a function NODE."
  (cl-assert (string= "function" (treesit-node-type node)))
  (let ((result (treesit-node-child-by-field-name node "parameter")))
    (cl-assert (not (null result))
               nil
               "No ‘paratemer’ field in function: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-indent--get-let-node-in (node)
  "Get ‘in’ field from a let_in NODE."
  (cl-assert (string= "let_in" (treesit-node-type node)))
  (let ((result (treesit-node-child node -2)))
    (cl-assert (and result
                    (string= (treesit-node-type result) "in"))
               nil
               "No ‘in’ field in let_in node %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-getters--extract-prefix-id-operator (node)
  (cl-assert (string= "prefix_id" (treesit-node-type node)))
  (let ((result (treesit-node-child node 1)))
    (cl-assert (and result
                    (string= (treesit-node-type result) "operator"))
               nil
               "No operator in prefix_id node %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-getters--extract-function-bind-or-signature-name-resolving-operator--extract-operator-name (node)
  (if (string= (treesit-node-type node) "prefix_id")
      (haskell-ts-getters--extract-prefix-id-operator node)
    node))

(defun haskell-ts-getters--extract-function-bind-or-signature-name-resolving-operator (node)
  (cl-assert (member (treesit-node-type node) '("function" "bind" "signature")))
  (let ((result (haskell-ts-getters--extract-function-bind-or-signature-name-resolving-operator--extract-operator-name
                 (treesit-node-child-by-field-name node "name"))))
    (cl-assert (and result
                    (member (treesit-node-type result) '("variable" "name" "constructor" "operator" "pragma")))
               nil
               "No name node in %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-getters--infix-left-operand (node)
  (cl-assert (string= (treesit-node-type node) "infix"))
  (let ((result (treesit-node-child-by-field-name node "left_operand")))
    (cl-assert result
               nil
               "No left operand in infix node, node = %s, parent = %s"
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-getters--infix-right-operand (node)
  (cl-assert (string= (treesit-node-type node) "infix"))
  (let ((result (treesit-node-child-by-field-name node "right_operand")))
    (cl-assert result
               nil
               "No right operand in infix node, node = %s, parent = %s"
               node
               (treesit-node-parent node))
    result))

(defun haskell-ts-getters--infix-operator (node)
  (cl-assert (string= (treesit-node-type node) "infix"))
  (let ((result (treesit-node-child-by-field-name node "operator")))
    (cl-assert result
               nil
               "No operator in infix node, node = %s, parent = %s"
               node
               (treesit-node-parent node))
    result))

(provide 'haskell-ts-getters)

;; Local Variables:
;; End:

;; haskell-ts-getters.el ends here
