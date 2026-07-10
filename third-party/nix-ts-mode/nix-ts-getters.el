;; nix-ts-getters.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 July 2026
;; Description:


(eval-when-compile
  (require 'cl)
  (require 'dash)
  (require 'macro-util))

(require 'common)
(require 'common-whitespace)
(require 'haskell-lexeme)
(require 'treesit)
(require 'treesit-utils)

(defun nix-ts-getters--find-child (node type-to-find)
  (catch 'result
    (dotimes (i (treesit-node-child-count node))
      (let ((child (treesit-node-child node i)))
        (when (string= type-to-find (treesit-node-type child))
          (throw 'result child))))))

(defun nix-ts-getters--if-expression-if (node)
  (cl-assert (string= (treesit-node-type node) "if_expression"))
  (let ((result (nix-ts-getters--find-child node "if")))
    (cl-assert (string= "if" (treesit-node-type result))
               nil
               "Not an ‘if’ node: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun nix-ts-getters--if-expression-then (node)
  (cl-assert (string= (treesit-node-type node) "if_expression"))
  (let ((result (nix-ts-getters--find-child node "then")))
    (cl-assert (string= "then" (treesit-node-type result))
               nil
               "Not a ‘then’ node: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun nix-ts-getters--if-expression-else (node)
  (cl-assert (string= (treesit-node-type node) "if_expression"))
  (let ((result (nix-ts-getters--find-child node "else")))
    (cl-assert (string= "else" (treesit-node-type result))
               nil
               "Not a ‘then’ node: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun nix-ts-getters--let-expression-in (node)
  (cl-assert (string= (treesit-node-type node) "let_expression"))
  (let ((result (nix-ts-getters--find-child node "in")))
    (cl-assert (string= "in" (treesit-node-type result))
               nil
               "Not an ‘in’ node: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun nix-ts-getters--string-first-delim (node)
  (cl-assert (member (treesit-node-type node) '("string_expression" "indented_string_expression")))
  (let ((result (treesit-node-child node 0)))
    (cl-assert (member (treesit-node-type result) '("\"" "''"))
               nil
               "Not string delimiter node: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(defun nix-ts-getters--string-last-delim (node)
  (cl-assert (member (treesit-node-type node) '("string_expression" "indented_string_expression")))
  (let ((result (treesit-node-child node -1)))
    (cl-assert (member (treesit-node-type result) '("\"" "''"))
               nil
               "Not string delimiter node: %s, node = %s, parent = %s"
               result
               node
               (treesit-node-parent node))
    result))

(provide 'nix-ts-getters)

;; Local Variables:
;; End:

;; nix-ts-getters.el ends here
