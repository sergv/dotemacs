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

(provide 'haskell-ts-getters)

;; Local Variables:
;; End:

;; haskell-ts-getters.el ends here
