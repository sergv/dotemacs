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

(provide 'haskell-ts-getters)

;; Local Variables:
;; End:

;; haskell-ts-getters.el ends here
