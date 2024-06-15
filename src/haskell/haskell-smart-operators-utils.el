;; haskell-smart-operators-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 26 February 2024
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(defun haskell-smart-operators--treesit--current-node ()
  (condition-case nil
      (treesit-node-at (point))
    (treesit-no-parser nil)))

(defun haskell-smart-operators--treesit--in-quasiquote-body? (node)
  (and node
       (equal (treesit-node-type node) "quasiquote_body")))

(defun haskell-smart-operators--treesit--in-string? (node)
  (when node
    (when (member (treesit-node-type node) '("char" "string" "quasiquote_body"))
      (and (not (eq (point) (treesit-node-start node)))
           (not (eq (point) (treesit-node-end node)))))))

(defun haskell-smart-operators--treesit--in-comment? (node)
  (when node
    (member (treesit-node-type node) '("comment" "haddock" "pragma"))))

;;;###autoload
(defun haskell-smart-operators--in-string-syntax?-raw (node)
  (or (haskell-smart-operators--treesit--in-string? node)
      (smart-operators--in-string-syntax?)))

;;;###autoload
(defun haskell-smart-operators--in-string-syntax? ()
  (haskell-smart-operators--in-string-syntax?-raw (haskell-smart-operators--treesit--current-node)))

;;;###autoload
(defun haskell-smart-operators--literal-insertion? (&optional disable-comment-check?)
  (let ((node (haskell-smart-operators--treesit--current-node)))
    (or (haskell-smart-operators--treesit--in-string? node)
        (haskell-smart-operators--treesit--in-comment? node)
        (smart-operators--literal-insertion? disable-comment-check?))))

(defun haskell-smart-operators--treesit--in-import-list? (node)
  (when node
    (equal (treesit-node-type node) "import_list")))

(defun haskell-smart-operators--in-import-list? ()
  (let ((node (haskell-smart-operators--treesit--current-node)))
    (treesit-utils-find-topmost-parent-limited node
                                               (lambda (x)
                                                 (haskell-smart-operators--treesit--in-import-list? x))
                                               5)))

(provide 'haskell-smart-operators-utils)

;; Local Variables:
;; End:

;; haskell-smart-operators-utils.el ends here
