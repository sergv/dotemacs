;; haskell-smart-operators-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 26 February 2024
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'treesit-utils)

(defun haskell-smart-operators--treesit--in-quasiquote-body? (node)
  (and node
       (equal (treesit-node-type node) "quasiquote_body")))

(defun haskell-smart-operators--treesit--in-string?-sure (node)
  (when (member (treesit-node-type node) '("char" "string" "quasiquote_body"))
    (and (not (eq (point) (treesit-node-start node)))
         (not (eq (point) (treesit-node-end node))))))

(defsubst haskell-smart-operators--treesit--in-string? (node)
  (when node
    (haskell-smart-operators--treesit--in-string?-sure node)))

(defun haskell-smart-operators--treesit--in-comment?-sure (node)
  (member (treesit-node-type node) '("comment" "haddock" "pragma")))

(defsubst haskell-smart-operators--treesit--in-comment? (node)
  (when node
    (haskell-smart-operators--treesit--in-comment?-sure node)))

;;;###autoload
(defun haskell-smart-operators--in-string-syntax?-raw (node)
  (or (haskell-smart-operators--treesit--in-string? node)
      (smart-operators--in-string-syntax?)))

;;;###autoload
(defun haskell-smart-operators--in-string-syntax? ()
  (haskell-smart-operators--in-string-syntax?-raw (treesit-haskell--current-node)))

;;;###autoload
(defun haskell-smart-operators--literal-insertion? (&optional disable-comment-check?)
  (or (when-let ((node (treesit-haskell--current-node)))
        (let ((p (point)))
          ;; If we’re not within current node then we’re in a space-filled
          ;; limbo. But that means we’re definitely not in a string or
          ;; comment so literal insertion context they entail does not apply.
          (and (<= (treesit-node-start node) p)
               (<= p (treesit-node-end node))
               (or (haskell-smart-operators--treesit--in-string?-sure node)
                   (haskell-smart-operators--treesit--in-comment?-sure node)))))
      (smart-operators--literal-insertion? disable-comment-check?)))

(defun haskell-smart-operators--treesit--in-import-list? (node)
  (when node
    (equal (treesit-node-type node) "import_list")))

(defun haskell-smart-operators--in-import-list? ()
  (when-let ((node (treesit-haskell--current-node)))
    (treesit-utils-find-topmost-parent-limited node
                                               (lambda (x)
                                                 (haskell-smart-operators--treesit--in-import-list? x))
                                               5)))

(provide 'haskell-smart-operators-utils)

;; Local Variables:
;; End:

;; haskell-smart-operators-utils.el ends here
