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

(defun haskell-smart-operators--treesit--in-string? (node)
  (when node
    (memq (treesit-node-type node) '(string quasiquote_body))))

(defun haskell-smart-operators--treesit--in-comment? (node)
  (when node
    (memq (treesit-node-type node) '(comment haddock pragma))))

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

(provide 'haskell-smart-operators-utils)

;; Local Variables:
;; End:

;; haskell-smart-operators-utils.el ends here
