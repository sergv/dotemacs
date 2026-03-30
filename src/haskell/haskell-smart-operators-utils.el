;; haskell-smart-operators-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 26 February 2024
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'haskell-ts-mode)
(require 'treesit-utils)

(defun haskell-smart-operators--treesit--in-quasiquote-body? (node)
  (and node
       (string= (treesit-node-type node) "quasiquote_body")))

(defun haskell-smart-operators--treesit--in-string?-sure (p node)
  (when (treesit-haskell--is-string-node-type? (treesit-node-type node))
    (and (< (treesit-node-start node) p)
         (< p (treesit-node-end node)))))

(defsubst haskell-smart-operators--treesit--in-string? (p node)
  (when node
    (haskell-smart-operators--treesit--in-string?-sure p node)))

(defun haskell-smart-operators--treesit--in-comment?-sure (node)
  (treesit-haskell--is-comment-node-type? (treesit-node-type node)))

(defsubst haskell-smart-operators--treesit--in-comment? (node)
  (when node
    (haskell-smart-operators--treesit--in-comment?-sure node)))

;;;###autoload
(defun haskell-smart-operators--in-string-syntax?-raw (p node)
  (or (haskell-smart-operators--treesit--in-string? p node)
      (if (bobp)
          nil
        (and (eq (syntax-class (syntax-after (1- p))) 7)
             (eq (syntax-class (syntax-after p)) 7)))))

;;;###autoload
(defun haskell-smart-operators--in-string-syntax? ()
  (let ((p (point)))
    (haskell-smart-operators--in-string-syntax?-raw p (treesit-haskell--node-at p))))

;;;###autoload
(defun haskell-smart-operators--literal-insertion? (&optional disable-comment-check?)
  (or (when-let ((node (treesit-haskell--current-node)))
        (let ((p (point)))
          ;; If we’re not within current node then we’re in a space-filled
          ;; limbo. But that means we’re definitely not in a string or
          ;; comment so literal insertion context they entail does not apply.
          (and (<= (treesit-node-start node) p)
               (<= p (treesit-node-end node))
               (or (haskell-smart-operators--treesit--in-string?-sure p node)
                   (and (not disable-comment-check?)
                        (haskell-smart-operators--treesit--in-comment?-sure node))))))
      (smart-operators--literal-insertion? disable-comment-check?)))

(defun haskell-smart-operators--treesit--in-import-list? (node)
  (string= (treesit-node-type node) "import_list"))

(defun haskell-smart-operators--in-import-list? ()
  (awhen (treesit-haskell--current-node)
    (treesit-utils-find-closest-parent-limited
     it
     #'haskell-smart-operators--treesit--in-import-list?
     5)))

(defun haskell-smart-operators--treesit--in-export-list? (node)
  (string= (treesit-node-type node) "exports"))

(defun haskell-smart-operators--in-export-list? ()
  (awhen (treesit-haskell--current-node)
    (treesit-utils-find-closest-parent-limited
     it
     #'haskell-smart-operators--treesit--in-export-list?
     5)))

(defun haskell-smart-operators--in-single-quote-context? (node)
  "Checkis if current point is where single quotes should not be
automatically duplicated."
  (not (null (treesit-utils-find-closest-parent
              node
              (lambda (x)
                (let ((typ (treesit-node-type x)))
                  (or (string= typ "type_family")
                      (string= typ "signature")
                      (string= (treesit-node-field-name x) "type"))))))))

(provide 'haskell-smart-operators-utils)

;; Local Variables:
;; End:

;; haskell-smart-operators-utils.el ends here
