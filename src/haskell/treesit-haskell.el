;; treesit-haskell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  4 September 2026
;; Description:

(eval-when-compile
  (require 'cl-lib))

(declare-function treesit-buffer-root-node "treesit")
(declare-function treesit-filter-child "treesit")
(declare-function treesit-node-at "treesit")

(defvar haskell-regexen/inline-pragmas-complete-pragma)
(defvar haskell-ts-buffer-lang)

(require 'buffer-span)
(require 'common)
(require 'common-small)
(require 'semnav)

(require 'treesit-utils)

(defsubst treesit-haskell--current-node ()
  (treesit-haskell--node-at (point)))

(defun treesit-haskell--node-at (pos)
  (when (derived-mode-p 'haskell-ts-base-mode)
    (treesit-node-at pos
                     ;; Hoping the parser will get reused, should be safe for
                     ;; haskell-ts-mode and its derivatives.
                     (treesit-parser-create haskell-ts-buffer-lang))))

(defun treesit-haskell--is-multiline-string? (node)
  (declare (pure nil) (side-effect-free t))
  (cl-assert (treesit-node-p node))
  (and (string= "string" (treesit-node-type node))
       (text-after-pos-matches? (treesit-node-start node) "\"\"\"")
       (text-before-pos-matches? (treesit-node-end node) "\"\"\"")))

(defun treesit-haskell--is-pragma-node-type? (typ)
  (declare (pure t) (side-effect-free t))
  (cl-assert (stringp typ))
  (string= typ "pragma"))

(defun treesit-haskell--is-comment-node-type? (typ)
  (declare (pure t) (side-effect-free t))
  (cl-assert (stringp typ))
  (or (string= typ "comment")
      (string= typ "haddock")
      (treesit-haskell--is-pragma-node-type? typ)))

(defun treesit-haskell--is-string-node-type? (typ)
  (declare (pure t) (side-effect-free t))
  (cl-assert (stringp typ))
  (or (string= typ "char")
      (string= typ "string")
      (string= typ "quasiquote_body")))

(defun treesit-haskell--is-inside-string-node? (p node)
  (declare (pure t) (side-effect-free t))
  (treesit-utils-is-inside-string-node?
   p
   node
   (lambda (_ x) (treesit-haskell--is-string-node-type? (treesit-node-type x)))))

(defun treesit-haskell--is-inside-comment-node? (p node)
  (declare (pure t) (side-effect-free t))
  (treesit-utils-is-inside-comment-node?
   p
   node
   (lambda (_ x) (treesit-haskell--is-comment-node-type? (treesit-node-type x)))))

(defun treesit-haskell--is-inside-string-or-comment-node? (p node)
  (declare (pure t) (side-effect-free t))
  (treesit-utils-is-inside-string-or-comment-node?
   p
   node
   (lambda (_ x) (treesit-haskell--is-string-node-type? (treesit-node-type x)))
   (lambda (_ x) (treesit-haskell--is-comment-node-type? (treesit-node-type x)))))

(defun treesit-haskell--is-not-inside-string-or-comment-node? (p node)
  (declare (pure t) (side-effect-free t))
  (treesit-utils-is-not-inside-string-or-comment-node?
   p
   node
   (lambda (_ x) (treesit-haskell--is-string-node-type? (treesit-node-type x)))
   (lambda (_ x) (treesit-haskell--is-comment-node-type? (treesit-node-type x)))))

(defun treesit-haskell-get-buffer-module-name ()
  (if-let* ((header-candidates (treesit-filter-child
                                (treesit-buffer-root-node haskell-ts-buffer-lang)
                                (lambda (node)
                                  (string= "header" (treesit-node-type node))))))
      (treesit-node-text-no-properties-unsafe (treesit-node-child-by-field-name (car header-candidates) "module"))
    "Main"))

(cl-defstruct (treesit-haskell-inline-pragma
               (:conc-name treesit-haskell-inline-pragma/))
  ;; String
  (pragma nil :read-only t)
  ;; String
  (function-name nil :read-only t)
  ;; Integer
  (function-name-start nil :read-only t)
  ;; Integer
  (function-name-end nil :read-only t)
  ;; Treesitter node
  (node nil :read-only t))

(defun treesit-haskell-inline-pragma-strictly-inside-node? (pragma node)
  (cl-assert (treesit-haskell-inline-pragma-p pragma))
  (cl-assert (treesit-node-p node))
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node))
         (pragma-node (treesit-haskell-inline-pragma/node pragma))
         (s (treesit-node-start pragma-node))
         (e (treesit-node-end pragma-node)))
    (and (< start s)
         (< s end)
         (< start e)
         (< e end))))

(defun treesit-haskell-inline-pragma= (x y)
  (and (string= (treesit-haskell-inline-pragma/pragma x)
                (treesit-haskell-inline-pragma/pragma y))
       (string= (treesit-haskell-inline-pragma/function-name x)
                (treesit-haskell-inline-pragma/function-name y))
       (eq (treesit-haskell-inline-pragma/function-name-start x)
           (treesit-haskell-inline-pragma/function-name-start y))
       (eq (treesit-haskell-inline-pragma/function-name-end x)
           (treesit-haskell-inline-pragma/function-name-end y))))

(defun treesit-haskell-inline-pragma-name-same-as-span? (pragma node)
  (buffer-span-text-in-current-buffer= node
                                       (treesit-haskell-inline-pragma/function-name-start pragma)
                                       (treesit-haskell-inline-pragma/function-name-end pragma)))

(defun treesit-haskell-parse-inline-pragma (node)
  (cl-assert (treesit-node-p node))
  (when (string= (treesit-node-type node) "pragma")
    (save-excursion
      (goto-char (treesit-node-start node))
      (when (looking-at haskell-regexen/inline-pragmas-complete-pragma)
        (make-treesit-haskell-inline-pragma
         :pragma (match-string-no-properties 1)
         :function-name (match-string-no-properties 2)
         :function-name-start (match-beginning 2)
         :function-name-end (match-end 2)
         :node node)))))

(defun point-inside-string?--ts-haskell (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (treesit-haskell--is-inside-string-node? (or pos (point))
                                           (treesit-haskell--current-node)))

(defun point-inside-comment?--ts-haskell (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (setf pos (or pos (point)))
  (or (point-inside-comment?--default pos)
      (treesit-haskell--is-inside-comment-node? pos
                                                (treesit-haskell--current-node))))

(defun point-inside-string-or-comment?--ts-haskell (&optional pos)
  "Return t if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (setf pos (or pos (point)))
  (or (point-inside-comment?--default pos)
      (treesit-haskell--is-inside-string-or-comment-node? pos
                                                          (treesit-haskell--current-node))))

(defsubst point-not-inside-string-or-comment?--ts-haskell (&optional pos)
  (declare (pure nil) (side-effect-free t))
  (setf pos (or pos (point)))
  (and (not (point-inside-comment?--default pos))
       (treesit-haskell--is-not-inside-string-or-comment-node? pos
                                                               (treesit-haskell--current-node))))

(defun treesit-haskell--is-inside-pragma-node? (p node)
  (declare (pure t) (side-effect-free t))
  (and (treesit-haskell--is-pragma-node-type? (treesit-node-type node))
       (treesit-utils-is-inside-node? p node)))


(provide 'treesit-haskell)

;; Local Variables:
;; End:

;; treesit-haskell.el ends here
