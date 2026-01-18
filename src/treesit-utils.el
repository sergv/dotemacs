;; treesit-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 June 2024
;; Description:

(defsubst treesit-haskell--current-node ()
  (treesit-haskell--node-at (point)))

(defun treesit-haskell--node-at (pos)
  (when (derived-mode-p 'haskell-ts-base-mode)
    (treesit-node-at pos
                     ;; Hoping the parser will get reused, should be safe for
                     ;; haskell-ts-mode and its derivatives.
                     (treesit-parser-create 'haskell))))

(defun treesit-haskell--is-multiline-string? (node)
  (declare (pure nil) (side-effect-free t))
  (cl-assert (treesit-node-p node))
  (and (string= "string" (treesit-node-type node))
       (text-after-pos-matches? (treesit-node-start node) "\"\"\"")
       (text-before-pos-matches? (treesit-node-end node) "\"\"\"")))

(defun treesit-haskell--is-inside-node? (p node)
  (declare (pure t) (side-effect-free t))
  (cl-assert (integerp p))
  (cl-assert (treesit-node-p node))
  (and (<= (treesit-node-start node) p)
       (<= p (treesit-node-end node))))

(defun treesit-haskell--is-inside-node-ex-start? (p node)
  (declare (pure t) (side-effect-free t))
  (cl-assert (integerp p))
  (cl-assert (treesit-node-p node))
  (and (< (treesit-node-start node) p)
       (<= p (treesit-node-end node))))

(defun treesit-haskell--is-comment-node-type? (typ)
  (declare (pure t) (side-effect-free t))
  (cl-assert (stringp typ))
  (or (string= typ "comment")
      (string= typ "haddock")
      (string= typ "pragma")))

(defun treesit-haskell--is-string-node-type? (typ)
  (declare (pure t) (side-effect-free t))
  (cl-assert (stringp typ))
  (or (string= typ "char")
      (string= typ "string")
      (string= typ "quasiquote_body")))

(defun treesit-haskell--is-inside-string-node? (p node)
  (declare (pure t) (side-effect-free t))
  (and (treesit-haskell--is-inside-node-ex-start? p node)
       (treesit-haskell--is-string-node-type? (treesit-node-type node))))

(defun treesit-haskell--is-inside-comment-node? (p node)
  (declare (pure t) (side-effect-free t))
  (and (treesit-haskell--is-inside-node-ex-start? p node)
       (treesit-haskell--is-comment-node-type? (treesit-node-type node))))

(defun treesit-haskell--is-inside-string-or-comment-node? (p node)
  (declare (pure t) (side-effect-free t))
  (let ((typ (treesit-node-type node)))
    (and (treesit-haskell--is-inside-node-ex-start? p node)
         (or (treesit-haskell--is-comment-node-type? typ)
             (treesit-haskell--is-string-node-type? typ)))))

(defun treesit-haskell--is-not-inside-string-or-comment-node? (p node)
  (declare (pure t) (side-effect-free t))
  (let ((typ (treesit-node-type node)))
    (not (and (treesit-haskell--is-inside-node-ex-start? p node)
              (or (treesit-haskell--is-comment-node-type? typ)
                  (treesit-haskell--is-string-node-type? typ))))))

(defun treesit-haskell-get-buffer-module-name ()
  (if-let* ((header-candidates (treesit-filter-child
                                (treesit-buffer-root-node 'haskell)
                                (lambda (node)
                                  (string= "header" (treesit-node-type node))))))
      (treesit-node-text-no-properties-unsafe (treesit-node-child-by-field-name (car header-candidates) "module"))
    "Main"))

(defun point-inside-string?--ts-haskell (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (treesit-haskell--is-inside-string-node? (or pos (point)) (treesit-haskell--current-node)))

(defun point-inside-comment?--ts-haskell (&optional pos)
  "Return non-nil if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (treesit-haskell--is-inside-comment-node? (or pos (point)) (treesit-haskell--current-node)))

(defun point-inside-string-or-comment?--ts-haskell (&optional pos)
  "Return t if point is positioned inside a string."
  (declare (pure nil) (side-effect-free t))
  (treesit-haskell--is-inside-string-or-comment-node? (or pos (point)) (treesit-haskell--current-node)))

(defsubst point-not-inside-string-or-comment?--ts-haskell (&optional pos)
  (declare (pure nil) (side-effect-free t))
  (treesit-haskell--is-not-inside-string-or-comment-node? (or pos (point)) (treesit-haskell--current-node)))

(defun treesit-haskell--is-inside-pragma-node? (p node)
  (declare (pure t) (side-effect-free t))
  (and (string= "pragma" (treesit-node-type node))
       (treesit-haskell--is-inside-node? p node)))

(cl-defstruct treesit-computed-indent
  (anchor-node :read-only t) ;; treesit node
  (flags       :read-only t) ;; list of symbols
  )

(defun treesit-matched-anchor-node-type (x)
  (cl-assert (or (treesit-node-p x)
                 (treesit-computed-indent-p x)))
  (let ((node
         (if (treesit-computed-indent-p x)
             (treesit-computed-indent-anchor-node x)
           x)))
    (treesit-node-type node)))

(defmacro* treesit-with-evaluated-anchor-and-offset
    ((evaluated-anchor-pos-var anchor)
     (evaluated-offset-num-var offset)
     &rest body)
  (declare (indent 2))
  (let ((anchor-var '#:anchor)
        (offset-var '#:offset))
    `(let ((,anchor-var ,anchor)
           (,offset-var ,offset))
       (let ((anchor-pos (cond
                           ((treesit-computed-indent-p ,anchor-var)
                            (treesit-node-start (treesit-computed-indent-anchor-node ,anchor-var)))
                           ((treesit-node-p ,anchor-var)
                            (treesit-node-start ,anchor-var))
                           ((number-or-marker-p ,anchor-var)
                            ,anchor-var)
                           ((null ,anchor-var)
                            nil)
                           (t
                            (error "Unexpected anchor: ‘%s’" ,anchor-var))))
             (offset-num (cond
                           ((functionp ,offset-var)
                            (funcall ,offset-var ,anchor-var))
                           ((numberp ,offset-var)
                            ,offset-var)
                           (t
                            (error "Unexpected offset: ‘%s’" ,offset-var)))))
         ,@body))))

(defun treesit-utils-find-topmost-parent (node pred)
  (cl-assert (treesit-node-p node))
  (let ((result nil)
        (p node))
    (while p
      (when (funcall pred p)
        (setf result p))
      (setf p (treesit-node-parent p)))
    result))

(defun treesit-utils-find-topmost-parent-limited (node pred limit)
  (cl-assert (treesit-node-p node))
  (let ((result nil)
        (p node))
    (while (and p
                (> limit 0))
      (when (funcall pred p)
        (setf result p))
      (setf p (treesit-node-parent p)
            limit (- limit 1)))
    result))

(defun treesit-utils-find-topmost-parent-stop-at-first (node pred)
  (cl-assert (treesit-node-p node))
  (let ((result nil)
        (p node)
        (continue? t))
    (while (and continue? p)
      (when (funcall pred p)
        (setf result p
              continue? nil))
      (setf p (treesit-node-parent p)))
    result))

(defun treesit-utils-find-closest-parent-limited (node pred limit)
  (cl-assert (treesit-node-p node))
  (let ((result nil)
        (p node)
        (continue? t))
    (while (and continue? (< 0 limit) p)
      (when (funcall pred p)
        (setf result p
              continue? nil))
      (setf p (treesit-node-parent p)
            limit (- limit 1)))
    result))

(defun treesit-utils-find-topmost-parent-stop-at-first-with-count (node pred)
  (cl-assert (treesit-node-p node))
  (let ((result nil)
        (p node)
        (continue? t)
        (n 0))
    (while (and continue? p)
      (when (funcall pred p)
        (setf result p
              continue? nil))
      (setf p (treesit-node-parent p)
            n (+ n 1)))
    (cons result n)))

(defun treesit-utils-largest-node-starting-at (p &optional lang)
  (let* ((node (treesit-node-at p lang))
         (start (treesit-node-start node))
         (tmp (treesit-node-parent node)))
    (while (eq (treesit-node-start tmp) start)
      (setf node tmp
            tmp (treesit-node-parent tmp)))
    node))

(defun treesit-utils--get-ast-node-soup (node &optional intern-fields?)
  "Extract structure of ast nodes from NODE as nested lists.

INTERN-FIELDS? is useful for debug but otherwise will only lead to extra
overhead if produced structures will only be compared once."
  (cl-assert (treesit-node-p node))
  (let ((children-count (treesit-node-child-count node)))
    (if (zerop children-count)
        (treesit-node-type node)
      (cons
       (treesit-node-type node)
       (cl-loop
        for i from 0 below children-count
        append
        (if-let* ((field-name (treesit-node-field-name-for-child node i)))
            (list
             (if intern-fields?
                 (string->symbol (concat ":" field-name))
               field-name)
             (treesit-utils--get-ast-node-soup (treesit-node-child node i)))
          (list
           (treesit-utils--get-ast-node-soup (treesit-node-child node i)))))))))

(provide 'treesit-utils)

;; Local Variables:
;; End:

;; treesit-utils.el ends here
