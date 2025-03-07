;; treesit-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 June 2024
;; Description:

(defsubst treesit-haskell--current-node ()
  (treesit-haskell--node-at (point)))

(defun treesit-haskell--node-at (pos)
  (when (derived-mode-p 'haskell-ts-mode)
    (treesit-node-at pos
                     ;; Hoping the parser will get reused, should be safe for
                     ;; haskell-ts-mode and its derivatives.
                     (treesit-parser-create 'haskell))))

(defun treesit-haskell--is-multiline-string? (node)
  (cl-assert (treesit-node-p node))
  (and (string= "string" (treesit-node-type node))
       (text-after-pos-matches? (treesit-node-start node) "\"\"\"")
       (text-before-pos-matches? (treesit-node-end node) "\"\"\"")))

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

(defun treesit-utils-find-topmost-parent-stop-at-first (node pred)
  (let ((result nil)
        (p node)
        (continue? t))
    (while (and continue? p)
      (when (funcall pred p)
        (setf result p
              continue? nil))
      (setf p (treesit-node-parent p)))
    result))

(defun treesit-utils-largest-node-starting-at (p)
  (let* ((node (treesit-node-at p))
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
