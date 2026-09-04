;; treesit-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 June 2024
;; Description:

(eval-when-compile
  (require 'cl-lib))

(declare-function treesit-buffer-root-node "treesit")
(declare-function treesit-filter-child "treesit")
(declare-function treesit-node-at "treesit")

(require 'buffer-span)
(require 'common)
(require 'common-small)
(require 'semnav)

(defun treesit-utils-is-inside-node? (p node)
  (declare (pure t) (side-effect-free t))
  (cl-assert (integerp p))
  (cl-assert (treesit-node-p node))
  (and (<= (treesit-node-start node) p)
       (<= p (treesit-node-end node))))

(defun treesit-utils--is-inside-node-ex-start? (p node)
  (declare (pure t) (side-effect-free t))
  (cl-assert (integerp p))
  (cl-assert (treesit-node-p node))
  (and (< (treesit-node-start node) p)
       (<= p (treesit-node-end node))))

(defun treesit-utils--do-resolve (f pos node)
  "Turn predicate F on treesit nodes that may return booleans into function
that always returns either nil (if the predicate fails) or treesit
node (if pthe redicate succeeds)."
  (cl-assert (functionp f))
  (cl-assert (fixnump pos))
  (cl-assert (treesit-node-p node))
  (let ((res (funcall f pos node)))
    (if (eq res t)
        node
      (progn
        (cl-assert (or (null res) (treesit-node-p res)))
        res))))

(defun treesit-utils-is-inside-string-node? (p node resolve-string)
  "RESOLVE-STRING function that takes treesitter node and returns either nil if it’s not
a string node or new treesitter node that is a string node (e.g. a parent of the passed node
that should be considered the string node we’re dealing with instead of the original node
which may have been a string delimiter that’s too narrow for our purposes).

Alternatively, RESOLVE-STRING may return t if passed node is the string node to go with,
in which case the passed node will be used further."
  (declare (pure t) (side-effect-free t))
  (and-let* ((resolved (treesit-utils--do-resolve resolve-string p node)))
    (treesit-utils--is-inside-node-ex-start? p resolved)))

(defun treesit-utils-is-inside-comment-node? (p node resolve-comment)
  "See ‘treesit-utils-is-inside-string-node?’ for meaning of RESOLVE-COMMENT."
  (declare (pure t) (side-effect-free t))
  (and-let* ((resolved (treesit-utils--do-resolve resolve-comment p node)))
    (treesit-utils--is-inside-node-ex-start? p resolved)))

(defun treesit-utils-is-inside-string-or-comment-node? (p node resolve-string resolve-comment)
  "See ‘treesit-utils-is-inside-string-node?’ for meaning of RESOLVE-STRING and RESOLVE-COMMENT."
  (declare (pure t) (side-effect-free t))
  (and-let* ((resolved (or (treesit-utils--do-resolve resolve-string p node)
                           (treesit-utils--do-resolve resolve-comment p node))))
    (treesit-utils--is-inside-node-ex-start? p resolved)))

(defun treesit-utils-is-not-inside-string-or-comment-node? (p node resolve-string resolve-comment)
  "See ‘treesit-utils-is-inside-string-node?’ for meaning of RESOLVE-STRING and RESOLVE-COMMENT."
  (declare (pure t) (side-effect-free t))
  (not
   (and-let* ((resolved (or (treesit-utils--do-resolve resolve-string p node)
                            (treesit-utils--do-resolve resolve-comment p node))))
     (treesit-utils--is-inside-node-ex-start? p resolved))))

(defun treesit-utils-semnav-bounds-of-string-at (node is-string-node?)
  "IS-STRING-NODE? - function that takes a treesitter node and shold return non-nil if it’s
as string node."
  (declare (pure nil) (side-effect-free t))
  (when-let* ((node (treesit-utils--string-at node is-string-node?)))
    (cons (treesit-node-start node) (treesit-node-end node))))

;;;###autoload
(defun treesit-utils--string-at (node string-node-type-pred)
  (declare (pure nil) (side-effect-free t))
  (treesit-utils-find-closest-parent
   node
   (lambda (n)
     (funcall string-node-type-pred (treesit-node-type n)))))

(cl-defstruct treesit-computed-indent
  ;; treesit node
  (anchor-node nil :read-only t)
  ;; list of symbols
  flags)

(defun treesit-matched-anchor-node-type (x)
  (cl-assert (or (treesit-node-p x)
                 (treesit-computed-indent-p x)))
  (let ((node
         (if (treesit-computed-indent-p x)
             (treesit-computed-indent-anchor-node x)
           x)))
    (treesit-node-type node)))

(cl-defmacro treesit-with-evaluated-anchor-and-offset
    ((evaluated-anchor-pos-var anchor)
     (evaluated-offset-num-var offset)
     &rest body)
  (declare (indent 2))
  (cl-assert (symbolp evaluated-anchor-pos-var))
  (cl-assert (symbolp evaluated-offset-num-var))
  (let ((anchor-var '#:anchor)
        (offset-var '#:offset))
    `(let ((,anchor-var ,anchor)
           (,offset-var ,offset))
       (let ((,evaluated-anchor-pos-var
              (cond
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
             (,evaluated-offset-num-var
              (cond
                ((functionp ,offset-var)
                 (funcall ,offset-var ,anchor-var))
                ((numberp ,offset-var)
                 ,offset-var)
                (t
                 (error "Unexpected offset: ‘%s’" ,offset-var)))))
         ,@body))))

(defun treesit-utils-find-topmost-parent (node pred)
  "Find highest parent of NODE that satisfies single-argument predicate PRED.

All parents of the returned node don’t satisfy PRED (if they exist)."
  (cl-assert (treesit-node-p node))
  (let ((result nil)
        (p node))
    (while p
      (when (funcall pred p)
        (setf result p))
      (setf p (treesit-node-parent p)))
    result))

(defun treesit-utils-find-topmost-parent-limited (node pred limit)
  "Like ‘treesit-utils-find-topmost-parent’ but stops after LIMIT number of iterations."
  (cl-assert (treesit-node-p node))
  (let ((result nil)
        (p node))
    (while (and p
                (< 0 limit))
      (when (funcall pred p)
        (setf result p))
      (setf p (treesit-node-parent p)
            limit (- limit 1)))
    result))

(defun treesit-utils-find-closest-parent (node pred)
  "Find first parent of NODE that satisfies single-argument predicate PRED."
  (cl-assert (or (treesit-node-p node) (null node)))
  (let ((result nil)
        (p node)
        (continue? t))
    (while (and continue? p)
      (when (funcall pred p)
        (setf result p
              continue? nil))
      (setf p (treesit-node-parent p)))
    result))

(defun treesit-utils-find-closest-parent-until (node pred stop-pred)
  "Find first parent of NODE that satisfies single-argument predicate PRED stopping once STOP-PRED
returns non-nil on the node to be processed."
  (cl-assert (or (treesit-node-p node) (null node)))
  (let ((result nil)
        (p node)
        (continue? t))
    (while (and continue? p)
      (if (funcall pred p)
        (setf result p
              continue? nil)
        (setf p (treesit-node-parent p)
              continue? (not (funcall stop-pred p)))))
    result))

(defun treesit-utils-find-closest-parent-limited (node pred limit)
  "Like ‘treesit-utils-find-closest-parent’ but stops after LIMIT number of iterations."
  (cl-assert (or (treesit-node-p node) (null node)))
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

(defun treesit-utils-find-closest-parent-with-count (node pred)
  "Like ‘treesit-utils-find-closest-parent’ but returns cons pair of found parent, if any,
and its height, i.e. number of jumps performed."
  (cl-assert (or (treesit-node-p node) (null node)))
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
  "Find biggest node that starts as position P, if it exists. All parents of the returned
node don’t start at P."
  (let* ((node (treesit-node-at p lang))
         (start (treesit-node-start node))
         (tmp (treesit-node-parent node)))
    (while (and (eq (treesit-node-start tmp) start)
                ;; "declarations" is usually not what we’re interested in.
                (not (string= (treesit-node-type tmp) "declarations")))
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

(defun treesit-utils-is-standalone-node? (node)
  (save-excursion
    (let ((start (treesit-node-start node)))
      (goto-char start)
      (skip-chars-backward " \t")
      (eq (point) (line-beginning-position)))))

(defmacro treesit-utils--with-parsed-string (str language node-var &rest body)
  "Better alternative to ‘treesit-parse-string’ that cleans up temporary
buffer after itself but imposes constraint that after BODY finishes no
references to parsed treesitter modes shall remain."
  (declare (indent 3))
  (cl-assert (symbolp node-var))
  `(with-temp-buffer
     (insert ,str)
     (let ((,node-var (treesit-parser-root-node (treesit-parser-create ,language))))
       ,@body)))

(defun treesit-node-text-no-properties-unsafe (node &optional str)
  (cl-assert (not (null node)))
  (cl-assert (treesit-node-p node))
  (if (stringp str)
      (substring-no-properties str (1- (treesit-node-start node)) (1- (treesit-node-end node)))
    (progn
      (cl-assert (or (eq (current-buffer) (treesit-node-buffer node))
                     (eq (buffer-base-buffer (current-buffer)) (treesit-node-buffer node))))
      (buffer-substring-no-properties (treesit-node-start node) (treesit-node-end node)))))

(defsubst treesit-utils--is-leaf-node? (node)
  (zerop (treesit-node-child-count node)))

;; (defun treesit-utils-node-texts-in-current-buffer= (x y)
;;   (cl-assert (treesit-node-p y))
;;   (cl-assert (eq (treesit-node-buffer y) (current-buffer)))
;;   (treesit-utils-node-text-in-current-buffer=
;;    x
;;    (treesit-node-start y)
;;    (treesit-node-end y)))
;;
;; (defun treesit-utils-node-text-in-current-buffer= (node start end)
;;   "Check that NODE’s text is the same as text in buffer between START and END."
;;   (cl-assert (treesit-node-p node))
;;   (cl-assert (eq (treesit-node-buffer node) (current-buffer)))
;;   (let ((case-fold-search nil))
;;     (compare-buffer-substrings nil
;;                                (treesit-node-start node)
;;                                (treesit-node-end node)
;;                                nil
;;                                start
;;                                end)))

(provide 'treesit-utils)

;; Local Variables:
;; End:

;; treesit-utils.el ends here
