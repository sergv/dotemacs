;; trie.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 16 January 2022
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'macro-util))

(defconst trie--unbound '#:unbound)

;; Trie where keys are literal case-sensitive (regardless of ‘case-fold-search’ bullshit) strings.

(defun make-trie-node (value subtrees)
  (cons value subtrees))

(defsubst trie-node-value (x)
  (car x))

(defsetf trie-node-value (x) (value)
  `(setf (car ,x) ,value))

(defsubst trie-node-subtrees (x)
  (cdr x))

(defsetf trie-node-subtrees (x) (value)
  `(setf (cdr ,x) ,value))

(defsubst trie-node-p (x)
  (consp x))

;; NB returns alist entry (<char> . <subnode>), not <subnode>.
(defsubst trie-node--find-subtree (key node)
  (cl-assert (trie-node-p node))
  (assq key (trie-node-subtrees node)))

(defun make-empty-trie ()
  (make-trie-node trie--unbound nil))

(defun trie-insert! (key value trie)
  "Mutate TRIE by binding VALUE to KEY."
  (trie-insert-with! key value trie (lambda (_old new) new)))

(defun trie-insert-with! (key value trie merge)
  "Mutate TRIE by binding VALUE to KEY. If KEY already has a value associated with it
call (MERGE old-val VALUE) to produce a new value."
  (cl-assert (stringp key))

  (let ((node trie))
    (cl-loop
     for c across key
     do
     (let ((subtree-entry (trie-node--find-subtree c node)))
       (aif subtree-entry
           (setf node (cdr it))
         (let ((new-node (make-empty-trie)))
           (setf (trie-node-subtrees node) (cons (cons c new-node)
                                                 (trie-node-subtrees node))
                 node new-node)))))

    (let ((val (trie-node-value node)))
      (setf (trie-node-value node)
            (if (eq val trie--unbound)
                value
              (funcall merge val value))))))

(defsubst trie-lookup-node-char (c trie)
  "Produce subtrie with character ‘C’ being stripped from TRIE."
  (cdr (trie-node--find-subtree c trie)))

(defun trie-lookup (key trie &optional def)
  "Find value that corresponds to KEY in TRIE, retrun DEF if not found."
  (let ((node trie)
        (size (length key))
        (i 0))
    (while (and (< i size)
                node)
      (let ((c (aref key i)))
        (setf node (trie-lookup-node-char c node)
              i (1+ i))))
    (if node
        (let ((val (trie-node-value node)))
          (if (eq val trie--unbound)
              def
            val))
      def)))

(provide 'trie)

;; Local Variables:
;; End:

;; trie.el ends here
