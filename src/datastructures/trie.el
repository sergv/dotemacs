;; trie.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 16 January 2022
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

;; Really, don’t create this symbol yourself, only reference it via
;; ‘trie--unbound’. Too bad reader doesn’t know about this rule...
(defconst trie--unbound 'trie--unbound)

;; Trie where keys are literal case-sensitive (regardless of ‘case-fold-search’ bullshit) strings.

(defun make-trie-node (value subtrees)
  (cons value subtrees))

(defsubst trie-node--value (x)
  (car x))

(defsetf trie-node--value (x) (value)
  `(setf (car ,x) ,value))

(defsubst trie-node--subtrees (x)
  (cdr x))

(defsetf trie-node--subtrees (x) (value)
  `(setf (cdr ,x) ,value))

(defsubst trie-node-p (x)
  (consp x))

;; NB returns alist entry (<char> . <subnode>), not <subnode>.
(defsubst trie-node--find-subtree (key node)
  (cl-assert (trie-node-p node))
  (assq key (trie-node--subtrees node)))

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
    (dovector (c key)
      (let ((subtree-entry (trie-node--find-subtree c node)))
        (aif subtree-entry
            (setf node (cdr it))
          (let ((new-node (make-empty-trie)))
            (setf (trie-node--subtrees node) (cons (cons c new-node)
                                                   (trie-node--subtrees node))
                  node new-node)))))

    (let ((val (trie-node--value node)))
      (setf (trie-node--value node)
            (if (eq val trie--unbound)
                value
              (funcall merge val value))))))

(defsubst trie-node-value-get (trie &optional def)
  (let ((val (trie-node--value trie)))
    (if (eq val trie--unbound)
        def
      val)))

(defsubst trie-lookup-node-char (c trie)
  "Produce subtrie with character C being stripped from TRIE as a prefix."
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
        (let ((val (trie-node--value node)))
          (if (eq val trie--unbound)
              def
            val))
      def)))

(defun trie-from-list (entries)
  (let ((tr (make-empty-trie)))
    (dolist (entry entries)
      (cl-assert (consp entry))
      (trie-insert! (car entry) (cdr entry) tr))
    tr))

;;;; Optimization

(defvar trie-opt--global-cache nil)

(defun trie-opt-normalize-subtrees! (trie)
  (dolist (subtree (trie-node--subtrees trie))
    (trie-opt-normalize-subtrees! (cdr subtree)))

  (setf (trie-node--subtrees trie)
        (sort (trie-node--subtrees trie)
              (lambda (x y) (< (car x) (car y))))))

(defun trie-opt-recover-sharing! (trie)
  (let ((cache (or trie-opt--global-cache
                   (make-hash-table :test #'equal))))
    (trie-opt-normalize-subtrees! trie)
    (trie-opt--recover-sharing-worker trie cache)))

(defun trie-opt--recover-sharing-worker (trie cache)
  (aif (gethash trie cache)
      it
    (let* ((value (trie-node--value trie))
           (subtrees (trie-node--subtrees trie))
           (shared-val (aif (gethash value cache)
                           it
                         (puthash value value cache)))
           (shared-subtrees
            ;; (trie-opt--recover-sharing-worker--subtrees subtrees cache)
            (trie-opt--recover-sharing-worker--subtrees
             (--map (cons (car it)
                          (trie-opt--recover-sharing-worker (cdr it) cache))
                    (trie-node--subtrees trie))
             cache))
           (shared (make-trie-node shared-val shared-subtrees)))
      (puthash trie shared cache)
      shared)))

(defun trie-opt--recover-sharing-worker--subtrees (subtrees cache)
  (aif (gethash subtrees cache)
      it
    (when subtrees
      (let* ((head (car subtrees))
             (rest (cdr subtrees))
             (cached-car (aif (gethash head cache)
                             it
                           (puthash head head cache)))
             (cached-cdr (aif (gethash rest cache)
                             it
                           (puthash rest
                                    (trie-opt--recover-sharing-worker--subtrees rest cache)
                                    cache)))
             (res (cons cached-car cached-cdr)))
        (puthash subtrees res cache)
        res))))

;;;; Matching

(defun trie-matches-backwards? (tr &optional def)
  (let ((continue t)
        (res def))
    (while (and tr
                continue)
      (if-let ((c (char-before)))
          (progn
            (setf tr (trie-lookup-node-char c tr))
            (forward-char -1)
            (let ((val (trie-node--value tr)))
              (unless (eq val trie--unbound)
                (setf continue nil
                      res val))))
        ;; End of buffer
        (setf continue nil)))
    res))

;;;; End

(provide 'trie)

;; Local Variables:
;; End:

;; trie.el ends here
