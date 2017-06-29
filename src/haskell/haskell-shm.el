;; haskell-shm.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 12 February 2014
;; Description:

(require 'shm)

(defun haskell-node/topmost-parent-pair ()
  "Get the topmost haskell node for current position."
  (let ((node-pair (shm-current-node-pair))
        (prev-pair nil))
    (while (not (null? node-pair))
      (setq prev-pair node-pair
            node-pair (shm-node-parent node-pair)))
    ;; ! abstraction is broken here...
    prev-pair))

(defun haskell-node/move-to-topmost-start ()
  "Move to start of the topmost node, similar to `glisp/beginning-of-defun'."
  (interactive)
  (goto-char (shm-node-start (cdr (haskell-node/topmost-parent-pair)))))

(defun haskell-node/move-to-topmost-end ()
  "Move to end of the topmost node, similar to `glisp/end-of-defun'."
  (interactive)
  (goto-char (shm-node-end (cdr (haskell-node/topmost-parent-pair)))))


(defun vim/motion-haskell-node/move-n-parents-up (n)
  "Move N - 1 parents up from current node and return resulting node."
  (let ((node-pair (shm-current-node-pair)))
    (assert (< 0 n))
    (dotimes (i (- n 1))
      (setq node-pair (shm-node-parent node-pair)))
    ;; ! abstraction is broken here...
    (cdr-safe node-pair)))

(vim:defmotion vim:motion-inner-haskell-node (inclusive count)
  "Select `count' inner haskell nodes."
  (let ((node (vim/motion-haskell-node/move-n-parents-up (or count 1))))
    (vim:make-motion :has-begin t
                     :begin (shm-node-start node)
                     :end (shm-node-end node)
                     :type 'inclusive)))


(vim:defmotion vim:motion-outer-haskell-node (inclusive count)
  "Select `count' outer haskell nodes."
  (let ((node (vim/motion-haskell-node/move-n-parents-up (or count 1))))
    (vim:make-motion :has-begin t
                     :begin (save-excursion
                              (goto-char (shm-node-start node))
                              (skip-syntax-backward " >")
                              (point))
                     :end (save-excursion
                            (goto-char (shm-node-end node))
                            (skip-syntax-forward " >")
                            (point))
                     :type 'inclusive)))


(vim:defmotion vim:motion-jump-haskell-item (inclusive)
  "Jump over sexps or boundaries of haskell nodes if there's no sexp at point.
Similar to `vim:motion-jump-item'."
  (if (let ((synt (char-syntax (char-after))))
        (or (char= synt ?\()
            (char= synt ?\))))
      (vim:motion-jump-item)
    (let ((node (shm-current-node)))
      (if node
          (let ((start (shm-node-start node))
                (end (shm-node-end node)))
            (cond ((= start (point))
                   (goto-char end))
                  ((= end (point))
                   (goto-char start))
                  (t
                   (shm/forward-node))))
        (error "No current node available")))))

(provide 'haskell-shm)

;; Local Variables:
;; End:

;; haskell-shm.el ends here
