;; buffer-span.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 April 2026
;; Description:

(eval-when-compile
  (require 'cl))

;; Pair of positions in a buffer. Interchangeable with treesit nodes.
(cl-defstruct (buffer-span
               (:conc-name buffer-span/)
               (:constructor make-buffer-span--impl))
  (start nil :read-only t)
  (end   nil :read-only t))

(defun make-buffer-span (start end)
  (cl-assert (numberp start))
  (cl-assert (numberp end))
  (cl-assert (<= start end))
  (make-buffer-span--impl :start start :end end))

(defun buffer-span-from-treesit-node (node)
  (make-buffer-span (treesit-node-start node) (treesit-node-end node)))

(defun buffer-span-start (x)
  (if (treesit-node-p x)
      (treesit-node-start x)
    (buffer-span/start x)))

(defun buffer-span-end (x)
  (if (treesit-node-p x)
      (treesit-node-end x)
    (buffer-span/end x)))

(defun buffer-span-text-no-properties (x)
  (buffer-substring-no-properties (buffer-span-start x) (buffer-span-end x)))

(defun buffer-span-texts-in-current-buffer= (x y)
  (cl-assert (or (buffer-span-p y) (treesit-node-p y)))
  (cl-assert (if (treesit-node-p y)
                 (eq (treesit-node-buffer y) (current-buffer))
               t))
  (buffer-span-text-in-current-buffer=
   x
   (buffer-span-start y)
   (buffer-span-end y)))

(defun buffer-span-text-in-current-buffer= (span start end)
  "Check that NODE’s text is the same as text in buffer between START and END."
  (cl-assert (or (buffer-span-p span) (treesit-node-p span)))
  (cl-assert (if (treesit-node-p span)
                 (eq (treesit-node-buffer span) (current-buffer))
               t))
  (let ((start2 (buffer-span-start span))
        (end2 (buffer-span-end span)))
    (or (and (eq start start2)
             (eq end end2))
        (let ((case-fold-search nil))
          (zerop
           (compare-buffer-substrings nil
                                      start2
                                      end2
                                      nil
                                      start
                                      end))))))

(provide 'buffer-span)

;; Local Variables:
;; End:

;; buffer-span.el ends here
