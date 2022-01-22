;; vim-undo.el - Undo/Redo for VIM. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Before the execution of an editing command, the calling function
;; should save the current head of buffer-undo-list.  When the
;; editing-command has finished, the calling function should use
;; vim--connect-undos! to connect the changes made during editing to one
;; single undo-block.
;;
;; Insert-mode has a special handling: when activated, it stores the
;; current head of buffer-undo-list in vim--last-insert-undo and used
;; this pointer to connect all editing actions during insert-mode to
;; one undo-block when insert-mode is deactivated.  If a function
;; activates insert-mode it may modify vim--last-insert-undo to an
;; apropriate value (see vim:execute-mapping for an example).

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'vim-macs)
(require 'vim-defs)
(require 'vim-compat)
(require 'vim-modes)

(defvar vim--last-undo nil
  "The last item in the undo list.")

(defun vim--prepare-buffer-undo-list! ()
  "Make sure ‘buffer-undo-list’ is not empty so that ‘vim--connect-undos!’
will always be able to locate tail of the undo list and correctly connect
undos even when there are no changes and the ‘buffer-undo-list’ is empty after
a buffer is opened."
  (when (and (not buffer-undo-list)
             (not (eq buffer-undo-list t)))
    (setq buffer-undo-list (list nil 'undo-tree-canary))))

(defun vim--copy-list-until (lst end)
  (let ((res nil)
        (tmp nil))
    (when (and lst
               (not (eq lst end)))
      (setf res (cons (car lst) nil)
            tmp res
            lst (cdr lst))
      (while (and lst
                  (not (eq lst end)))
        (setf tmp
              (setcdr-sure tmp
                           (cons (car lst) nil))
              lst (cdr lst)))
      res)))

;; undo stuff
(defun vim--connect-undos! (last-undo)
  (unless (eq buffer-undo-list t)

    ;; ensure last-undo is still in the undo list
    (when (and last-undo
               (not (eq last-undo buffer-undo-list))
               (let ((lst buffer-undo-list))
                 (while (and lst
                             (not (eq lst last-undo)))
                   (setq lst (cdr-sure lst)))
                 lst))

      ;; add the end-of-command mark if not already there
      (when (car buffer-undo-list)
        (push nil buffer-undo-list))

      ;; remove all nils until the mark
      (let ((lst buffer-undo-list))
        (while (and lst
                    (not (eq (cdr-sure lst) last-undo)))
          (let ((rest (cdr-sure lst)))
            (if (car-sure rest)
                (setq lst rest)
              ;; Skip over nil.
              (progn
                ;; Do 1 iteration since we’re sure (car rest) is nil.
                (setq rest (cdr-sure rest))
                (while (and rest
                            (not (eq rest last-undo))
                            (null (car-sure rest)))
                  (setq rest (cdr-sure rest)))
                (setcdr-sure lst rest)))))))))

;; (defun vim--extract-undo-prefix! (last-undo)
;;   "Strips away list of undo elements from ‘buffer-undo-list’ just before LAST-UNDO."
;;   (unless (eq buffer-undo-list t)
;;     (let* ((lst buffer-undo-list)
;;            (res lst)
;;            prev)
;;       (while (and lst
;;                   (not (eq lst last-undo)))
;;         (setq prev lst
;;               lst (cdr-sure lst)))
;;       (when prev
;;         (setcdr-sure prev nil))
;;       (setq buffer-undo-list lst)
;;       res)))

;; (defun vim--apply-deltas! (deltas last-undo)
;;   "Destructively undo deltas to the section of ‘buffer-undo-list’ before LAST-UNDO."
;;   (unless (eq buffer-undo-list t)
;;     (let ((lst buffer-undo-list))
;;       (while (and lst
;;                   (not (eq lst last-undo)))
;;         (setcar-sure lst (undo-adjust-elt (car-sure lst) deltas))
;;         (setf lst (cdr-sure lst))))))

;; (defun vim--invert-delta! (x)
;;   (setcar x (- (car x)))
;;   (setcdr x (- (cdr x)))
;;   x)

(provide 'vim-undo)

;; Local Variables:
;; End:

;; vim-undo.el ends here
