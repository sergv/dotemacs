;;; vim-undo.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

(provide 'vim-undo)


;; undo stuff
(defvar vim:last-undo nil)

(defun vim:connect-undos ()
  (labels
      ((find-mark (lst)
                  (while (or (null lst)
                             (eq lst vim:last-undo))
                    (setq lst (cdr lst)))
                  (not (null lst))))
                   
    ;; ensure vim:last-undo is still in the undo list
    (when (and vim:last-undo
               (not (eq vim:last-undo buffer-undo-list))
               (find-mark buffer-undo-list))
      
      ;; add the end-of-command mark if not already there
      (unless (null (car buffer-undo-list))
        (push nil buffer-undo-list))

      ;; remove all nils until the mark
      (let ((lst buffer-undo-list))
        (while (and lst
                    (not (eq (cdr lst) vim:last-undo)))
          (if (null (cadr lst))
              (setcdr lst (cddr lst))
            (setq lst (cdr lst))))))

    (setq vim:last-undo nil)))


(vim:define vim:cmd-undo (count)
            :type 'simple
            :repeatable nil
  (setq vim:last-undo nil)
  (dotimes (i (or count 1))
    (undo)))
    
(vim:define vim:cmd-redo (count)
            :type 'simple
            :repeatable nil
  (setq vim:last-undo nil)
  (redo (or count 1)))
    
  
  
  
