(provide 'vim-undo)


;; undo stuff
(defvar vim:last-undo nil)

(defun vim:connect-undos ()
  (labels
      ((find-mark (lst)
                  (cond
                   ((null lst) nil)
                   ((eq lst vim:last-undo) t)
                   (t (find-mark (cdr lst))))))
    
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
              
      


(defun vim:cmd-undo (count motion)
  (setq vim:last-undo nil)
  (dotimes (i (or count 1))
    (undo)))
    
(defun vim:cmd-redo (count motion)
  (setq vim:last-undo nil)
  (redo (or count 1)))
    
  
  
  
