;; prev-buffer-tracking.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 12 December 2021
;; Description:

(eval-when-compile
  (require 'macro-util))

;;;; keeping window's previous buffers and switching to them

(defun prev-bufs--add-buf (prev-buf new-buf entries)
  (let ((buf-list (prev-bufs--buffers entries)))
    (cond
      ;; Common case of switching between the current and the provious buffers.
      ((and (equal new-buf (car buf-list))
            (equal prev-buf (cadr buf-list)))
       (cl-rotatef (car buf-list) (cadr buf-list))
       entries)
      (t
       (cons t (cons prev-buf buf-list))))))

(defsubst prev-bufs--has-changes? (x)
  (car x))

(defsubst prev-bufs--buffers (x)
  (cdr x))

(defsetf prev-bufs--buffers (x) (value)
  `(setcdr-sure ,x ,value))

(defun prev-bufs--filter-live-and-dedup! (x)
  (let* ((res (cons nil (prev-bufs--buffers x)))
         (prev res)
         (tmp (cdr prev))
         (buffers (make-hash-table :test #'equal)))

    (while tmp
      (let ((buf (car-sure tmp)))
        (if (or (not (buffer-live-p buf))
                (gethash buf buffers))
            ;; Don’t move ‘prev’ here in order to be able to delete multiple consecutive elements.
            (setf tmp (setcdr-sure prev (cdr-sure tmp)))
          (progn
            (puthash buf t buffers)
            (setf prev (cdr-sure prev)
                  ;; Move to the next thing after prev.
                  tmp (cdr-sure prev))))))

    ;; Reuse cons with nil to signal that there are no changes
    res))

(defun record-previous-buffer-for-window (win new-buf &rest _ignored)
  (let ((prev-buf (window-buffer win)))
    (when (and (not (minibufferp prev-buf))
               (not (minibufferp new-buf)))
      (set-window-parameter win
                            'prev-buffers
                            (prev-bufs--add-buf prev-buf
                                                new-buf
                                                (window-parameter win 'prev-buffers))))))

(advice-add 'set-window-buffer :before #'record-previous-buffer-for-window)

(defun switch-to-prev-buffer-in-window ()
  "Switch to previous alive buffer for selected window, if there's one."
  (interactive)
  (let* ((win (selected-window))
         (current-buf (window-buffer win))
         (entry (window-parameter win 'prev-buffers)))

    (if-let ((next-buf (car (prev-bufs--buffers entry))))
        (progn
          (when (or (prev-bufs--has-changes? entry)
                    (not (buffer-live-p next-buf)))
            (setf entry (prev-bufs--filter-live-and-dedup! entry))
            (set-window-parameter win 'prev-buffers entry))

          (if-let ((next-buf (car (prev-bufs--buffers entry))))
              (progn
                ;; Cannot proceed further if next buffer is the same as the current one. It’s guaranteed
                ;; to occur only once in the list though so we can just take the tail.
                (when (equal next-buf current-buf)
                  (setf (prev-bufs--buffers entry) (cdr (prev-bufs--buffers entry)))
                  (set-window-parameter win 'prev-buffers entry))

                (if-let ((prev-bufs (prev-bufs--buffers entry)))
                    (switch-to-buffer (car prev-bufs))
                  (error "no alive previous buffers to switch to")))
            (error "no alive previous buffers to switch to")))
      (error "no alive previous buffers to switch to"))))

(provide 'prev-buffer-tracking)

;; Local Variables:
;; End:

;; prev-buffer-tracking.el ends here
