;; prev-buffer-tracking.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 12 December 2021
;; Description:

;;;; keeping window's previous buffers and switching to them

(defun record-previous-buffer-for-window (win new-buf &rest _ignored)
  (let* ((prev-buf (window-buffer win))
         (entries (cons prev-buf
                        (-filter #'buffer-live-p
                                 (window-parameter win 'prev-buffers)))))
    (set-window-parameter win 'prev-buffers entries)))

;; (add-function 'set-window-buffer :before #'record-previous-buffer-for-window)
(advice-add 'set-window-buffer :before #'record-previous-buffer-for-window)

(defun switch-to-prev-buffer-in-window ()
  "Switch to previous alive buffer for selected window, if there's one."
  (interactive)
  (let* ((win (selected-window))
         (prev-bufs (-filter #'buffer-live-p
                             (window-parameter win 'prev-buffers))))
    (if (null? prev-bufs)
        (error "no alive previous buffers to switch to")
      (switch-to-buffer (car prev-bufs)))))

(provide 'prev-buffer-tracking)

;; Local Variables:
;; End:

;; prev-buffer-tracking.el ends here
