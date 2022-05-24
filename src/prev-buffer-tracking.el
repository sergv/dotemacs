;; prev-buffer-tracking.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 12 December 2021
;; Description:

(defun switch-to-prev-buffer-in-window ()
  "Switch to previous alive buffer for selected window, if there's one."
  (interactive)
  (let* ((win (selected-window))
         (current-buf (window-buffer win))
         (prev-bufs (window-prev-buffers win)))

    (while (let ((buf (caar prev-bufs)))
             (or (not (buffer-live-p buf))
                 (equal buf current-buf)))
      (pop prev-bufs))

    (if-let ((next-buf-entry (car prev-bufs)))
        (progn
          (switch-to-buffer (car next-buf-entry))
          (set-window-start win (cadr next-buf-entry))
          (goto-char (caddr next-buf-entry)))
      (error "no alive previous buffers to switch to"))))

(provide 'prev-buffer-tracking)

;; Local Variables:
;; End:

;; prev-buffer-tracking.el ends here
