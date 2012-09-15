;; win-buf-utils.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: long ago
;; Description:


(defun next-buffer (n)
  "Go to the buffer which is at the end of buffer list."
  (interactive "p")
  (dotimes (i n)
    (unbury-buffer)))

(defun prev-buffer (n)
  "Go to the buffer which is at the top of buffer list behind
the current buffer."
  (interactive "p")
  (dotimes (i n)
    (bury-buffer (current-buffer))
    (switch-to-buffer (other-buffer (current-buffer))))) ;dont forget about 0 here (??)


(defsubst next-w (n)
  "Go to next Nth window"
  (interactive "p")
  (other-window n))

(defsubst prev-w (n)
  "Go to previous Nth window"
  (interactive "p")
  (other-window (- n)))

(defun swap-buffers-forward ()
  "Swap current buffer with next buffer"
  (interactive)
  (let* ((current-win (selected-window))
         (current-buf (current-buffer))
         (next-win (next-window current-win 0))
         (next-buffer
           (save-selected-window
            (select-window next-win t)
            (current-buffer))))
    (switch-to-buffer next-buffer)
    (select-window next-win)
    (switch-to-buffer current-buf)))

(defun swap-buffers-backward ()
  "Swap current buffer with previous buffer"
  (interactive)
  (let* ((current-win (selected-window))
         (current-buf (current-buffer))
         (prev-win (previous-window current-win 0))
         (prev-buffer
           (save-selected-window
            (select-window prev-win t)
            (current-buffer))))
    (switch-to-buffer prev-buffer)
    (select-window prev-win)
    (switch-to-buffer current-buf)))


;; Local Variables:
;; End:

;; win-buf-utils.el ends here
