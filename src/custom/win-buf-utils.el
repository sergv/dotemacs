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
  (let* ((curr-win (selected-window))
         (curr-buffer (current-buffer))
         (curr-pos (with-selected-window curr-win
                     (with-current-buffer curr-buffer
                       (point))))
         (next-win (next-window curr-win 0))
         (next-buffer (save-selected-window (select-window next-win t)
                                            (current-buffer)))
         (next-pos (with-selected-window next-win
                     (with-current-buffer next-buffer
                       (point)))))
    (if (eq? next-buffer curr-buffer)
      (begin
        (with-selected-window curr-win
          (with-current-buffer curr-buffer
            (goto-char next-pos)))
        (select-window next-win)
        (with-selected-window next-win
          (with-current-buffer next-buffer
            (goto-char curr-pos))))
      (begin
        (switch-to-buffer next-buffer)
        (select-window next-win)
        (switch-to-buffer curr-buffer)))))

(defun swap-buffers-backward ()
  "Swap current buffer with previous buffer"
  (interactive)
  (let* ((curr-win (selected-window))
         (curr-buffer (current-buffer))
         (curr-pos (with-selected-window curr-win
                     (with-current-buffer curr-buffer
                       (point))))
         (prev-win (previous-window curr-win 0))
         (prev-buffer (save-selected-window (select-window prev-win t)
                                            (current-buffer)))
         (prev-pos (with-selected-window prev-win
                     (with-current-buffer prev-buffer
                       (point)))))
    (if (eq? prev-buffer curr-buffer)
      (begin
        (with-selected-window curr-win
          (with-current-buffer curr-buffer
            (goto-char prev-pos)))
        (select-window prev-win)
        (with-selected-window prev-win
          (with-current-buffer prev-buffer
            (goto-char curr-pos))))
      (begin
        (switch-to-buffer prev-buffer)
        (select-window prev-win)
        (switch-to-buffer curr-buffer)))))


;; Local Variables:
;; End:

;; win-buf-utils.el ends here
