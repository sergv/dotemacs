;;; comint-setup.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 January 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-after-load
 "shell"
 '(progn
   ;; this calls `comint-write-input-ring' from the repl buffer so that it
   ;; will see correct value of `comint-write-input-ring' which is permamently
   ;; local everywhere
   (redefun shell-write-history-on-exit (process event)
     "Called when the shell process is stopped.

Writes the input history to a history file
`comint-input-ring-file-name' using `comint-write-input-ring'
from the right buffer and inserts a short message in the shell buffer.

This function is a sentinel watching the shell interpreter process.
Sentinels will always get the two parameters PROCESS and EVENT."
     (let ((buf (process-buffer process)))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           ;; Write history.
           (comint-write-input-ring)
           (insert (format "\nProcess %s %s\n" process event))))))))


(defun comint-setup ()
  (def-keys-for-map comint-mode-map
    ("<up>"   comint-previous-input)
    ("<down>" comint-next-input)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)))


(defun comint-clear-prompt () ;; shell-clear-prompt
  "Clear shell prompt from input."
  (interactive)
  (comint-bol)
  (when (not (equal (point)
                    (line-end-position)))
    (delete-region (point) (line-end-position))))

(defun comint-clear-buffer-above-prompt () ;; shell-clear-buffer
  "Clear everything between start of buffer and line above current one
inclusively."
  (interactive)
  (save-excursion
   (let ((inhibit-read-only t))
     (forward-line -1)
     (delete-region (point-min) (line-end-position))
     (delete-char 1))))


(add-hook 'comint-mode-hook #'comint-setup)

(provide 'comint-setup)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; comint-setup.el ends here
