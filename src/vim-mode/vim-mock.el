;; vim-mock.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  1 January 2012
;; Description:


(defun vim-mock:motion-go-to-first-non-blank-beg (&optional count)
  (interactive "p")
  (vim:save-position)
  (if count
      (goto-line1 count)
    (goto-char (point-min)))
  (skip-to-indentation))

(defun vim-mock:motion-go-to-first-non-blank-end ()
  (interactive)
  (vim:save-position)
  (goto-char (max
              (1- (point-max))
              (point-min)))
  (skip-to-indentation))


(defun vim-mock:scroll-line-to-center ()
  "Scroll selected line to the center of the window."
  (interactive)
  (recenter nil))

(defun vim-mock:scroll-line-to-top ()
  "Scroll selected line to the top of the window."
  (interactive)
  (recenter 0))

(defun vim-mock:scroll-line-to-bottom ()
  "Scroll selected line to the bottom of the window."
  (interactive)
  (recenter -1))

(defun vim-mock:motion-fwd-word-end (count)
  (interactive "p")
  (vim:move-fwd-end (or count 1) #'vim:boundary-word))

(defun vim-mock:motion-fwd-WORD-end (count)
  (interactive "p")
  (vim:move-fwd-end (or count 1) #'vim:boundary-WORD))

(defun vim-mock:motion-bwd-word-end (count)
  (interactive "p")
  (vim:move-bwd-end (or count 1) #'vim:boundary-word))

(defun vim-mock:motion-bwd-WORD-end (count)
  (interactive "p")
  (vim:move-bwd-end (or count 1) #'vim:boundary-WORD))

(defun vim-mock:motion-fwd-word (count)
  "Moves the cursor beginning of the next word."
  (interactive "p")
  (let ((line (line-number-at-pos (point))))
    (vim:move-fwd-beg (or count 1) #'vim:boundary-word)

    ;; in operator-pending mode, if we reached the beginning of a new
    ;; line, go back to the end of the previous line
    (when (and (vim:operator-pending-mode-p)
               (< line (line-number-at-pos (point))) ; only if we skipped a newline
               (vim:looking-back "^[ \t]*")
               (not (save-excursion
                      (forward-visible-line -1)
                      (and (bolp) (eolp)))))
      (forward-visible-line -1)
      (end-of-line))))

(defun vim-mock:motion-bwd-word (count)
  "Moves the cursor beginning of the previous word."
  (interactive "p")
  (vim:move-bwd-beg (or count 1) #'vim:boundary-word))

(defun vim-mock:motion-fwd-WORD (count)
  "Moves the cursor to beginning of the next WORD."
  (interactive "p")
  (let ((line (line-number-at-pos (point))))
    (vim:move-fwd-beg (or count 1) #'vim:boundary-WORD)

    ;; in operator-pending mode, if we reached the beginning of a new
    ;; line, go back to the end of the previous line
    (when (and (vim:operator-pending-mode-p)
               (< line (line-number-at-pos (point))) ; only if we skipped a newline
               (vim:looking-back "^[ \t]*")
               (not (save-excursion
                      (forward-visible-line -1)
                      (and (bolp) (eolp)))))
      (forward-visible-line -1)
      (end-of-line))))

(defun vim-mock:motion-bwd-WORD (count)
  "Moves the cursor to beginning of the previous WORD."
  (interactive "p")
  (vim:move-bwd-beg (or count 1) #'vim:boundary-WORD))


;; Names of standard commands are swapped so there's no error here

(defun vim-mock:scroll-line-up (&optional count)
  (interactive "p")
  (scroll-down (or count 1))
  (when vim-scroll-move-point
    (vim-mock:motion-up (or count 1))))

(defun vim-mock:scroll-line-down (&optional count)
  (interactive "p")
  (scroll-up (or count 1))
  (when vim-scroll-move-point
    (vim-mock:motion-down (or count 1))))

;; motions

(defun vim-mock:motion-down (&optional count)
  (interactive "p")
  (forward-line (or count 1)))

(defun vim-mock:motion-up (&optional count)
  (interactive "p")
  (forward-line (if count (- count) -1)))

(defun vim-mock:motion-left (&optional count)
  (interactive "p")
  (goto-char (max (line-beginning-position)
                  (- (point) (or count 1)))))

(defun vim-mock:motion-right (&optional count)
  (interactive "p")
  (goto-char
   (min (line-end-position)
        (+ (point) (or count 1)))))

(defun vim-mock:motion-jump-item ()
  (interactive)
  (let* ((next-open
          (condition-case nil
              (1- (scan-lists (point) 1 -1))
            (error (point-max))))
         (next-close
          (condition-case nil
              (1- (scan-lists (point) 1 +1))
            (error (point-max))))
         (pos (min next-open next-close)))
    (when (>= pos (line-end-position))
      (error "No matching item found on the current line"))
    (if (= pos next-open)
        (progn
          (goto-char pos)
          (forward-list)
          (backward-char))
      (progn
        (goto-char (1+ pos))
        (backward-list)))))

(defun vim-mock:motion-beginning-of-line ()
  (interactive)
  (beginning-of-line))

(defun vim-mock:motion-end-of-line ()
  (interactive)
  (end-of-line))

(provide 'vim-mock)

;; Local Variables:
;; End:

;; vim-mock.el ends here
