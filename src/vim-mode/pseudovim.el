;; pseudovim.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  1 January 2012
;; Description:

(eval-when-compile
  (require 'cl)

  (defvar vim-scroll-move-point))

(require 'vim-motions)

(defun pseudovim-scroll-line-to-center ()
  "Scroll selected line to the center of the window."
  (interactive)
  (recenter nil))

(defun pseudovim-scroll-line-to-top ()
  "Scroll selected line to the top of the window."
  (interactive)
  (recenter 0))

(defun pseudovim-scroll-line-to-bottom ()
  "Scroll selected line to the bottom of the window."
  (interactive)
  (recenter -1))

(defun pseudovim-motion-fwd-word-end (count)
  (interactive "p")
  (vim--move-fwd-end (or count 1) #'vim-boundary--word))

(defun pseudovim-motion-fwd-WORD-end (count)
  (interactive "p")
  (vim--move-fwd-end (or count 1) #'vim-boundary--WORD))

(defun pseudovim-motion-bwd-word-end (count)
  (interactive "p")
  (vim--move-bwd-end (or count 1) #'vim-boundary--word))

(defun pseudovim-motion-bwd-WORD-end (count)
  (interactive "p")
  (vim--move-bwd-end (or count 1) #'vim-boundary--WORD))

(defun pseudovim-motion-fwd-word (count)
  "Moves the cursor to the beginning of the next word."
  (interactive "p")
  (vim--move-fwd-beg (or count 1) #'vim-boundary--word))

(defun pseudovim-motion-bwd-word (count)
  "Moves the cursor beginning of the previous word."
  (interactive "p")
  (vim--move-bwd-beg (or count 1) #'vim-boundary--word))

(defun pseudovim-motion-fwd-WORD (count)
  "Moves the cursor to beginning of the next WORD."
  (interactive "p")
  (let ((line (when (vim-operator-pending-mode-p)
                (line-number-at-pos (point)))))
    (vim--move-fwd-beg (or count 1) #'vim-boundary--WORD)

    ;; in operator-pending mode, if we reached the beginning of a new
    ;; line, go back to the end of the previous line
    (when (and line
               (< line (line-number-at-pos (point))) ; only if we skipped a newline
               (vim--looking-back "^[ \t]*")
               (not (save-excursion
                      (forward-visible-line -1)
                      (and (bolp) (eolp)))))
      (forward-visible-line -1)
      (end-of-line))))

(defun pseudovim-motion-bwd-WORD (count)
  "Moves the cursor to beginning of the previous WORD."
  (interactive "p")
  (vim--move-bwd-beg (or count 1) #'vim-boundary--WORD))

;; Names of standard commands are swapped so there's no error here

(defun pseudovim-scroll-line-up (&optional count)
  (interactive "p")
  (scroll-down (or count 1))
  (when vim-scroll-move-point
    (pseudovim-motion-up (or count 1))))

(defun pseudovim-scroll-line-down (&optional count)
  (interactive "p")
  (scroll-up (or count 1))
  (when vim-scroll-move-point
    (pseudovim-motion-down (or count 1))))

;; motions

(defun pseudovim-motion-down (&optional count)
  (interactive "p")
  (forward-line (or count 1)))

(defun pseudovim-motion-up (&optional count)
  (interactive "p")
  (forward-line (if count (- count) -1)))

(defun pseudovim-motion-left (&optional count)
  (interactive "p")
  (goto-char (max (line-beginning-position)
                  (- (point) (or count 1)))))

(defun pseudovim-motion-right (&optional count)
  (interactive "p")
  (goto-char
   (min (line-end-position)
        (+ (point) (or count 1)))))

(defun pseudovim-motion-jump-item-to-pos (start-pos limit)
  (let ((pmax (point-max))
        (pmin (point-min)))
    (save-restriction
      (when limit
        (narrow-to-region
         (max pmin (- (point) limit))
         (min pmax (+ (point) limit))))
      (let* ((next-open
              (condition-case nil
                  (1- (scan-lists start-pos 1 -1))
                (error (point-max))))
             (next-close
              (condition-case nil
                  (1- (scan-lists start-pos 1 +1))
                (error (point-max))))
             (pos (min next-open
                       next-close)))
        (when (>= pos (line-end-position))
          (error "No matching item found on the current line"))
        (if (= pos next-open)
            (1- (or (scan-lists pos 1 0) (buffer-end 1)))
          (or (scan-lists (+ 1 pos) -1 0) (buffer-end -1)))))))

(defun pseudovim-motion-jump-item ()
  "Find the next item in this line after or under the cursor and
jumps to the corresponding one."
  (interactive)
  (goto-char (pseudovim-motion-jump-item-to-pos (point) nil)))

(defun pseudovim-motion-beginning-of-line-or-digit-argument ()
  (interactive)
  (vim--motion-beginning-of-line-or-digit-argument-impl
   #'beginning-of-line))

(defun pseudovim-motion-first-non-blank ()
  (interactive)
  (pseudovim-motion-beginning-of-line-or-digit-argument)
  (skip-to-indentation))

(defun pseudovim-motion-end-of-line ()
  (interactive)
  (end-of-line))

(provide 'pseudovim)

;; Local Variables:
;; End:

;; pseudovim.el ends here
