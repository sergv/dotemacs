;; pseudovim.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  1 January 2012
;; Description:

(eval-when-compile
  (require 'cl)

  (defvar vim-scroll-move-point))

(require 'common)
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

(defvar-local pseudovim-motion-jump-item-syntax-table nil
  "Override default syntax table for ‘pseudovim-motion-jump-item’.")

(defvar-local pseudovim-motion-jump-item--matching-comment-start-str nil
  "Matching comment construction to treat as matching items. Start
of such sequence - plain string, not regexp.")

(defvar-local pseudovim-motion-jump-item--matching-comment-end-str nil
  "Matching comment construction to treat as matching items. End
of such sequence - plain string, not regexp.")

(defun pseudovim-motion-jump-item-to-pos (start-pos limit)
  (let ((parse-sexp-ignore-comments t))
    (if limit
        (let ((pmax (point-max))
              (pmin (point-min)))
          (save-restriction
            (narrow-to-region
             (max pmin (- (point) limit))
             (min pmax (+ (point) limit)))
            (pseudovim-motion-jump-item-to-pos--impl start-pos)))
      (pseudovim-motion-jump-item-to-pos--impl start-pos))))

(defun pseudovim-motion-jump-item-to-pos--impl (start-pos)
  (let* ((next-open-list
          (condition-case nil
              (1- (scan-lists start-pos 1 -1))
            (error nil)))
         (next-close-list
          (condition-case nil
              (1- (scan-lists start-pos 1 +1))
            (error nil)))
         (next-open-str
          (when pseudovim-motion-jump-item--matching-comment-start-str
            (save-excursion
              (search-forward pseudovim-motion-jump-item--matching-comment-start-str nil t))))
         (next-close-str
          (when pseudovim-motion-jump-item--matching-comment-end-str
            (if (or (text-before-matches? pseudovim-motion-jump-item--matching-comment-end-str)
                    (text-before-matches1? pseudovim-motion-jump-item--matching-comment-end-str))
                (point)
              (save-excursion
                (search-forward pseudovim-motion-jump-item--matching-comment-end-str nil t)))))
         (pos (extended-min (extended-min next-open-list
                                          next-close-list)
                            (extended-min next-open-str
                                          next-close-str))))
    (when (or (null pos)
              (> pos (line-end-position)))
      (error "No matching item found on the current line")
      ;; (error "No matching item found on the current line: %s_|_%s"
      ;;        (buffer-substring-no-properties (line-beginning-position) (point))
      ;;        (buffer-substring-no-properties (point) (line-end-position)))
      )
    (cond
      ((eq pos next-open-str)
       ;; Scan forward
       (or (save-excursion
             (and (search-forward pseudovim-motion-jump-item--matching-comment-end-str nil t)
                  (1- (point))))
           (point-max)))
      ((eq pos next-close-str)
       ;; Scan backward
       (or (save-excursion
             (and (search-backward pseudovim-motion-jump-item--matching-comment-start-str nil t)
                  (point)))
           (point-min)))
      ((eq pos next-open-list)
       ;; Scan forward
       (1- (condition-case nil
               (scan-lists pos 1 0)
             (error (point-max)))))
      ((eq pos next-close-list)
       ;; Scan backward
       (condition-case nil
           (scan-lists (+ 1 pos) -1 0)
         (error (point-min))))
      (t
       (error "Impossible happened")))))

(defun pseudovim-motion-jump-item ()
  "Find the next item in this line after or under the cursor and
jumps to the corresponding one (i.e. jump to matching paren, bracket, etc)."
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
