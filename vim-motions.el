;;; vim-motions.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; TODO: bad insert-mode hack in vim:adjust-point

(provide 'vim-motions)

(defvar vim:this-column nil)
(defvar vim:last-column nil)

(defun vim:adjust-point ()
  "Adjust the pointer after a command."
  (when (and (not (eq vim:active-mode vim:insert-mode))
             (not (eq vim:active-mode vim:replace-mode)))
    (when vim:this-column
      (goto-char (min (+ (line-beginning-position) vim:this-column)
                      (line-end-position))))
    ;; always stop at the last character (not the newline)
    (when (and (eolp) (not (bolp)))
      (backward-char)))
  (setq vim:last-column (or vim:this-column
                            (- (point) (line-beginning-position))))
  (setq vim:this-column nil))


(defun vim:use-last-column ()
  (setq vim:this-column vim:last-column))
        

;; This structure is passed to operators taking a motion.
(defstruct (vim:motion
            (:constructor vim:make-motion))
  begin  ; first point in this motion
  end    ; last point in this motion
  type   ; 'inclusive, 'exclusive, 'linewise
  )

(defun vim:motion-line-count (motion)
  "Returns a new motion with same range but new type."
  (1+ (- (line-number-at-pos (max (vim:motion-begin motion)
                                  (vim:motion-end motion)))
         (line-number-at-pos (min (vim:motion-begin motion)
                                  (vim:motion-end motion))))))


(defun vim:adjust-end-of-line-position (pos)
  "If pos is an end-of-line returns pos - 1 and pos otherwise."
  (save-excursion
    (goto-char pos)
    (if (eolp)
        (1- pos)
      pos)))

(vim:define vim:motion-left (count)
            :type 'exclusive
  "Move the cursor count characters left."
  (max (line-beginning-position)
       (- (point) (or count 1))))

(vim:define vim:motion-right (count)
            :type 'exclusive
  "Move the cursor count characters right."
  (min (line-end-position)
       (+ (point) (or count 1))))

(vim:define vim:motion-up (count)
            :type 'linewise
  "Move the cursor count lines up."
  (vim:use-last-column)
  (save-excursion
    (forward-line (- (or count 1)))
    (point)))

(vim:define vim:motion-down (count)
            :type 'linewise
  "Move the cursor count lines down."
  (vim:use-last-column)
  (save-excursion
    (forward-line (or count 1))
    (point)))

(vim:define vim:motion-beginning-of-line (count)
            :type 'exclusive
  "Move the cursor to the beginning of the current line."
  (line-beginning-position))

(vim:define vim:motion-first-non-blank (count)
            :type 'exclusive
  "Move the cursor to the first non-blank character of the current line."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[^[:space:]]\\|$" )
    (match-beginning 0)))

(vim:define vim:motion-end-of-line (count)
            :type 'inclusive
  "Move the cursor to the end of the current line."
  (line-end-position))

(vim:define vim:motion-last-non-blank (count)
            :type 'inclusive
  "Move the cursor to the last non-blank charactor of the current line."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[[:space:]]*$")
    (max (line-beginning-position)
         (1- (match-beginning 0)))))

(vim:define vim:motion-go-to-first-non-blank-beg (count)
            :type 'linewise
  "Moves the cursor to the first non-blank charactor of line count."
  (let ((motion (save-excursion
                  (if count
                      (goto-line count)
                    (goto-char (point-min)))
                  (vim:motion-first-non-blank 1))))
    (vim:motion-end motion)))
  
(vim:define vim:motion-go-to-first-non-blank-end (count)
            :type 'linewise
  "Moves the cursor to the first non-blank charactor of line count."
  (let ((motion (save-excursion
                  (if count
                      (goto-line count)
                    (goto-char (point-max)))
                  (vim:motion-first-non-blank 1))))
    (vim:motion-end motion)))

(vim:define vim:motion-fwd-word (count)
            :type 'exclusive
  "Moves the cursor beginning of the next word."
  (save-excursion
    (forward-word (or count 1))
    (point)))


(vim:define vim:motion-find (count arg)
            :type 'inclusive
            :argument t
  "Move the cursor to the next count'th occurrence of arg."
  (save-excursion
    (unless (search-forward (char-to-string arg)
                            nil t (or count 1))
      (error (format "Can't find %s" arg)))
    (1- (point))))


