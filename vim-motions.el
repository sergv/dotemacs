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


(defun vim:adjust-point ()
  "Adjust the pointer after a command."
  (when (and (eolp) (not (bolp))
             (not (eq vim:active-mode vim:insert-mode)))
    (backward-char)))

;; This structure is passed to operators taking a motion.
(defstruct (vim:motion
            (:constructor vim:make-motion))
  (begin (point)) ; first point in this motion
  end             ; last point in this motion
  type            ; 'inclusive, 'exclusive, 'linewise
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

(defun vim:adjust-point ()
  "Adjust the pointer after a command."
  (when (and (eolp) (not (bolp))
             (not (eq vim:active-mode vim:insert-mode)))
    (backward-char)))


(defun vim:motion-left (count)
  "Move the cursor count characters left."
  (vim:make-motion :end (max (line-beginning-position)
                             (- (point) (or count 1)))
                   :type 'exclusive))

(defun vim:motion-right (count)
  "Move the cursor count characters right."
  (vim:make-motion :end (min (line-end-position)
                             (+ (point) (or count 1)))
                   :type 'exclusive))

(defun vim:motion-up (count)
  "Move the cursor count lines up."
  (vim:make-motion :end (save-excursion
                          (forward-line (- (or count 1)))
                          (point))
                   :type 'linewise))

(defun vim:motion-down (count)
  "Move the cursor count lines down."
  (vim:make-motion :end (save-excursion
                          (forward-line (or count 1))
                          (point))
                   :type 'linewise))

(defun vim:motion-beginning-of-line (count)
  "Move the cursor to the beginning of the current line."
  (vim:make-motion :end (line-beginning-position)
                   :type 'exclusive))

(defun vim:motion-first-non-blank (count)
  "Move the cursor to the first non-blank character of the current line."
  (vim:make-motion :end (save-excursion
                          (beginning-of-line)
                          (re-search-forward "[^[:space:]]\\|$" )
                          (match-beginning 0))
                   :type 'exclusive))

(defun vim:motion-end-of-line (count)
  "Move the cursor to the end of the current line."
  (vim:make-motion :end (line-end-position)
                   :type 'inclusive))

(defun vim:motion-last-non-blank (count)
  "Move the cursor to the last non-blank charactor of the current line."
  (vim:make-motion :end (save-excursion
                          (beginning-of-line)
                          (re-search-forward "[[:space:]]*$")
                          (max (line-beginning-position)
                               (1- (match-beginning 0))))
                   :type 'inclusive))

(defun vim:motion-go-to-first-non-blank-beg (count)
  "Moves the cursor to the first non-blank charactor of line count."
  (let ((motion (save-excursion
                  (if count
                      (goto-line count)
                    (goto-char (point-min)))
                  (vim:motion-first-non-blank 1))))
  (vim:make-motion :end (vim:motion-end motion)
                   :type 'linewise)))
  
(defun vim:motion-go-to-first-non-blank-end (count)
  "Moves the cursor to the first non-blank charactor of line count."
  (let ((motion (save-excursion
                  (if count
                      (goto-line count)
                    (goto-char (point-max)))
                  (vim:motion-first-non-blank 1))))
    (vim:make-motion :end (vim:motion-end motion)
                     :type 'linewise)))

(defun vim:motion-fwd-word (count)
  "Moves the cursor beginning of the next word."
  (vim:make-motion :end (save-excursion
                          (forward-word (or count 1))
                          (point))
                   :type 'exclusive))


(defun vim:motion-find (count arg)
  "Move the cursor to the next count'th occurrence of arg."
  (save-excursion
    (unless (search-forward (char-to-string arg)
                            nil t (or count 1))
      (error (format "Can't find %s" arg)))
    (vim:make-motion :end (1- (point))
                     :type 'inclusive)))


