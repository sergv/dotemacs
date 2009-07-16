;;; vim-motions.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; Description:

;; Motions describe functions moving the cursor or representing an
;; argument for an operator.  There are three types of motions:
;; character-wise, line-wise and block-wise.  Usually only the first
;; two types are represented by motion-commands while the last one is
;; implicitly used by visual-block-mode.
;;
;; Each motion-function may return one position or a pair of positions
;; in the buffer.  If it returns one position it's considered as the
;; end of the motion.  If it returns two positions the first is
;; considered as the begin and the other as the end of the motion.
;; The latter is useful for implementing text-objects.
;;
;; Motions are defined using the `vim:define' macro:
;;
;;  (vim:define my-motion (count arg)
;;              :type 'inclusive
;;              :count t
;;              :argument t
;;
;;     ... code ...
;;
;;     position)
;;
;; The type of a motion should be one of `inclusive', `exclusive',
;; `linewise' or `block'.  The first two types are characterwise.
;;
;; If `count' is non-nil, the motion takes an optional count
;; argument.  In this case the first parameter passed to the function
;; is the count (may be nil if no count is given).  If `count' is nil
;; the paramter must be omitted.
;;
;; If `argument' is non-nil, the motion takes an addition
;; key-argument.  In this case the last parameter passed to the
;; function is the corresponding event.  This event may be an
;; arbitrary Emacs-event so the function should check its type and
;; signal an error if the event is invalid.  If `argument' is nil
;; the parameter must be omitted.
;;


(provide 'vim-motions)

(vim:deflocalvar vim:this-column nil
  "The resulting column of the current motion.")

(vim:deflocalvar vim:last-column nil
  "The resulting column of the previous motion.")

(defun vim:adjust-point ()
  "Adjust the pointer after a command."
  ;; TODO: should we check modes directly?
  (when (and (not (eq vim:active-mode vim:insert-mode))
             (not (eq vim:active-mode vim:replace-mode)))
             
    (when vim:this-column
      (move-to-column vim:this-column))
    ;; always stop at the last character (not the newline)
    (when (and (not (eq vim:active-mode vim:visual-mode))
               (eolp) (not (bolp)))
      (backward-char)))
  
  (setq vim:last-column (or vim:this-column
                            (current-column)))
  (setq vim:this-column nil))


(defun vim:use-last-column ()
  "This function should by called by a motion not changing the column."
  (setq vim:this-column vim:last-column))
        

;; This structure is passed to operators taking a motion.
;; It should *not* be returned by motions.
(defstruct (vim:motion
            (:constructor vim:make-motion))
  begin  ; first point in this motion
  end    ; last point in this motion
  type   ; 'inclusive, 'exclusive, 'linewise
  )


(defun vim:motion-line-count (motion)
  "Returns a new motion with same range but new type."
  (let ((cnt
         (1+ (case (vim:motion-type motion)
               ('linewise (- (vim:motion-end motion)
                             (vim:motion-begin motion)))
               ('block (- (car (vim:motion-end motion))
                          (car (vim:motion-begin motion))))
               
               (t (- (line-number-at-pos (vim:motion-end motion))
                     (line-number-at-pos (vim:motion-begin motion))))))))
    (when (< cnt 0)
      (error "Invalid motion (begin must be <= end)"))
    cnt))


(defun vim:motion-begin-row (motion)
  "Returns the row-number of the beginning-position of `motion'."
  (case (vim:motion-type motion)
    ('linewise (vim:motion-begin motion))
    ('block (car (vim:motion-begin motion)))
    (t (line-number-at-pos (vim:motion-begin motion)))))
      

(defun vim:motion-end-row (motion)
  "Returns the row-number of the end-position of `motion'."
  (case (vim:motion-type motion)
    ('linewise (vim:motion-end motion))
    ('block (car (vim:motion-end motion)))
    (t (line-number-at-pos (vim:motion-end motion)))))


(defun vim:motion-begin-pos (motion)
  "Returns the offset of the beginning-position of `motion'."
  (case (vim:motion-type motion)
    ('linewise (save-excursion
                 (goto-line (vim:motion-begin motion))
                 (line-beginning-position)))
    ('block (save-excursion
              (goto-line (car (vim:motion-begin motion)))
              (move-to-column (cdr (vim:motion-begin motion)))
              (point)))
    (t (vim:motion-begin motion))))


(defun vim:motion-end-pos (motion)
  "Returns the offset of the end-position of `motion'."
  (case (vim:motion-type motion)
    ('linewise (save-excursion
                 (goto-line (vim:motion-end motion))
                 (line-end-position)))
    ('block (save-excursion
              (goto-line (car (vim:motion-end motion)))
              (move-to-column (cdr (vim:motion-end motion)))
              (point)))
    (t (vim:motion-end motion))))
                              
      


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

(vim:define vim:motion-beginning-of-line ()
            :type 'exclusive
            :count nil
  "Move the cursor to the beginning of the current line."
  (line-beginning-position))

(vim:define vim:motion-first-non-blank ()
            :type 'exclusive
            :count nil
  "Move the cursor to the first non-blank character of the current line."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[^[:space:]]\\|$" )
    (match-beginning 0)))

(vim:define vim:motion-end-of-line ()
            :type 'inclusive
            :count nil
  "Move the cursor to the end of the current line."
  (line-end-position))

(vim:define vim:motion-last-non-blank ()
            :type 'inclusive
            :count nil
  "Move the cursor to the last non-blank charactor of the current line."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[[:space:]]*$")
    (max (line-beginning-position)
         (1- (match-beginning 0)))))

(vim:define vim:motion-go-to-first-non-blank-beg (count)
            :type 'linewise
  "Moves the cursor to the first non-blank charactor of line count."
  (save-excursion
    (if count
        (goto-line count)
      (goto-char (point-min)))
    (vim:motion-first-non-blank)))
  
(vim:define vim:motion-go-to-first-non-blank-end (count)
            :type 'linewise
  "Moves the cursor to the first non-blank charactor of line count."
  (save-excursion
    (if count
        (goto-line count)
      (goto-char (point-max)))
    (vim:motion-first-non-blank)))

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
    (forward-char)
    (unless (search-forward (char-to-string arg)
                            nil t (or count 1))
      (error (format "Can't find %c" arg)))
    (1- (point))))



(vim:define vim:motion-inner-word (count)
            :type 'inclusive
   "Select `count' words."
   (cons (save-excursion
           (forward-char)
           (re-search-backward "\\b\\w" nil t)
           (match-beginning 0))
         (save-excursion
           (re-search-forward "\\w\\b" nil t (or count 1))
           (match-beginning 0))))
           
   
