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
;; A motion function may overwrite its default type given by :type
;; by prepending a symbol to its return value describing the type
;; (see vim:motion-repeat-last-find for an example).
;;
;; Motions are defined using the `vim:defmotion' macro:
;;
;;  (vim:defmotion my-motion (inclusive count (argument arg))
;;
;;     ... code ...
;;
;;     position)
;;
;; The type of a motion should be one of `inclusive', `exclusive',
;; `linewise' or `block'.  The first two types are characterwise.
;; This type must be specified in the parameter list.
;;
;; If the motion takes a count, the `count' parameter should be specified.
;; The count will passed to an argument named `count'.  If you wish to rename
;; this parameter, use (count new-name) instead.
;;
;; If the motion takes an argument, the `argument' parameter should be specified.
;; The argument will passed to an argument named `argument'.  If you wish to rename
;; this parameter, use (argument new-name) instead.


(provide 'vim-motions)

(vim:deflocalvar vim:this-column nil
  "The resulting column of the current motion.")

(vim:deflocalvar vim:last-column nil
  "The resulting column of the previous motion.")

(vim:deflocalvar vim:last-find nil
  "The previous find command (command . arg).")

(defcustom vim:word "0-9a-zA-Z_"
  "Regexp-set matching a word."
  :type 'string
  :group 'vim-mode)

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


(defun vim:motion-begin-col (motion)
  "Returns the column-number of the beginning-position of `motion'."
  (case (vim:motion-type motion)
    ('block (cdr (vim:motion-begin motion)))
    (t (error "Column information only available for block motions."))))
      

(defun vim:motion-end-col (motion)
  "Returns the column-number of the end-position of `motion'."
  (case (vim:motion-type motion)
    ('block (cdr (vim:motion-end motion)))
    (t (error "Column information only available for block motions."))))


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

(vim:defmotion vim:motion-left (exclusive count)
  "Move the cursor count characters left."
  (max (line-beginning-position)
       (- (point) (or count 1))))

(vim:defmotion vim:motion-right (exclusive count)
  "Move the cursor count characters right."
  (min (line-end-position)
       (+ (point) (or count 1))))

(vim:defmotion vim:motion-up (linewise count)
  "Move the cursor count lines up."
  (vim:use-last-column)
  (save-excursion
    (forward-line (- (or count 1)))
    (point)))

(vim:defmotion vim:motion-down (linewise count)
  "Move the cursor count lines down."
  (vim:use-last-column)
  (save-excursion
    (forward-line (or count 1))
    (point)))

(vim:defmotion vim:motion-beginning-of-line (exclusive)
  "Move the cursor to the beginning of the current line."
  (line-beginning-position))

(vim:defmotion vim:motion-first-non-blank (exclusive)
  "Move the cursor to the first non-blank character of the current line."
  (save-excursion
    (back-to-indentation)
    (point)))

(vim:defmotion vim:motion-end-of-line (inclusive)
  "Move the cursor to the end of the current line."
  (line-end-position))

(vim:defmotion vim:motion-last-non-blank (inclusive)
  "Move the cursor to the last non-blank charactor of the current line."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[ \t]*$")
    (max (line-beginning-position)
         (1- (match-beginning 0)))))

(vim:defmotion vim:motion-go-to-first-non-blank-beg (linewise count)
  "Moves the cursor to the first non-blank charactor of line count."
  (save-excursion
    (if count
        (goto-line count)
      (goto-char (point-min)))
    (vim:motion-first-non-blank)))
  
(vim:defmotion vim:motion-go-to-first-non-blank-end (linewise count)
  "Moves the cursor to the first non-blank charactor of line count."
  (save-excursion
    (if count
        (goto-line count)
      (goto-char (point-max)))
    (vim:motion-first-non-blank)))


(vim:defmotion vim:motion-fwd-word (exclusive count)
  "Moves the cursor beginning of the next word."
  (save-excursion
    (dotimes (i (or count 1))
      (forward-char)
      (while
          (not
           (or (and (looking-back "[ \t\r\n]")
                    (looking-at "[^ \t\r\n]"))
               (and (looking-back (concat "[" vim:word "]"))
                    (looking-at (concat "[^ \t\r\n" vim:word "]")))
               (and (looking-back (concat "[^ \t\r\n" vim:word "]"))
                    (looking-at (concat "[" vim:word "]")))
               (and (bolp) (eolp))))
        (forward-char)))
    ;; in operator-pending mode, if we reached the beginning of a new
    ;; line, go back to the end of the previous line
    (when (and (vim:operator-pending-p)
               (looking-back "^[ \t]*")
               (not (save-excursion
                      (forward-line -1)
                      (and (bolp) (eolp)))))
      (forward-line -1)
      (end-of-line))
    (point)))


(vim:defmotion vim:motion-fwd-WORD (exclusive count)
  "Moves the cursor to beginning of the next WORD."
  (save-excursion
    (dotimes (i (or count 1))
      (forward-char)
      (while
          (not
           (or (and (looking-back "[ \t\r\n]")
                    (looking-at "[^ \t\r\n]"))
               (and (bolp) (eolp))))
        (forward-char)))
    ;; in operator-pending mode, if we reached the beginning of a new
    ;; line, go back to the end of the previous line
    (when (and (vim:operator-pending-p)
               (looking-back "^[ \t]*")
               (not (save-excursion
                      (forward-line -1)
                      (and (bolp) (eolp)))))
      (forward-line -1)
      (end-of-line))
    (point)))


(vim:defmotion vim:motion-fwd-word-end (inclusive count)
  "Moves the cursor to the end of the next word."            
  (save-excursion
    (dotimes (i (or count 1))
      (forward-char)
      (while
          (not
           (or (and (looking-at (concat "[^ \t\r\n]"
                                        "[ \t\r\n]")))
               (and (looking-at (concat "[" vim:word "]"
                                        "[^ \t\r\n" vim:word "]")))
               (and (looking-at (concat "[^ \t\r\n" vim:word "]"
                                        "[" vim:word "]")))))
        (forward-char)))
    (point)))
  

(vim:defmotion vim:motion-fwd-WORD-end (inclusive count)
  "Moves the cursor to the end of the next WORD."            
  (save-excursion
    (dotimes (i (or count 1))
      (forward-char)
      (while
          (not (and (looking-at (concat "[^ \t\r\n]"
                                        "[ \t\r\n]"))))
        (forward-char)))
    (point)))
  

(vim:defmotion vim:motion-bwd-word (exclusive count)
  "Moves the cursor beginning of the previous word."
  (save-excursion
    (dotimes (i (or count 1))
      (backward-char)
      (while
          (not
           (or (and (looking-back "[ \t\r\n]")
                    (looking-at "[^ \t\r\n]"))
               (and (looking-back (concat "[" vim:word "]"))
                    (looking-at (concat "[^ \t\r\n" vim:word "]")))
               (and (looking-back (concat "[^ \t\r\n" vim:word "]"))
                    (looking-at (concat "[" vim:word "]")))
               (and (bolp) (eolp))))
        (backward-char)))
    (point)))
  

(vim:defmotion vim:motion-bwd-WORD (exclusive count)
  "Moves the cursor to beginning of the previous WORD."
  (save-excursion
    (dotimes (i (or count 1))
      (backward-char)
      (while
          (not
           (or (and (looking-back "[ \t\r\n]")
                    (looking-at "[^ \t\r\n]"))
               (and (bolp) (eolp))))
        (backward-char)))
    (point)))


(vim:defmotion vim:motion-bwd-word-end (inclusive count)
  "Moves the cursor to the end of the previous word."            
  (save-excursion
    (dotimes (i (or count 1))
      (backward-char)
      (while
          (not
           (or (and (looking-at (concat "[^ \t\r\n]"
                                        "[ \t\r\n]")))
               (and (looking-at (concat "[" vim:word "]"
                                        "[^ \t\r\n" vim:word "]")))
               (and (looking-at (concat "[^ \t\r\n" vim:word "]"
                                        "[" vim:word "]")))))
        (backward-char)))
    (point)))
            

(vim:defmotion vim:motion-bwd-WORD-end (inclusive count)
  "Moves the cursor to the end of the next WORD."            
  (save-excursion
    (dotimes (i (or count 1))
      (backward-char)
      (while
          (not (and (looking-at (concat "[^ \t\r\n]"
                                        "[ \t\r\n]"))))
        (backward-char)))
    (point)))
            

(vim:defmotion vim:motion-find (inclusive count (argument arg))
  "Move the cursor to the next count'th occurrence of arg."
  (save-excursion
    (forward-char)
    (let ((case-fold-search nil))
      (unless (search-forward (char-to-string arg)
                              nil t (or count 1))
        (error (format "Can't find %c" arg)))
      (setq vim:last-find (cons 'vim:motion-find arg))
      (1- (point)))))


(vim:defmotion vim:motion-find-back (exclusive count (argument arg))
  "Move the cursor to the previous count'th occurrence of arg."
  (save-excursion
    (let ((case-fold-search nil))
      (unless (search-backward (char-to-string arg)
                               nil t (or count 1))
        (error (format "Can't find %c" arg)))
      (setq vim:last-find (cons 'vim:motion-find-back arg))
      (point))))


(vim:defmotion vim:motion-find-to (inclusive count (argument arg))
  "Move the cursor to the character before the next count'th\
   occurence of arg."
  (let ((pos (1- (vim:motion-find :count count :argument arg))))
    (setq vim:last-find (cons 'vim:motion-find-to arg))
    pos))


(vim:defmotion vim:motion-find-back-to (exclusive count (argument arg))
  "Move the cursor to the character after the previous count'th\
   occurence of arg."
  (let ((pos (1+ (vim:motion-find-back :count count :argument arg))))
    (setq vim:last-find (cons 'vim:motion-find-to arg))
    pos))


(vim:defmotion vim:motion-repeat-last-find (inclusive count)
  "Repeats the last find command."
  (unless vim:last-find
    (error "No previous find command."))
  (cons (vim:cmd-type (car vim:last-find))
        (funcall (car vim:last-find)
                 :count count
                 :argument (cdr vim:last-find))))


(vim:defmotion vim:motion-repeat-last-find-opposite (inclusive count)
  "Repeats the last find command."
  (unless vim:last-find
    (error "No previous find command."))
  (let ((func (case (car vim:last-find)
                ('vim:motion-find 'vim:motion-find-back)
                ('vim:motion-find-back 'vim:motion-find)
                ('vim:motion-find-to 'vim:motion-find-back-to)
                ('vim:motion-find-back-to 'vim:motion-find-to)
                (t (error (format "Unexpected find command %s"
                                  (car vim:last-find))))))
        (arg (cdr vim:last-find)))
    (let ((vim:last-find nil))
      (cons (vim:cmd-type func)
            (funcall func :count count :argument arg)))))


(vim:defmotion vim:motion-jump-item (inclusive)
  "Find the next item in this line after or under the cursor and
jumps to the corresponding one."
  (save-excursion
    (let ((next-open
           (condition-case err
               (1- (scan-lists (point) 1 -1))
             (error
              (point-max))))
          (next-close
           (condition-case nil
               (1- (scan-lists (point) 1 +1))
             (error (point-max)))))
      (let ((pos (min next-open next-close)))
        (when (>= pos (line-end-position))
          (error "No matching item found on the current line."))
        (if (= pos next-open)
            (progn
              (goto-char pos)
              (forward-list)
              (backward-char))
          (progn
            (goto-char (1+ pos))
            (backward-list)))
        (point)))))


(vim:defmotion vim:motion-inner-word (inclusive count)
   "Select `count' words."
   (cons (save-excursion
           (forward-char)
           (re-search-backward "\\b\\w" nil t)
           (match-beginning 0))
         (save-excursion
           (re-search-forward "\\w\\b" nil t (or count 1))
           (match-beginning 0))))
           
   
