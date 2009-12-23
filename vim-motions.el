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

(defcustom vim:word "0-9[:alpha:]_"
  "Regexp-set matching a word."
  :type 'string
  :group 'vim-mode)

(defun vim:adjust-point ()
  "Adjust the pointer after a command."
  ;; TODO: should we check modes directly?
  (when (and (not (vim:insert-mode-p))
             );(not vim:replace-mode))
             
    (when vim:this-column
      (move-to-column vim:this-column))
    ;; always stop at the last character (not the newline)
    (when (and (not (vim:visual-mode-p))
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
            (:constructor vim:make-motion-struct))
  has-begin ; t iff the motion defined an explicit begin
  begin  ; first point in this motion
  end    ; last point in this motion
  type   ; 'inclusive, 'exclusive, 'linewise
  )

(defun* vim:make-motion (&key
                         has-begin
			 (begin (point))
			 (end (point))
			 type)
  "Creates a new motion with `begin' and `end' always 
positions within (point-min) and (point-max) and not at 
 (line-end-position) (if possible)."
  (unless type
    (setq type (if (<= begin end) 'inclusive 'exclusive)))
  
  (labels 
      ((shrink-to (pos lower upper)
                  (max lower (min upper pos)))
       
       (normalize-pos (pos)
                      (let ((pos (shrink-to pos (point-min) (point-max))))
                        (shrink-to pos 
                                   (save-excursion
                                     (goto-char pos)
                                     (line-beginning-position))
                                   (save-excursion
                                     (goto-char pos)
                                     (- (line-end-position)
                                        (if (eq type 'inclusive) 1 0)))))))
    
    (vim:make-motion-struct :has-begin has-begin
                            :begin (normalize-pos begin)
                            :end (normalize-pos end)
                            :type type)))


(defun vim:motion-line-count (motion)
  "Returns the number of lines the `motion' covers."
  (1+ (- (vim:motion-last-line motion)
	 (vim:motion-first-line motion))))

(defun vim:motion-first-line (motion)
  "Returns the first line covered by `motion'."
  (min (line-number-at-pos (vim:motion-begin motion))
       (line-number-at-pos (vim:motion-end motion))))

(defun vim:motion-last-line (motion)
  "Returns the last line covered by `motion'."
  (max (line-number-at-pos (vim:motion-begin motion))
       (line-number-at-pos (vim:motion-end motion))))

(defun vim:motion-first-col (motion)
  "Returns the first column covered by `motion'."
  (min (save-excursion 
	 (goto-char (vim:motion-begin motion))
	 (current-column))
       (save-excursion 
	 (goto-char (vim:motion-end motion))
	 (current-column))))

(defun vim:motion-last-col (motion)
  "Returns the last column covered by `motion'."
  (max (save-excursion 
	 (goto-char (vim:motion-begin motion))
	 (current-column))
       (save-excursion 
	 (goto-char (vim:motion-end motion))
	 (current-column))))

(defun vim:motion-begin-pos (motion)
  "Returns the smaller position covered by `motion'."
  (if (eq (vim:motion-type motion) 'linewise)
      (save-excursion
	(goto-line (vim:motion-first-line motion))
	(line-beginning-position))
    (min (vim:motion-begin motion)
	 (vim:motion-end motion))))

(defun vim:motion-end-pos (motion)
  "Returns the larger position covered by `motion' + 1, so 
it can be used as the end of an Emacs range."
  (if (eq (vim:motion-type motion) 'linewise)
      (save-excursion
	(goto-line (vim:motion-last-line motion))
	(line-end-position))

    (let ((e (max (vim:motion-begin motion)
		  (vim:motion-end motion))))
      (if (eq (vim:motion-type motion) 'exclusive)
	  e
	(1+ e)))))


(defmacro vim:do-motion (type expression)
  "Executes a motion body, ensuring the return of a valid vim:motion object."
  (let ((current-pos (gensym))
        (motion (gensym)))
    `(let* ((,current-pos (point))
            (,motion ,expression))
       (if (vim:motion-p ,motion)
           ,motion
         (vim:make-motion :has-begin nil
                          :begin ,current-pos
                          :end (point)
                          :type ,type)))))


(defun vim:adjust-end-of-line-position (pos)
  "If pos is an end-of-line returns pos - 1 and pos otherwise."
  (save-excursion
    (goto-char pos)
    (max (line-beginning-position)
         (min (1- (line-end-position)) pos))))

(vim:defmotion vim:motion-left (exclusive count)
  "Move the cursor count characters left."
  (goto-char (max (line-beginning-position)
                  (- (point) (or count 1)))))

(vim:defmotion vim:motion-right (exclusive count)
  "Move the cursor count characters right."
  (goto-char
   (min (line-end-position)
        (+ (point) (or count 1)))))

(vim:defmotion vim:motion-up (linewise count)
  "Move the cursor count lines up."
  (vim:use-last-column)
  (forward-line (- (or count 1))))

(vim:defmotion vim:motion-down (linewise count)
  "Move the cursor count lines down."
  (vim:use-last-column)
  (forward-line (or count 1)))
  
(vim:defmotion vim:motion-lines (linewise count)
  "Moves count - 1 lines down."
  (vim:use-last-column)
  (forward-line (1- (or count 1))))


(defun vim:motion-beginning-of-line-or-digit-argument ()
  "Feeds a 0 count or moves the cursor to the beginning of the line."
  (interactive)
  (if (and current-prefix-arg
           (not (zerop (prefix-numeric-value current-prefix-arg))))
      (call-interactively 'digit-argument)
    (call-interactively 'vim:motion-beginning-of-line)))
                 

(vim:defmotion vim:motion-beginning-of-line (exclusive)
  "Move the cursor to the beginning of the current line."
  (beginning-of-line))

(vim:defmotion vim:motion-first-non-blank (exclusive)
  "Move the cursor to the first non-blank character of the current line."
  (back-to-indentation))

(vim:defmotion vim:motion-end-of-line (inclusive count)
  "Move the cursor to the end of the current line."
  (end-of-line count))

(vim:defmotion vim:motion-last-non-blank (inclusive count)
  "Move the cursor to the last non-blank charactor of the current line."
  (goto-char
   (save-excursion
     (beginning-of-line count)
     (re-search-forward "[ \t]*$")
     (max (line-beginning-position)
          (1- (match-beginning 0))))))

(vim:defmotion vim:motion-go-to-first-non-blank-beg (linewise count)
  "Moves the cursor to the first non-blank charactor of line count."
  (if count
      (goto-line count)
    (goto-char (point-min)))
  (vim:motion-first-non-blank))
  
(vim:defmotion vim:motion-go-to-first-non-blank-end (linewise count)
  "Moves the cursor to the first non-blank charactor of line count."
  (if count
      (goto-line count)
    (goto-char (point-max)))
  (vim:motion-first-non-blank))


(vim:defmotion vim:motion-fwd-word (exclusive count)
  "Moves the cursor beginning of the next word."
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
             (and (bolp) (eolp))
             (eobp)))
      (forward-char)))
  ;; in operator-pending mode, if we reached the beginning of a new
  ;; line, go back to the end of the previous line
  (when (and (vim:operator-pending-mode-p)
             (looking-back "^[ \t]*")
             (not (save-excursion
                    (forward-line -1)
                    (and (bolp) (eolp)))))
    (forward-line -1)
    (end-of-line)))


(vim:defmotion vim:motion-fwd-WORD (exclusive count)
  "Moves the cursor to beginning of the next WORD."
  (dotimes (i (or count 1))
    (forward-char)
    (while
        (not
         (or (and (looking-back "[ \t\r\n]")
                  (looking-at "[^ \t\r\n]"))
             (and (bolp) (eolp))
             (eobp)))
      (forward-char)))
  ;; in operator-pending mode, if we reached the beginning of a new
  ;; line, go back to the end of the previous line
  (when (and (vim:operator-pending-mode-p)
             (looking-back "^[ \t]*")
             (not (save-excursion
                    (forward-line -1)
                    (and (bolp) (eolp)))))
    (forward-line -1)
    (end-of-line)))


(vim:defmotion vim:motion-fwd-word-end (inclusive count)
  "Moves the cursor to the end of the next word."            
  (dotimes (i (or count 1))
    (forward-char)
    (while
        (not
         (or (and (looking-at (concat "[^ \t\r\n]"
                                      "[ \t\r\n]")))
             (and (looking-at (concat "[" vim:word "]"
                                      "[^ \t\r\n" vim:word "]")))
             (and (looking-at (concat "[^ \t\r\n" vim:word "]"
                                        "[" vim:word "]")))
             (eobp)))
      (forward-char))))


(vim:defmotion vim:motion-fwd-WORD-end (inclusive count)
  "Moves the cursor to the end of the next WORD."            
  (dotimes (i (or count 1))
    (forward-char)
    (while
        (not (and (looking-at (concat "[^ \t\r\n]"
                                      "[ \t\r\n]"))
                  (eobp)))
      (forward-char))))
  

(vim:defmotion vim:motion-bwd-word (exclusive count)
  "Moves the cursor beginning of the previous word."
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
             (and (bolp) (eolp))
             (bobp)))
      (backward-char))))
  

(vim:defmotion vim:motion-bwd-WORD (exclusive count)
  "Moves the cursor to beginning of the previous WORD."
  (dotimes (i (or count 1))
    (backward-char)
    (while
        (not
         (or (and (looking-back "[ \t\r\n]")
                  (looking-at "[^ \t\r\n]"))
             (and (bolp) (eolp))
             (bobp)))
      (backward-char))))


(vim:defmotion vim:motion-bwd-word-end (inclusive count)
  "Moves the cursor to the end of the previous word."            
  (dotimes (i (or count 1))
    (backward-char)
    (while
        (not
         (or (and (looking-at (concat "[^ \t\r\n]"
                                      "[ \t\r\n]")))
             (and (looking-at (concat "[" vim:word "]"
                                      "[^ \t\r\n" vim:word "]")))
             (and (looking-at (concat "[^ \t\r\n" vim:word "]"
                                        "[" vim:word "]")))
             (bobp)))
      (backward-char))))
            

(vim:defmotion vim:motion-bwd-WORD-end (inclusive count)
  "Moves the cursor to the end of the next WORD."            
  (dotimes (i (or count 1))
    (backward-char)
    (while
        (not (and (looking-at (concat "[^ \t\r\n]"
                                      "[ \t\r\n]"))
                  (bobp)))
      (backward-char))))


(vim:defmotion vim:motion-fwd-sentence (exclusive count)
  "Move the cursor `count' sentences forward."
  
  (re-search-forward "\\(?:[.!?][]\"')]*\\(?:[ \t\r]+\\|$\\)\n?\\|\\=[ \t\r]*\n\\(?:[ \t\r]*\n\\)*[ \t\r]*\\)"
                     nil
                     t
                     (or count 1)))
            

(vim:defmotion vim:motion-bwd-sentence (exclusive count)
  "Move the cursor `count' sentences forward."
  
  (dotimes (i (or count 1))
    (goto-char (max (save-excursion (backward-sentence 1) (point))
                    (save-excursion (backward-paragraph 1) (point))))))
            

(vim:defmotion vim:motion-fwd-paragraph (exclusive count)
  "Move the cursor `count' paragraphs forward."
  (forward-paragraph (or count 1)))
            

(vim:defmotion vim:motion-bwd-paragraph (exclusive count)
  "Move the cursor `count' paragraphs backward."
  (backward-paragraph (or count 1)))
            

(vim:defmotion vim:motion-find (inclusive count (argument arg))
  "Move the cursor to the next count'th occurrence of arg."
  (forward-char)
  (let ((case-fold-search nil))
    (unless (search-forward (char-to-string arg)
                            nil t (or count 1))
      (backward-char)
      (error (format "Can't find %c" arg)))
    (setq vim:last-find (cons 'vim:motion-find arg))
    (backward-char)))


(vim:defmotion vim:motion-find-back (exclusive count (argument arg))
  "Move the cursor to the previous count'th occurrence of arg."
  (let ((case-fold-search nil))
    (unless (search-backward (char-to-string arg)
                             nil t (or count 1))
      (error (format "Can't find %c" arg)))
    (setq vim:last-find (cons 'vim:motion-find-back arg))))


(vim:defmotion vim:motion-find-to (inclusive count (argument arg))
  "Move the cursor to the character before the next count'th\
   occurence of arg."
  (vim:motion-find :count count :argument arg)
  (backward-char)
  (setq vim:last-find (cons 'vim:motion-find-to arg)))


(vim:defmotion vim:motion-find-back-to (exclusive count (argument arg))
  "Move the cursor to the character after the previous count'th\
   occurence of arg."
   (vim:motion-find-back :count count :argument arg)
   (forward-char)
   (setq vim:last-find (cons 'vim:motion-find-to arg)))


(vim:defmotion vim:motion-repeat-last-find (inclusive count)
  "Repeats the last find command."
  (unless vim:last-find
    (error "No previous find command."))
  (funcall (car vim:last-find)
           :count count
           :argument (cdr vim:last-find)))


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
      (funcall func :count count :argument arg))))


(vim:defmotion vim:motion-jump-item (inclusive)
  "Find the next item in this line after or under the cursor and
jumps to the corresponding one."
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
          (backward-list))))))


(vim:defmotion vim:motion-inner-word (inclusive count)
   "Select `count' words."
   (let ((beg (save-excursion
                (forward-char)
                (vim:motion-bwd-word)
                (point)))
         (end (save-excursion
                (backward-char)
                (vim:motion-fwd-word-end :count count)
                (point))))
     (goto-char end)
     (vim:make-motion :has-begin t
                      :begin beg
                      :end end
                      :type 'inclusive)))
           
   
