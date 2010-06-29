;;; vim-motions.el - Implementation of VIM motions.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Motions describe functions moving the cursor or representing an
;; argument for an operator. There are three types of motions:
;; character-wise, line-wise and block-wise. Usually only the first
;; two types are represented by motion-commands while the last one is
;; implicitly used by visual-block-mode.
;;
;; A motion is defined using the macro 'vim:defmotion' which has the
;; following form:
;; 
;; (vim:defmotion name (count 
;;                      argument[:{char}] 
;;                      {inclusive,exclusive,linewise,block}) 
;;   body...)
;;
;; Each of the arguments is optional. The names of the arguments must
;; be exactly as in the definition above (but see 'Argument-renaming'
;; below).
;;
;; The COUNT argument (if given) takes the count of the motion which
;; is usually the number how often the motion should be repeated. This
;; argument may be nil if no count is given.
;;
;; The ARGUMENT argument is an aditional text-argument to be given and
;; may be nil, too. If it is specified as ARGUMENT:CHAR, the argument
;; is a one-character argument (see `vim:motion-find' usually bound to
;; 'f' for an example), otherwise it's a string-argument. Currently all
;; motions taking an argument take a character-argument.
;;
;; One if the pseudo-arguments INCLUSIVE, EXCLUSIVE, LINEWISE and BLOCK
;; must be given and specifies the type of the motion. See the Vim-manual
;; for an explanation of motion-types.
;;
;; If you do not like the default argument names, they may be renamed by using
;; (ARG NEWNAME) instead of ARG, e.g.
;;
;;   (vim:defmotion vim:motion-find (inclusive count (argument:char arg))
;;
;; defines an inclusive motion with a count-argument but renames the 
;; character-argument to ARG.
;;
;; Each motion should return an object of type `vim:motion'. This may happen
;; in one of two ways: explicit or implicit. 
;;
;; Explicit: The function creates an object of type `vim:motion' using
;; `vim:make-motion' specifing the begin position, the end position
;; and the type of the motion (overriding the motion-type specified in
;; the argument list). If the motion is a usual motion, the vim:motion
;; parameter :has-begin should be nil, if it's a text-objects it
;; should be t. The difference is that text-objects actively define a
;; range from the begin-position to the end-position, while
;; conventional motions define only the end-position placing begin at
;; (point). The motion should also change (point) usually to the
;; end-position of the returned motion.
;;
;; Implicit: Creating an explicit `vim:motion' object is overkill for
;; most simple motions. If the motion does not return a `vim:motion'
;; object, its created implicitly with the following rules: 
;;   - the begin-position is set to (point) before the execution of motion's 
;;     body
;;   - the end-position is set to (point) after the execution of motion's
;;     body
;;   - :has-begin is nil 
;;   - the type is the type defined in the motion's argument list
;; Almost all motions defined in this file are implicit.
;;
;; Note that, independently on whether the motion is defined
;; implicitly or explicitly, calling a motion always returns a
;; `vim:motion' object, i.e. (vim:motion-p (vim:motion-left)) would
;; return t.
;;
;; Motions can be bound to some key-sequence as any other interactive
;; Emacs function, but they work only in vim-mode. Ususally motions
;; are bound to the operator-pending-mode keymap using `vim:omap'.

;;; Code:

(vim:deflocalvar vim:this-column nil
  "The resulting column of the current motion.")

(vim:deflocalvar vim:last-column nil
  "The resulting column of the previous motion.")

(vim:deflocalvar vim:last-find nil
  "The previous find command (command . arg).")

(defcustom vim:word "[:word:]"
  "Regexp-set matching a word."
  :type 'string
  :group 'vim-mode)

(defcustom vim:whitespace " \t\r\n"
  "Regexp-set matching a whitespace."
  :type 'string
  :group 'vim-mode)

(defun vim:adjust-point ()
  "Adjust the pointer after a command."
  ;; TODO: should we check modes directly?
  (when (and (not (vim:insert-mode-p))
             )				;(not vim:replace-mode))
    
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
  has-begin		  ; t iff the motion defined an explicit begin
  begin			  ; first point in this motion
  end			  ; last point in this motion
  type			  ; 'inclusive, 'exclusive, 'linewise
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
  "Returns the smaller position covered by `motion'.
The result is modified depending on the motion type to
return the correct start-position of emacs-ranges, i.e.
  - if motion is inclusive or exclusive, nothing is changed
  - if motion is line-wise, is always bol of the first line in the motion,
  - if motion is block 1 is added if and only if the begin column
    is larget than the end column."
  (case (vim:motion-type motion)
    (linewise
     (save-excursion
       (goto-line (vim:motion-first-line motion))
       (line-beginning-position)))
    ('block
        (let ((b (min (vim:motion-begin motion) (vim:motion-end motion)))
              (e (max (vim:motion-begin motion) (vim:motion-end motion))))
          (if (> (save-excursion (goto-char b) (current-column))
                 (save-excursion (goto-char e) (current-column)))
              (1+ b)
            b)))
    (t (min (vim:motion-begin motion) (vim:motion-end motion)))))


(defun vim:motion-end-pos (motion)
  "Returns the larger position covered by `motion'.
The result is modified depending on the motion type to
return the correct end-position of emacs-ranges, i.e.
  - if motion is inclusive, 1 is added,
  - if motion is exclusive, nothing is change,
  - if motion is line-wise, is always eol of the last line in the motion,
  - if motion is block 1 is added if and only if the end column
    is larger than or equal to the begin column."
  (case (vim:motion-type motion)
    (linewise
     (save-excursion
       (goto-line (vim:motion-last-line motion))
       (line-end-position)))
    ('block
        (let ((b (min (vim:motion-begin motion) (vim:motion-end motion)))
              (e (max (vim:motion-begin motion) (vim:motion-end motion))))
          (if (>= (save-excursion (goto-char e) (current-column))
                  (save-excursion (goto-char b) (current-column)))
              (1+ e)
            e)))
    (inclusive
     (1+ (max (vim:motion-begin motion) (vim:motion-end motion))))
    (t (max (vim:motion-begin motion) (vim:motion-end motion)))))


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
(font-lock-add-keywords 'emacs-lisp-mode '("vim:do-motion"))


(vim:deflocalvar vim:local-marks-alist nil
  "Local marks for this buffer.")

(defvar vim:global-marks-alist nil
  "Global marks.")

(defun vim:local-mark-p (mark-char)
  "Returns t if `mark-char' is a local mark."
  (or (and (>= mark-char ?a) (<= mark-char ?z))
      (member mark-char '(?^ ?. ?< ?>))))

(defun vim:global-mark-p (mark-char)
  "Returns t if `mark-char' is a global mark."
  (and (>= mark-char ?A) (<= mark-char ?z)))

(defun vim:set-mark (mark-char &optional pos)
  "Sets the mark `mark-char' to `pos' or (point)."
  (let (m)
    (cond
     ((vim:local-mark-p mark-char)
      (setq m (or (cdr-safe (assoc mark-char vim:local-marks-alist))))
      (unless m
        (setq m (make-marker))
        (push (cons mark-char m) vim:local-marks-alist)))
     
     ((vim:global-mark-p mark-char)
      (setq m (or (cdr-safe (assoc mark-char vim:global-marks-alist))))
      (unless m
        (setq m (make-marker))
        (push (cons mark-char m) vim:global-marks-alist)))
     (t (error "Unknown mark '%c'" mark-char)))
    (set-marker m (or pos (point)))))

(defun vim:get-local-mark (mark-char)
  "Returns the marker of `mark-char' if it's in the current buffer."
  (cond
   ((vim:local-mark-p mark-char)
    (let ((m (cdr-safe (assoc mark-char vim:local-marks-alist))))
      (if m m
        (error "No mark '%c' defined." mark-char))))
   ((vim:global-mark-p mark-char)
    (let ((m (cdr-safe (assoc mark-char vim:global-marks-alist))))
      (if m
          (if (eq (marker-buffer m) (current-buffer))
              m
            (error "Global mark '%c' not in current buffer." mark-char))
        (error "No mark '%c' defined." mark-char))))
   (t
    (error "Unknown mark: '%c'" mark-char))))

(add-hook 'before-change-functions 'vim:set-change-mark)
(defun vim:set-change-mark (beg end)
  "Sets the change mark . to `beg'."
  (vim:set-mark ?. beg))

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
  (let ((word (concat "[" vim:word "]"))
        (noword (concat "[^" vim:word "]"))
        (nonword (concat "[^" vim:whitespace vim:word "]"))
        (nononword (concat "[" vim:whitespace vim:word "]")))
    (dotimes (i (or count 1))
      (forward-char)
      (while
          (not
           (or (and (looking-back noword) (looking-at word))
               (and (looking-back nononword) (looking-at nonword))
               (and (bolp) (eolp))
               (eobp)))
        (forward-char))))

  ;; in operator-pending mode, if we reached the beginning of a new
  ;; line, go back to the end of the previous line
  (when (and (vim:operator-pending-mode-p)
             (vim:looking-back "^[ \t]*")
             (not (save-excursion
                    (forward-line -1)
                    (and (bolp) (eolp)))))
    (forward-line -1)
    (end-of-line)))


(vim:defmotion vim:motion-bwd-word (exclusive count)
  "Moves the cursor beginning of the previous word."
  (let ((word (concat "[" vim:word "]"))
        (noword (concat "[^" vim:word "]"))
        (nonword (concat "[^" vim:whitespace vim:word "]"))
        (nononword (concat "[" vim:whitespace vim:word "]")))
    (dotimes (i (or count 1))
      (backward-char)
      (while
          (not
           (or (and (looking-back noword) (looking-at word))
               (and (looking-back nononword) (looking-at nonword))
               (and (bolp) (eolp))
               (eobp)))
        (backward-char)))))


(vim:defmotion vim:motion-fwd-word-end (inclusive count)
  "Moves the cursor to the end of the next word."            
  (let ((wordend (concat "[" vim:word "][^" vim:word "]"))
	(nowordend (concat "[^" vim:whitespace vim:word "][" vim:whitespace vim:word "]")))
    (forward-char)
    (re-search-forward (concat wordend "\\|" nowordend "\\|\\'") nil nil (or count 1))
    (goto-char (match-beginning 0))))


(vim:defmotion vim:motion-bwd-word-end (inclusive count)
  "Moves the cursor to the end of the previous word."            
  (let ((wordend (concat "[" vim:word "][^" vim:word "]"))
	(nowordend (concat "[^" vim:whitespace vim:word "][" vim:whitespace vim:word "]")))
    (unless (eobp) (forward-char))
    (re-search-backward (concat wordend "\\|" nowordend "\\|\\`") nil nil (or count 1))
    (goto-char (match-beginning 0))))


(vim:defmotion vim:motion-fwd-WORD (exclusive count)
  "Moves the cursor to beginning of the next WORD."
  (let ((WORD (concat "[^" vim:whitespace "]"))
	(noWORD (concat "[" vim:whitespace "]")))
    (dotimes (i (or count 1))
      (forward-char)
      (while
	  (not
	   (or (and (looking-back noWORD) (looking-at WORD))
	       (and (bolp) (eolp))
	       (eobp)))
	(forward-char))))
  
  ;; in operator-pending mode, if we reached the beginning of a new
  ;; line, go back to the end of the previous line
  (when (and (vim:operator-pending-mode-p)
	     (vim:looking-back "^[ \t]*")
	     (not (save-excursion
		    (forward-line -1)
		    (and (bolp) (eolp)))))
    (forward-line -1)
    (end-of-line)))


(vim:defmotion vim:motion-bwd-WORD (exclusive count)
  "Moves the cursor to beginning of the previous WORD."
  (let ((WORD (concat "[^" vim:whitespace "]"))
	(noWORD (concat "[" vim:whitespace "]")))
    (dotimes (i (or count 1))
      (backward-char)
      (while
	  (not
	   (or (and (looking-back noWORD) (looking-at WORD))
	       (and (bolp) (eolp))
	       (eobp)))
	(backward-char)))))


(vim:defmotion vim:motion-fwd-WORD-end (inclusive count)
  "Moves the cursor to the end of the next WORD."            
  (let ((WORDend (concat "[^" vim:whitespace "][" vim:whitespace "]")))
    (forward-char)
    (re-search-forward (concat WORDend "\\|\\'") nil nil (or count 1))
    (goto-char (match-beginning 0))))


(vim:defmotion vim:motion-bwd-WORD-end (inclusive count)
  "Moves the cursor to the end of the next WORD."            
  (let ((WORDend (concat "[^" vim:whitespace "][" vim:whitespace "]")))
    (unless (eobp) (forward-char))
    (re-search-backward (concat WORDend "\\|\\'") nil nil (or count 1))
    (goto-char (match-beginning 0))))


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


(vim:defmotion vim:motion-find (inclusive count (argument:char arg))
  "Move the cursor to the next count'th occurrence of arg."
  (forward-char)
  (let ((case-fold-search nil))
    (unless (search-forward (char-to-string arg)
			    nil t (or count 1))
      (backward-char)
      (error (format "Can't find %c" arg)))
    (setq vim:last-find (cons 'vim:motion-find arg))
    (backward-char)))


(vim:defmotion vim:motion-find-back (exclusive count (argument:char arg))
  "Move the cursor to the previous count'th occurrence of arg."
  (let ((case-fold-search nil))
    (unless (search-backward (char-to-string arg)
			     nil t (or count 1))
      (error (format "Can't find %c" arg)))
    (setq vim:last-find (cons 'vim:motion-find-back arg))))


(vim:defmotion vim:motion-find-to (inclusive count (argument:char arg))
  "Move the cursor to the character before the next count'th\
   occurence of arg."
  (vim:motion-find :count count :argument arg)
  (backward-char)
  (setq vim:last-find (cons 'vim:motion-find-to arg)))


(vim:defmotion vim:motion-find-back-to (exclusive count (argument:char arg))
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


(vim:defmotion vim:motion-mark (exclusive (argument:char mark-char))
  "Moves to the position of `mark-char'."
  (goto-char (vim:get-local-mark mark-char)))

(vim:defmotion vim:motion-mark-line (linewise (argument:char mark-char))
  "Moves to the first non-blank char in the line of `mark-char'."
  (goto-char (vim:get-local-mark mark-char))
  (vim:motion-first-non-blank)
  t)

(provide 'vim-motions)

;;; vim-motions.el ends here
