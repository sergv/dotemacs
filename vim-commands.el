;;; vim-commands.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; Description:

;; In general there are two types of commands: those operating on a
;; motion and those not taking a motion.  Examples of the first one
;; are the vim-commands c, d, y, =, examples of the second one are dd,
;; D, p, x.
;;
;; An operation based on motions should always respect the motion
;; type, i.e. if the motion is character-wise, line-wise or block-wise
;; not.  Motions passed to commands will always be inclusive (and
;; never exlusive).  For example, the command dG has a line-wise motion
;; argument and should delete whole lines.
;;
;; Motions are passed via the vim:motion structure.  The
;; representation of the start and end-positions depends on the
;; motion-type:
;;
;;   - character-wise: each position is a buffer-offset,
;;   - line-wise: each position is a line-number,
;;   - block-wise: each position is a (row,column) pair.
;;
;; Furthermore, each operation should place (point) at the correct
;; position after the operation.
;;
;;
;; Operations are defined by the `vim:define' macro:
;;
;;  (vim:define my-command (count arg)
;;              :type 'simple
;;              :argument t
;;              :count t
;;              :repeatable t
;;
;;       ... code ...
;;  )
;;
;; The command-type `:type' should be one of `simple', `complex' or `special'.
;; Simple commands are commands not taking a motion-argument.  Complex commands
;; are operations with motion-argument.  Special-commands are called without
;; affecting the key-parse mode and can be used to implement special behaviour
;; on some keys (e.g. numeric counts are implemented using special commands).
;;
;; If `type' is complex, the first argument passed to the function is
;; the associated motion as a vim:motion structure.
;;
;; If `count' is non-nil, the command takes an optional count
;; argument.  In this case the first parameter passed to the function
;; is the count (may be nil if no count is given).  If the command is
;; `complex', no count-argument is possible since the command-count
;; has already been passed to the associated motion (i.e. the motion
;; has got the count (command-count * motion-count).  If `count' is
;; nil the paramter must be omitted.
;;
;; If `argument' is non-nil, the command takes an addition
;; key-argument.  In this case the last parameter passed to the
;; function is the corresponding event.  This event may be an
;; arbitrary Emacs-event so the function should check its type and
;; signal an error if the event is invalid.  If `argument' is nil
;; the parameter must be omitted.
;;
;; If `repeatable' is non-nil, the command can be repeated using the
;; '.' command.  Some commands like scrolling commands or the repeat-command
;; itself cannot be repeated and should set repeatable to nil.


(provide 'vim-commands)

(defcustom vim:shift-width 8
  "The number of columns for shifting commands like < or >."
  :type 'integer
  :group 'vim-mode)

(vim:define vim:cmd-insert (count)
            :type 'simple
  (vim:activate-mode vim:insert-mode))

(vim:define vim:cmd-append (count)
            :type 'simple
  (unless (eolp) (forward-char))
  (vim:activate-mode vim:insert-mode))

(vim:define vim:cmd-Insert (count)
            :type 'simple
  (goto-char (vim:motion-first-non-blank))
  (vim:cmd-insert count))

(vim:define vim:cmd-Append (count)
            :type 'simple
  (end-of-line)
  (vim:cmd-append count))



(vim:define vim:cmd-delete-line (count)
            :type 'simple
  "Deletes the next count lines."
  (vim:cmd-yank-line count)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-line (1- (or count 1)))
               (line-end-position))))
    (if (= beg (point-min))
        (if (= end (point-max))
            (erase-buffer)
          (delete-region beg (save-excursion
                               (goto-char end)
                               (forward-line)
                               (line-beginning-position))))
      (delete-region (save-excursion
                       (goto-char beg)
                       (forward-line -1)
                       (line-end-position))
                     end))
    (goto-char beg)
    (goto-char (vim:motion-first-non-blank))))


(vim:define vim:cmd-delete (motion)
            :type 'complex
  "Deletes the characters defined by motion."
  (case (vim:motion-type motion)
    ('linewise
     (goto-line (vim:motion-begin motion))
     (vim:cmd-delete-line (vim:motion-line-count motion)))

    ('block
     (vim:cmd-yank motion)
     (let ((beg (save-excursion
                  (goto-line (car (vim:motion-begin motion)))
                  (move-to-column (cdr (vim:motion-begin motion)) t)
                  (point)))
           (end (save-excursion
                  (goto-line (car (vim:motion-end motion)))
                  (move-to-column (1+ (cdr (vim:motion-end motion))) t)
                  (point))))
       (delete-rectangle beg end)
       (goto-char beg)))

    (t
      (kill-region (vim:motion-begin motion) (min (point-max) (1+ (vim:motion-end motion))))
      (goto-char (vim:motion-begin motion)))))


(vim:define vim:cmd-change (motion)
            :type 'complex
  "Deletes the characters defined by motion and goes to insert mode."
  (case (vim:motion-type motion)
    ('linewise
     (goto-line (vim:motion-begin motion))
     (vim:cmd-change-line (vim:motion-line-count motion)))

    ('block
     (vim:cmd-delete motion)
     (vim:visual-insert motion))

    (t
     ;; TODO: getting the node from vim:motion-keymap is dangerous if
     ;; someone changes the binding of e or E.  It would be better to
     ;; create a new dummy vim:node representing the motion!
     
     ;; deal with cw and cW
     (unless (member (char-after)
                     '(?  ?\r ?\n ?\t))
       (cond
        ((eq (vim:node-cmd vim:current-motion) 
             'vim:motion-fwd-word)
         (setq vim:current-motion (vim:get-subnode vim:motion-keymap ?e))
         (setq motion (vim:get-current-cmd-motion)))
        
        ((eq (vim:node-cmd vim:current-motion) 
             'vim:motion-fwd-WORD)
         (setq vim:current-motion (vim:get-subnode vim:motion-keymap ?E))
         (setq motion (vim:get-current-cmd-motion)))))
        
     (vim:cmd-delete motion)
     (if (eolp)
         (vim:cmd-append 1)
       (vim:cmd-insert 1)))))


(vim:define vim:cmd-change-line (count)
            :type 'simple
  "Deletes count lines and goes to insert mode."
  (let ((pos (line-beginning-position)))
    (vim:cmd-delete-line count)
    (if (< (point) pos)
        (progn
          (end-of-line)
          (newline))
      (progn
        (beginning-of-line)
        (newline)
        (forward-line -1)))
    (indent-according-to-mode)
    (if (eolp)
        (vim:cmd-append 1)
      (vim:cmd-insert 1))))


(vim:define vim:cmd-replace-char (count arg)
            :type 'simple
            :argument t
  "Replaces the next count characters with arg."
  (unless (integerp arg)
    (error "Expected a character."))
  (when (< (- (line-end-position) (point))
           (or count 1))
    (error "Too few characters to end of line."))
  (delete-region (point) (+ (point) (or count 1)))
  (insert-char arg (or count 1))
  (backward-char))


(vim:define vim:cmd-replace (count)
            :type 'simple
  "Goes to replace-mode."
  (vim:activate-mode vim:replace-mode))


(vim:define vim:cmd-replace-region (motion arg)
            :type 'complex
            :argument t
   "Replace the complete region with `arg'"
   (case (vim:motion-type motion)
     ('block
      ;; replace in block
      (let ((begrow (car (vim:motion-begin motion)))
            (begcol (cdr (vim:motion-begin motion)))
            (endrow (car (vim:motion-end motion)))
            (endcol (1+ (cdr (vim:motion-end motion)))))
        (goto-line begrow)
        (dotimes (i (1+ (- endrow begrow)))
          ;; TODO does it work with \r\n at the end?
          (let ((maxcol (save-excursion
                          (end-of-line)
                          (current-column))))
            (when (> maxcol begcol)
              (delete-region (save-excursion
                               (move-to-column begcol t)
                               (point))
                             (save-excursion
                               (move-to-column (min endcol maxcol) t)
                               (point)))
              (move-to-column begcol t)
              (insert-char arg (- (min endcol maxcol) begcol))))
          (forward-line 1))
        (goto-line begrow)
        (move-to-column begcol)))
       
     (t ;; replace in linewise and normal
      (let ((begrow (vim:motion-begin-row motion))
            (endrow (vim:motion-end-row motion)))
        (goto-line begrow)
        (do ((r begrow (1+ r)))
            ((> r endrow))
          (goto-line r)
          (let ((begcol
                 (if (and (= r begrow)
                          (not (eq (vim:motion-type motion) 'linewise)))
                     (save-excursion
                       (goto-char (vim:motion-begin motion))
                       (current-column))
                   0))
                (endcol
                 (if (and (= r endrow)
                          (not (eq (vim:motion-type motion) 'linewise)))
                     (save-excursion
                       (goto-char (vim:motion-end motion))
                       (1+ (current-column)))
                   ;; TODO does it work with \r\n at the end?
                   (save-excursion
                     (end-of-line)
                     (current-column)))))

              (delete-region (save-excursion
                               (move-to-column begcol t)
                               (point))
                             (save-excursion
                               (move-to-column endcol t)
                               (point)))
              (move-to-column begcol t)
              (insert-char arg (- endcol begcol)))))

        (goto-char (vim:motion-begin motion)))))


(vim:define vim:cmd-yank (motion)
            :type 'complex
            :repeatable nil
  "Saves the characters in motion into the kill-ring."
  (case (vim:motion-type motion)
    ('block (vim:cmd-yank-rectangle motion))
    ('linewise (goto-line (vim:motion-begin-row motion))
	       (vim:cmd-yank-line (vim:motion-line-count motion)))
    (t
     (kill-new (buffer-substring
                (vim:motion-begin-pos motion)
                (1+ (vim:motion-end-pos motion)))))))
  

(vim:define vim:cmd-yank-line (count)
            :type 'simple
            :repeatable nil
  "Saves the next count lines into the kill-ring."
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-line (1- (or count 1)))
               (line-end-position))))
    (kill-new (concat (buffer-substring beg end) "\n") nil)))


(defun vim:cmd-yank-rectangle (motion)
  "Stores the rectangle defined by motion into the kill-ring."
  (unless (eq (vim:motion-type motion) 'block)
    (error "Motion must be of type block"))
  ;; TODO: yanking should not insert spaces or expand tabs.
  (let ((begrow (car (vim:motion-begin motion)))
        (begcol (cdr (vim:motion-begin motion)))
        (endrow (car (vim:motion-end motion)))
        (endcol (cdr (vim:motion-end motion)))
        (parts nil))
    (goto-line endrow)
    (dotimes (i (1+ (- endrow begrow)))
      (let ((beg (save-excursion (move-to-column begcol t) (point)))
            (end (save-excursion (move-to-column (1+ endcol) t) (point))))
        (setq parts (cons "\n" (cons (buffer-substring beg end) parts)))
        (forward-line -1)))
    (kill-new (apply #'concat (cdr parts)) nil (list 'vim:yank-block-handler))
    (goto-line begrow)
    (move-to-column begcol)))


(defun vim:yank-block-handler (text)
  "Inserts the current text as block."
  (let ((parts (split-string text "\n"))
        (col (current-column)))
    (dolist (part parts)
      (insert part)
      (forward-line 1)
      (move-to-column col t))))
                    

(vim:define vim:cmd-paste-before (count)
            :type 'simple
  "Pastes the latest yanked text before the cursor position."
  (unless kill-ring-yank-pointer
    (error "kill-ring empty"))
  
  (let* ((txt (car kill-ring-yank-pointer))
         (yhandler (get-text-property 0 'yank-handler txt)))
    (cond
     (yhandler ; block or other strange things
      (save-excursion (yank))) 
     
     ((= (elt txt (1- (length txt))) ?\n) ; linewise
      (beginning-of-line)
      (save-excursion
        (dotimes (i (or count 1))
          (yank))))

     (t ; normal
      (dotimes (i (or count 1))
        (yank))
      (backward-char)))))


(vim:define vim:cmd-paste-behind (count)
            :type 'simple
  "Pastes the latest yanked text behind point."
  (unless kill-ring-yank-pointer
    (error "kill-ring empty"))
  
  (let* ((txt (car kill-ring-yank-pointer))
         (yhandler (get-text-property 0 'yank-handler txt)))

    (cond
     (yhandler ; block or other string things
      (forward-char)
      (save-excursion (yank)))

     ((= (elt txt (1- (length txt))) ?\n) ; linewise
      (let ((last-line (= (line-end-position) (point-max))))
        (if last-line
            (progn
              (end-of-line)
              (newline))
          (forward-line))
        (beginning-of-line)
        (save-excursion
          (dotimes (i (or count 1))
            (yank))
          (when last-line
            ;; remove the last newline
            (let ((del-pos (point)))
              (forward-line -1)
              (end-of-line)
              (delete-region (point) del-pos))))))

     (t ; normal
      (forward-char)
      (dotimes (i (or count 1))
        (yank))
      (backward-char)))))


(vim:define vim:cmd-join-lines (count)
	    :type 'simple
  "Join `count' lines with a minimum of two lines."
  (dotimes (i (max 1 (1- (or count 1))))
    (when (re-search-forward "\\(\\s-*\\)\\(\n\\s-*\\)\\()?\\)")
      (delete-region (match-beginning 2)
                     (match-end 2))
      (when (and (= (match-beginning 1) (match-end 1))
                 (= (match-beginning 3) (match-end 3)))
        (insert-char ?  1))
      (backward-char))))


(vim:define vim:cmd-join (motion)
	    :type 'complex
  "Join the lines covered by `motion'."
  (goto-line (vim:motion-begin-row motion))
  (vim:cmd-join-lines (vim:motion-line-count motion)))


(vim:define vim:cmd-indent (motion)
            :type 'complex
  "Reindent the lines covered by `motion'."
  (goto-line (vim:motion-begin-row motion))
  (vim:cmd-indent-lines (vim:motion-line-count motion)))
  

(vim:define vim:cmd-indent-lines (count)
            :type 'simple
  "Reindent the next `count' lines."
  (indent-region (line-beginning-position)
                 (line-end-position count)))
  

(vim:define vim:cmd-shift-left (motion)
            :type 'complex
  "Shift the lines covered by `motion' leftwards."
  (goto-line (vim:motion-begin-row motion))
  (vim:cmd-shift-left-lines (vim:motion-line-count motion)))


(vim:define vim:cmd-shift-left-lines (count)
            :type 'simple
  "Shift the next `count' lines leftwards."
  (indent-rigidly (line-beginning-position)
                  (line-end-position count)
                  (- vim:shift-width)))


(vim:define vim:cmd-shift-right (motion)
            :type 'complex
  "Shift the lines covered by `motion' rightwards."
  (goto-line (vim:motion-begin-row motion))
  (vim:cmd-shift-right-lines (vim:motion-line-count motion)))
  

(vim:define vim:cmd-shift-right-lines (count)
            :type 'simple
  "Shift the next `count' lines rightwards."
  (indent-rigidly (line-beginning-position)
                  (line-end-position count)
                  vim:shift-width))



(vim:define vim:cmd-repeat ()
            :type 'simple
            :repeatable nil
            :count nil
  "Repeats the last command."
  (unless vim:repeat-events
    (error "Nothing to repeat"))
  (vim:reset-key-state)
  (dotimes (i (or count 1))
    (let ((repeat-events vim:repeat-events)
          (vim:repeat-events nil))
      (execute-kbd-macro repeat-events)))
  (vim:reset-key-state))


(vim:define vim:cmd-emacs ()
            :type 'simple
            :repeatable nil
            :count nil
   "Switches to Emacs for the next command."
   (message "Switch to Emacs for the next command.")
   (vim:escape-to-emacs nil))
