;;; vim.el --- a VIM-emulation for Emacs

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"
;; Compatibility: Developed on Emacs 22 - everything else is unknown.
;;
;; URL: http://www.tu-chemnitz.de/~fifr/repos/vim-mode.el
;; 
;; This file is not part of GNU Emacs.

;; TODO:

;;   - when calling a non-vim-mode-command the region should be modified
;;     s.t. the emacs command uses the correct region.


(provide 'vim-visual-mode)

(defcustom vim:visual-region-face 'region
  "Face of the highlighted region."
  :type 'face
  :group 'vim-mode)

(defcustom vim:visual-mode-cursor 'hollow
  "The cursor-type for visual-mode."
  :group 'vim-mode)

(defconst vim:visual-mode-keymap
  (vim:make-node :next-keymap vim:motion-keymap))

(vim:deflocalvar vim:visual-mode-type 'normal
  "Type of visual mode, should be 'normal, 'linewise or 'block.")

;; Last transient-mark-mode.
(vim:deflocalvar vim:visual-old-transient-mark-mode)

;; Old global variables changed in visual-mode.
(vim:deflocalvar vim:visual-old-global-variables)

;; List of overlays for the current region.
(vim:deflocalvar vim:visual-overlays)

;; The last beginning position of the region.
(vim:deflocalvar vim:visual-last-begin)

;; The last end position of the region.
(vim:deflocalvar vim:visual-last-end)

;; The last motion used to insert something in visual mode.
(vim:deflocalvar vim:visual-last-insert-motion nil)

;; The undo-mark of the last visual mode insert command.
(vim:deflocalvar vim:visual-last-insert-undo nil)

;; If non-nil, the last region will be used when visual mode is activated.
(vim:deflocalvar vim:visual-reactivate-last-region nil)

;;; System variables which must temporarily be buffer local.
(defconst vim:visual-temporary-local-variables
  '(transient-mark-mode
    deactivate-mark-hook))

;;; Commands the deactivate the mark (and so visual-mode).
(defconst vim:visual-deactivate-mark-commands
  '(clear-rectangle
    copy-rectangle
    copy-rectangle-to-register
    kill-rectangle
    open-rectangle
    string-rectangle
    yank-rectangle
    keyboard-quit))


(defun vim:activate-visual (type)
  "Activates visual-mode with certain type."
  (setq vim:visual-mode-type type)
  (if (eq vim:active-mode vim:visual-mode)
      (vim:visual-highlight-region)
    (vim:activate-mode vim:visual-mode)))

(defun vim:visual-toggle-mode (type)
  "Switches to visual mode of certain type or deactivates the mode."
  (if (and (eq vim:active-mode vim:visual-mode)
           (eq vim:visual-mode-type type))
      (vim:visual-mode-exit)
    (vim:activate-visual type)))
  

(vim:defcmd vim:visual-toggle-normal (nonrepeatable keep-visual)
    "Switches to normal visual-mode or deactivates it."
    (vim:visual-toggle-mode 'normal))


(vim:defcmd vim:visual-toggle-linewise (nonrepeatable keep-visual)
    "Switches to linewise visual-mode or deactivates it."
    (vim:visual-toggle-mode 'linewise))


(vim:defcmd vim:visual-toggle-block (nonrepeatable keep-visual)
    "Switches to block visual-mode or deactivates it."
    (vim:visual-toggle-mode 'block))


(vim:defcmd vim:visual-mode-exit (nonrepeatable)
  "Deactivates visual mode."
  (vim:activate-mode vim:normal-mode))


(vim:defcmd vim:visual-mode-reactivate (nonrepeatable)
  "Called when the last visual region should be reactivated."
  (unless (and vim:visual-mode-type
               vim:visual-last-begin
               vim:visual-last-end)
    (error "No former visual selection."))

  (setq vim:visual-reactivate-last-region t)
  (vim:activate-visual vim:visual-mode-type))


(defun vim:visual-mode-activate ()
  "Called when visual mode is activated."

  (setq cursor-type vim:visual-mode-cursor)
  
  (if vim:visual-reactivate-last-region
      (progn
        (set-mark (save-excursion
                    (goto-line (car vim:visual-last-begin))
                    (move-to-column (cdr vim:visual-last-begin))
                    (point)))
        (goto-line (car vim:visual-last-end))
        (move-to-column (cdr vim:visual-last-end))
        (setq vim:visual-reactivate-last-region nil))
    (set-mark (point)))
  
  (case vim:visual-mode-type
    ('normal (message "-- VISUAL --"))
    ('linewise (message "-- VISUAL LINE --"))
    ('block (message "-- VISUAL BLOCK --"))
    (t (error "Unknown visual mode type: %s"
              vim:visual-mode-type)))

  (setq vim:visual-overlays nil
        vim:visual-old-transient-mark-mode (and (boundp 'transient-mark-mode)
                                                transient-mark-mode)
        vim:visual-old-global-variables
        ;; Remember which system variables weren't buffer local
        (mapcan #'(lambda (variable)
                    (if (assoc variable (buffer-local-variables))
                        (list variable)
                      nil))
                vim:visual-temporary-local-variables))
  
  ;; The make them all buffer local, too.
  (mapcar #'make-local-variable vim:visual-temporary-local-variables)
  (when (boundp 'transient-mark-mode) (setq transient-mark-mode nil))
  (add-hook 'post-command-hook 'vim:visual-post-command)
  (add-hook 'deactivate-mark-hook 'vim:visual-mode-exit))


(defun vim:visual-mode-deactivate ()
  "Called when visual mode is deactivated."

  ;; hide the selection
  (vim:visual-hide-region)
  
  ;; cleanup local variables
  (setq deactivate-mark-hook (delq 'vim:visual-mode-exit deactivate-mark-hook))
  (setq post-command-hook (delq 'vim:visual-post-command post-command-hook))
  (when (boundp 'transient-mark-mode)
    (setq transient-mark-mode vim:visual-old-transient-mark-mode))
  (vim:visual-delete-overlays vim:visual-overlays)
  (mapcar #'kill-local-variable vim:visual-old-global-variables)
  (deactivate-mark))


(defun vim:visual-post-command ()
  (cond
   ((eq vim:active-mode vim:visual-mode)
    (if (memq this-command vim:visual-deactivate-mark-commands)
        (condition-case nil
            (vim:visual-mode-exit)
          (error nil))
      (condition-case info
          (vim:visual-highlight-region)
        (error
         (ding)
         (message "visual-mode trouble: %s" info)
         (condition-case nil
             (vim:visual-mode-exit)
           (error nil))))))))


(defun vim:visual-highlight-region ()
  "Highlights the selected region depending on `point' and `mark'."

  (let ((start (min (point) (mark t)))
        (end (max (point) (mark t))))
    (case vim:visual-mode-type
      ('normal (vim:visual-highlight-normal start end))
      ('linewise (vim:visual-highlight-linewise start end))
      ('block (vim:visual-highlight-block start end))
      (t (error "Unknown visual mode %s" vim:visual-mode-type)))))



(defun vim:visual-highlight-normal (start end)
  "Adjusts the normal region between `start' and `end'."
  (if vim:visual-overlays
      (vim:visual-delete-overlays (cdr vim:visual-overlays))
    (setq vim:visual-overlays nil))
  
  (setq vim:visual-overlays
        (list (vim:visual-create-or-update-overlay (car vim:visual-overlays)
                                                   start (1+ end)))))


(defun vim:visual-highlight-linewise (start end)
  "Adjusts the linewise region between `start' and `end'."
  (let ((start-line (save-excursion
                      (goto-char start)
                      (line-beginning-position)))
        (end-line (save-excursion
                    (goto-char end)
                    (line-end-position))))
    
    (if vim:visual-overlays
        (vim:visual-delete-overlays (cdr vim:visual-overlays))
      (setq vim:visual-overlays nil))
    
    (setq vim:visual-overlays
          (list (vim:visual-create-or-update-overlay (car vim:visual-overlays)
                                                     start-line
                                                     (1+ end-line))))))


(defun vim:visual-highlight-block (start end)
  "Adjusts the block region between `start' and `end'."
  ;; Adapted from: rm-highlight-rectangle
  ;; This function is used to highlight the rectangular region from
  ;; START to END.  We do this by putting an overlay on each line
  ;; within the rectangle.  Each overlay extends across all the
  ;; columns of the rectangle.  We try to reuse overlays where
  ;; possible because this is more efficient and results in less
  ;; flicker.
  (save-excursion
    ;; Calculate the rectangular region represented by point and mark,
    ;; putting start in the north-west corner and end in the
    ;; south-east corner.
    (let ((start-col (save-excursion
		       (goto-char start)
		       (current-column)))
	  (end-col (save-excursion
		     (goto-char end)
		     (current-column))))
      (if (> start-col end-col)
	  (setq start-col (prog1
			      end-col
			    (setq end-col start-col))
		start (save-excursion
			(goto-char start)
			(move-to-column start-col nil)
			(point))
		end (save-excursion
		      (goto-char end)
		      (move-to-column end-col nil)
		      (point))))
      ;; Force a redisplay so we can do reliable window start/end
      ;; calculations.
      (sit-for 0)
      (let* ((old vim:visual-overlays)
	     (new nil)
	     overlay
	     (window-start (max (window-start) start))
	     (window-end (min (window-end) end))
	     (nlines (count-lines window-start
				  (min (1+ window-end)
				       (point-max)))))
	;; Iterate over those lines of the rectangle which are visible
	;; in the currently selected window.
	(goto-char window-start)
	(dotimes (i nlines)
	  ;(while  (< (point) window-end)
	  (let ((row-start (progn
			     (move-to-column start-col nil)
			     (point)))
		(row-end (progn
			   (move-to-column end-col nil)
			   (min (1+ (point))
				(line-end-position)))))
	    ;; Trim old leading overlays.
	    (while (and old
			(setq overlay (car old))
			(< (overlay-start overlay) row-start)
			(/= (overlay-end overlay) row-end))
	      (delete-overlay overlay)
	      (setq old (cdr old)))
	    ;; Reuse an overlay if possible, otherwise create one.
	    (if (and old
		     (setq overlay (car old))
		     (or (= (overlay-start overlay) row-start)
			 (= (overlay-end overlay) row-end)))
		(progn
		  (move-overlay overlay row-start row-end)
		  (setq new (cons overlay new)
			old (cdr old)))
	      (setq overlay (make-overlay row-start row-end))
	      (overlay-put overlay 'face vim:visual-region-face)
	      (overlay-put overlay 'priority 99)
	      (setq new (cons overlay new))))
	  (forward-line 1))
	;; Trim old trailing overlays.
	(vim:visual-delete-overlays old)
	(setq vim:visual-overlays (nreverse new))))))


(defun vim:visual-create-or-update-overlay (ov start end)
  "Creates a new overlay or updates the given overlay."
  (if (overlayp ov)
      (progn
        (vim:visual-delete-overlays (cdr vim:visual-overlays))
        (setcdr vim:visual-overlays nil)
        (move-overlay (car vim:visual-overlays) start  end)
        ov)
    (let ((ov (make-overlay start  end)))
      (vim:visual-hide-region)
      (overlay-put ov 'face vim:visual-region-face)
      (overlay-put ov 'priority 99)
      ov)))
    
  

(defun vim:visual-hide-region ()
  "Removes the highlighting."
  (vim:visual-delete-overlays vim:visual-overlays)
  (setq vim:visual-overlays nil))

(defun vim:visual-delete-overlays (overlays)
  "Deletes all overlays in `overlays'."
  (mapcar #'delete-overlay overlays))

(defun vim:visual-current-motion ()
  "Returns a motion representing the current region."
  (case vim:visual-mode-type
    ('normal (vim:visual-current-normal-motion))
    ('linewise (vim:visual-current-linewise-motion))
    ('block (vim:visual-current-block-motion))))

(defun vim:visual-current-normal-motion ()
  "Returns a motion representing the current normal region."
  (vim:make-motion :begin (min (point) (mark t))
                   :end (max (point) (mark t))
                   :type 'inclusive))


(defun vim:visual-current-linewise-motion ()
  "Returns a motion representing the current linewise region."
  (vim:make-motion :begin (line-number-at-pos (min (point) (mark t)))
                   :end (line-number-at-pos (max (point) (mark t)))
                   :type 'linewise))


(defun vim:visual-current-block-motion ()
  "Returns a motion representing the current block region."
  (let ((row1 (line-number-at-pos (mark t)))
        (col1 (save-excursion (goto-char (mark t)) (current-column)))
        (row2 (line-number-at-pos (point)))
        (col2 (current-column)))
    (vim:make-motion :begin (cons (min row1 row2)      
                                  (min col1 col2))
                     :end (cons (max row1 row2)
                                (max col1 col2))
                     :type 'block)))


(defun vim:visual-adjust-region (motion)
  "Adjusts the region according to a certain motion."
  (if (vim:motion-begin motion)
      (progn
        (case (vim:motion-type motion)
          ('linewise (vim:activate-visual 'linewise))
          ('block (vim:activate-visual 'block))
          (t (vim:activate-visual 'normal)))
        (if (= (point) (mark t))
            (progn
              (set-mark (vim:motion-begin-pos motion))
              (goto-char (vim:motion-end-pos motion)))
          (goto-char (if (< (point) (mark t))
                         (vim:motion-begin-pos motion)
                       (vim:motion-end-pos motion)))))
    (vim:default-mode-exec-motion motion)))


(defun vim:visual-mode-exec-cmd (cmd count motion arg)
  "Called to execute a command is visual mode."
  
  ;; save the last region
  (setq vim:visual-last-begin (cons (line-number-at-pos (mark t))
                                    (save-excursion
                                      (goto-char (mark t))
                                      (current-column))))
  (setq vim:visual-last-end (cons (line-number-at-pos (point))
                                  (current-column)))

  (if (vim:cmd-motion-p cmd)
      (vim:default-mode-exec-cmd cmd
                                 count 
                                 (vim:visual-current-motion)
                                 arg)
    (vim:default-mode-exec-cmd cmd count motion arg))
  (when (and (eq vim:active-mode vim:visual-mode)
             (not (vim:cmd-keep-visual-p cmd)))
    (vim:visual-mode-exit)))

(defun vim:visual-mode-exec-motion (motion)
  "Called to execute a motion in visual mode."
  (vim:visual-adjust-region motion))

(defconst vim:visual-mode
  (vim:make-mode :name "Visual"
                 :id "V"
                 :activate #'vim:visual-mode-activate
                 :deactivate #'vim:visual-mode-deactivate
                 :execute-command #'vim:visual-mode-exec-cmd
                 :execute-motion #'vim:visual-mode-exec-motion
                 :keymap 'vim:visual-mode-keymap
                 :default-handler 'vim:default-default-handler))


(vim:defcmd vim:visual-insert (motion nonrepeatable)
  "Starts insertion at the left column of a visual region."
  
  (case vim:visual-mode-type
    ('block
     ;; TODO: ensure the right command is run on repeation.
     ;; this is really a dirty hack
     (setq vim:current-key-sequence (list ?i))
     (setq vim:visual-last-insert-motion motion)
     (goto-line (car (vim:motion-begin motion)))
     (move-to-column (cdr (vim:motion-begin motion)) t)
     (vim:cmd-insert 1)
     (add-hook 'vim:insert-mode-deactivate-hook
               'vim:insert-block-copies))

    ('linewise
     ;; TODO: ensure the right command is run on repeation.
     ;; this is really a dirty hack
     (setq vim:current-key-sequence (list ?I))
     (setq vim:visual-last-insert-motion
           (vim:make-motion :begin (vim:motion-begin-row motion)
                            :end (vim:motion-end-row motion)
                            :type 'linewise))
     (goto-char (vim:motion-begin-pos motion))
     (vim:cmd-Insert 1)
     (add-hook 'vim:insert-mode-deactivate-hook
               'vim:insert-linewise-copies)))

  (setq vim:visual-last-insert-undo vim:last-insert-undo))


(defun vim:insert-block-copies ()
  "Called to repeat the last block-insert."
  (remove-hook 'vim:insert-mode-deactivate-hook 'vim:insert-block-copies)
  (let ((begrow (car (vim:motion-begin vim:visual-last-insert-motion)))
        (begcol (cdr (vim:motion-begin vim:visual-last-insert-motion)))
        (endrow (car (vim:motion-end vim:visual-last-insert-motion))))
    (save-excursion
      (dotimes (i (- endrow begrow))
        (goto-line (+ begrow i 1))
        (when (>= (save-excursion
                    (end-of-line)
                    (current-column))
                  begcol)
          (move-to-column begcol t)
          (vim:cmd-repeat)))
      (vim:connect-undos vim:visual-last-insert-undo))))


(defun vim:insert-linewise-copies ()
  "Called to repeat the last linewise-insert."
  (remove-hook 'vim:insert-mode-deactivate-hook 'vim:insert-linewise-copies)
  (let ((begrow (vim:motion-begin-row vim:visual-last-insert-motion))
        (endrow (vim:motion-end-row vim:visual-last-insert-motion)))
    (save-excursion
      (goto-line (1+ begrow))
      (dotimes (i (- endrow begrow))
        (goto-line (+ begrow i 1))
        (vim:cmd-repeat))
      (vim:connect-undos vim:visual-last-insert-undo))))


(vim:defcmd vim:visual-append (motion nonrepeatable)
  "Starts insertion at the right column of a visual block."
  
  (case vim:visual-mode-type
    ('block
     ;; TODO: ensure the right command is run on repeation.
     ;; this is really a dirty hack
     (setq vim:current-key-sequence (list ?a))
     (setq vim:visual-last-insert-motion motion)
     (goto-line (car (vim:motion-begin motion)))
     (move-to-column (cdr (vim:motion-end motion)) t)
     (vim:cmd-append 1)
     (add-hook 'vim:insert-mode-deactivate-hook
               'vim:append-block-copies))

    ('linewise
     ;; TODO: ensure the right command is run on repeation.
     ;; this is really a dirty hack
     (setq vim:current-key-sequence (list ?A))
     (setq vim:visual-last-insert-motion
           (vim:make-motion :begin (vim:motion-begin-row motion)
                            :end (vim:motion-end-row motion)
                            :type 'linewise))
     (goto-char (vim:motion-begin-pos motion))
     (vim:cmd-Append 1)
     (add-hook 'vim:insert-mode-deactivate-hook
               'vim:insert-linewise-copies)))

  (setq vim:visual-last-insert-undo vim:last-insert-undo))


(defun vim:append-block-copies ()
  "Called to repeat the last block-insert."
  (remove-hook 'vim:insert-mode-deactivate-hook 'vim:append-block-copies)
  (let ((begrow (car (vim:motion-begin vim:visual-last-insert-motion)))
        (endrow (car (vim:motion-end vim:visual-last-insert-motion)))
        (endcol (cdr (vim:motion-end vim:visual-last-insert-motion))))
    (message "OK")
    (save-excursion
      (goto-line (1+ begrow))
      (dotimes (i (- endrow begrow))
        (move-to-column (1+ endcol) t) ; extend the newline at the end
        (move-to-column endcol t)
        (vim:cmd-repeat)
        (forward-line 1))
      (vim:connect-undos vim:visual-last-insert-undo))))


(vim:defcmd vim:visual-exchange-point-and-mark (nonrepeatable keep-visual)
   "Exchanges point and mark."
   (exchange-point-and-mark))


(vim:defcmd vim:visual-jump-point (nonrepeatable keep-visual)
  "In normal and linewise visual mode, this is the same as
`vim:visual-exchange-point-and-mark'.  In block visual-mode the
cursor jumps to the other corner of the selected region in the
current line."
  (case vim:visual-mode-type
    ((normal linewise)
     (vim:visual-exchange-point-and-mark))
    ('block
	(let ((mark-col (save-excursion
			  (goto-char (mark t))
			  (current-column)))
	      (point-col (current-column)))
	  (set-mark (save-excursion
		      (goto-char (mark t))
		      (move-to-column point-col t)
		      (point)))
	  (move-to-column mark-col t)))
    (t (error "Not in visual mode."))))
