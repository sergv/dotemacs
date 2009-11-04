;;; vim.el --- a VIM-emulation for Emacs

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.2.0
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

(defconst vim:visual-mode-keymap (vim:make-keymap vim:operator-pending-mode-keymap)
  "VIM visual-mode keymap.")
(defun vim:vmap (keys command)
  "Defines a new visual-mode mapping."
  (vim:map keys command :keymap vim:visual-mode-keymap))

(vim:define-mode visual "VIM visual mode"
                 :ident "V"
                 :keymap vim:visual-mode-keymap
                 :command-function 'vim:visual-mode-command
                 :cursor 'hollow
                 :activate 'vim:visual-mode-activate
                 :deactivate 'vim:visual-mode-deactivate)

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

;; Info-struct to save information for visual-insertion.
(defstruct (vim:visual-insert-info
            (:constructor vim:make-visual-insert-info))
  first-line
  last-line
  column)

;; The last motion used to insert something in visual mode.
(vim:deflocalvar vim:visual-last-insert-info nil)

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
  (if (vim:visual-mode-p)
      (progn
        (vim:visual-highlight-region)
        (case vim:visual-mode-type
          ('normal (message "-- VISUAL --"))
          ('linewise (message "-- VISUAL LINE --"))
          ('block (message "-- VISUAL BLOCK --"))
          (t (error "Unknown visual mode type: %s"
                    vim:visual-mode-type))))
    (vim:activate-visual-mode)))

(defun vim:visual-toggle-mode (type)
  "Switches to visual mode of certain type or deactivates the mode."
  (if (and (vim:visual-mode-p)
           (eq vim:visual-mode-type type)
           (vim:toplevel-execution))
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
  "Deactivates visual mode, returning to normal-mode."
  (vim:activate-normal-mode))


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


(defun vim:visual-mode-command (command)
  "Executes a command in visual mode."
  (case (vim:cmd-type command)
    ('simple (vim:visual-execute-command command))
    ('complex (vim:visual-execute-command command))
    ('special (error "no special so far"))
    (t (vim:visual-execute-motion command))))


(defun vim:visual-execute-command (command)
  "Called to execute a command is visual mode."
  
  ;; save the last region
  (setq vim:visual-last-begin (cons (line-number-at-pos (mark t))
                                    (save-excursion
                                      (goto-char (mark t))
                                      (current-column))))
  (setq vim:visual-last-end (cons (line-number-at-pos (point))
                                  (current-column)))

  (if (vim:cmd-motion-p command)
      (unwind-protect
          (let ((vim:last-undo buffer-undo-list))
            (if (vim:cmd-arg-p command)
                (vim:funcall-save-buffer (vim:cmd-function command)
                                         :motion (vim:visual-current-motion)
                                         :argument (read-char-exclusive))
              (vim:funcall-save-buffer (vim:cmd-function command)
                                       :motion (vim:visual-current-motion)))
            (when (vim:cmd-repeatable-p command)
              (setq vim:repeat-events (vconcat vim:current-key-sequence)))

            (vim:connect-undos vim:last-undo))
        (vim:reset-key-state)
        (vim:clear-key-sequence)
        (vim:adjust-point))
    (vim:normal-execute-simple-command command))

  ;; deactivate visual mode unless the command should keep it
  (when (and vim:visual-mode
             (not (vim:cmd-keep-visual-p command)))
    (vim:visual-mode-exit)))
  

(defun vim:visual-execute-motion (command)
  "Called to execute a motion in visual mode."
  
  (setq vim:current-motion command)

  (when current-prefix-arg
    (setq vim:current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (vim:cmd-arg-p command)
    (setq vim:current-motion-arg (read-char-exclusive)))

  (unwind-protect
     (vim:visual-adjust-region (vim:execute-current-motion))
    
    (vim:adjust-point)
    (vim:clear-key-sequence)
    (vim:reset-key-state)))
  

(defun vim:visual-post-command ()
  (cond
   ((vim:visual-mode-p)
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
  (vim:make-motion :has-begin t
                   :begin (min (point) (mark t))
                   :end (max (point) (mark t))
                   :type 'inclusive))


(defun vim:visual-current-linewise-motion ()
  "Returns a motion representing the current linewise region."
  (vim:make-motion :has-begin t
                   :begin (min (point) (mark t))
                   :end (max (point) (mark t))
                   :type 'linewise))


(defun vim:visual-current-block-motion ()
  "Returns a motion representing the current block region."
  (vim:make-motion :has-begin t
                   :begin (min (point) (mark t))
		   :end (max (point) (mark t))
		   :type 'block))


(defun vim:visual-adjust-region (motion)
  "Adjusts the region according to a certain motion."
  (if (vim:motion-has-begin motion)
      (progn
        (case (vim:motion-type motion)
          ('linewise (vim:activate-visual 'linewise))
          ('block (vim:activate-visual 'block))
          (t (vim:activate-visual 'normal)))
        (if (< (point) (mark t))
            ;; increase backward
            (progn
              (goto-char (vim:motion-begin-pos motion))
              (when (> (vim:motion-end-pos motion) (mark t))
                (set-mark (vim:motion-end-pos motion))))
          ;; increase forward
          (when (< (vim:motion-begin motion)
                          (mark t))
            (set-mark (vim:motion-begin motion)))))))
      

(vim:defcmd vim:visual-insert (motion)
  "Starts insertion at the left column of a visual region."

  (vim:visual-start-insert 
   (vim:make-visual-insert-info :first-line (vim:motion-first-line motion)
                                :last-line (vim:motion-last-line motion)
                                :column (vim:motion-first-col motion))))

(defun vim:visual-start-insert (insert-info)
  "Starts a new multi-line insert operation with `insert-info'."
  
  (setq vim:visual-last-insert-info insert-info)
  (goto-line (vim:visual-insert-info-first-line insert-info))
  (move-to-column (vim:visual-insert-info-column insert-info) t)
  
  (case vim:visual-mode-type
    ('block
     ;; TODO: ensure the right command is run on repetition.
     ;; this is really a dirty hack
     (setq vim:current-key-sequence "i")
     (vim:cmd-insert :count 1)
     (add-hook 'vim:normal-mode-on-hook 'vim:insert-block-copies))

    ('linewise
     ;; TODO: ensure the right command is run on repetition.
     ;; this is really a dirty hack
     (setq vim:current-key-sequence "I")
     (vim:cmd-Insert :count 1)
     (add-hook 'vim:normal-mode-on-hook 'vim:insert-linewise-copies)))

  (setq vim:visual-last-insert-undo vim:last-insert-undo))

(defun vim:insert-block-copies ()
  "Called to repeat the last block-insert."
  (remove-hook 'vim:normal-mode-on-hook 'vim:insert-block-copies)
  (let ((begrow (vim:visual-insert-info-first-line vim:visual-last-insert-info))
        (begcol (vim:visual-insert-info-column vim:visual-last-insert-info))
        (endrow (vim:visual-insert-info-last-line vim:visual-last-insert-info)))
    (save-excursion
      (dotimes (i (- endrow begrow))
            (goto-line (+ begrow i 1))
        (when (>= (save-excursion
                    (end-of-line)
                    (current-column))
                  begcol)
          (move-to-column begcol t)
          (vim:cmd-repeat)))
      (setq vim:last-undo vim:visual-last-insert-undo))))


(defun vim:insert-linewise-copies ()
  "Called to repeat the last linewise-insert."
  (remove-hook 'vim:normal-mode-on-hook 'vim:insert-linewise-copies)
  (let ((begrow (vim:visual-insert-info-first-line vim:visual-last-insert-info))
        (endrow (vim:visual-insert-info-last-line vim:visual-last-insert-info)))
    (save-excursion
      (goto-line (1+ begrow))
      (dotimes (i (- endrow begrow))
        (goto-line (+ begrow i 1))
        (vim:cmd-repeat))
      (setq vim:last-undo vim:visual-last-insert-undo))))


(vim:defcmd vim:visual-append (motion)
  "Starts insertion at the right column of a visual block."
  
  (vim:visual-start-append
   (vim:make-visual-insert-info :first-line (vim:motion-first-line motion)
                                :last-line (vim:motion-last-line motion)
                                :column (vim:motion-last-col motion))))

  
(defun vim:visual-start-append (insert-info)
  "Starts a new multi-line append operation with `insert-info'."
  
  (setq vim:visual-last-insert-info insert-info)
  (goto-line (vim:visual-insert-info-first-line insert-info))
  (move-to-column (vim:visual-insert-info-column insert-info) t)
  
  (case vim:visual-mode-type
    ('block
     ;; TODO: ensure the right command is run on repeation.
     ;; this is really a dirty hack
     (setq vim:current-key-sequence "a")
     (vim:cmd-append :count 1)
     (add-hook 'vim:normal-mode-on-hook 'vim:append-block-copies))

    ('linewise
     ;; TODO: ensure the right command is run on repeation.
     ;; this is really a dirty hack
     (setq vim:current-key-sequence "A")
     (vim:cmd-Append :count 1)
     (add-hook 'vim:normal-mode-on-hook 'vim:insert-linewise-copies)))

  (setq vim:visual-last-insert-undo vim:last-insert-undo))


(defun vim:append-block-copies ()
  "Called to repeat the last block-insert."
  (remove-hook 'vim:normal-mode-on-hook 'vim:append-block-copies)
  (let ((begrow (vim:visual-insert-info-first-line vim:visual-last-insert-info))
        (endcol (vim:visual-insert-info-column vim:visual-last-insert-info))
        (endrow (vim:visual-insert-info-last-line vim:visual-last-insert-info)))
    (save-excursion
      (goto-line (1+ begrow))
      (dotimes (i (- endrow begrow))
        (move-to-column (1+ endcol) t) ; extend the newline at the end
        (move-to-column endcol t)
        (vim:cmd-repeat)
        (forward-line 1))
      (setq vim:last-undo vim:visual-last-insert-undo))))


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
