;;; vim-search.el - Search und substitute commands for ex-mode.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;; TODO:
;;
;;  - searching currently uses isearch.  Although this is quite powerful,
;;    it's only usuably as interactive search and difficult to use with
;;    semi-interactive stuff like the "*" command.  The current implementation
;;    using unread-command-events is quite ugly.
;;  - the substitute command should be more interactive and especially an operation
;;    without the 'g' option should highlight all future occurences

;;; Code:

(defcustom vim:interactive-search-highlight 'all-windows
  "Determine in which windows the interactive highlighting should be shown."
  :type '(radio (const :tag "All windows." all-windows)
		(const :tag "Selected window." selected-window)
		(const :tag "Disable highlighting." nil))
  :group 'vim-mode)

(defconst vim:search-keymap (make-sparse-keymap))
(vim:set-keymap-default-binding vim:search-keymap 'vim:search-mode-exit)

(vim:deflocalvar vim:search-last-direction nil
  "The last search direction, either 'forward or 'backward.")

(defun vim:search-mode-activate ()
  (setq cursor-type vim:normal-mode-cursor))

(defun vim:search-mode-deactivate ()
  (isearch-exit))

(vim:defcmd vim:search-mode-exit ()
  (vim:activate-normal-mode)
  (push last-command-event unread-command-events))

;; Search mode is a very special mode being activated during a search
;; command.  Its purpose is to disable highlighting of search results
;; if something else than a repeat-search event occurs.
(vim:define-mode search "VIM search mode"
                 :ident "S"
                 :keymaps '(vim:search-keymap)
                 :command-function 'vim:search-mode-command)
(add-hook 'vim:search-mode-on-hook 'vim:search-mode-activate)
(add-hook 'vim:search-mode-off-hook 'vim:search-mode-activate)

(defun vim:search-mode-command (command)
  "Executes a simple-command in search-mode."
  (case (vim:cmd-type command)
    ('simple (vim:normal-execute-simple-command command))
    (t (error "Only simple commands allowed in search-mode."))))

(vim:defcmd vim:search-start (nonrepeatable)
  "Starts an incremental regexp search."
  (let ((search-nonincremental-instead nil))
    (ad-activate 'isearch-message-prefix)
    (isearch-forward-regexp)
    (ad-deactivate 'isearch-message-prefix)
    (setq vim:last-search-direction (if isearch-forward 'forward 'backward))))

(vim:defcmd vim:search-start-backward (nonrepeatable)
  "Starts an incremental regexp search."
  (let ((search-nonincremental-instead nil))
    (ad-activate 'isearch-message-prefix)
    (isearch-backward-regexp)
    (ad-deactivate 'isearch-message-prefix)
    (setq vim:last-search-direction (if isearch-forward 'forward 'backward))))

(vim:defcmd vim:search-repeat (nonrepeatable)
  "Repeats the last incremental search."
  (unless (vim:search-mode-p)
    (vim:activate-search-mode))
  (ad-activate 'isearch-message-prefix)
  (isearch-repeat vim:last-search-direction)
  (ad-deactivate 'isearch-message-prefix))

(vim:defcmd vim:search-repeat-opposite (nonrepeatable)
  "Starts an incremental regexp search."
  (unless (vim:search-mode-p)
    (vim:activate-search-mode))
  (ad-activate 'isearch-message-prefix)
  (isearch-repeat (if (eq vim:last-search-direction 'forward) 'backward 'forward))
  (ad-deactivate 'isearch-message-prefix))

(defadvice isearch-message-prefix (after vim:isearch-message-prefix (&optional c-q-hack ellipsis nonincremental))
  "This advice changes the minibuffer indicator to '/' or '?'"
  (setq ad-return-value (if isearch-forward "/" "?")))


;; A pattern.
(defstruct (vim:pattern
	    (:constructor nil)
	    (:constructor vim:make-pattern
			  (&key ((:regex re))
				((:case-fold ca) nil)
				(whole-line t)
			   &aux (regex (vim:regex-without-case re))
			        (case-fold (vim:regex-case re ca)))))
  regex      ;; The pattern itself.
  case-fold  ;; The case for this pattern.
  whole-line ;; If non-nil the pattern matches the whole line,
	     ;; otherwise only the first occurrence.
  )

(defun vim:regex-without-case (re)
  "Returns the regular expression without all occurrences of \\c and \\C."
  (replace-regexp-in-string "\\\\[cC]" "" re t t))

(defun vim:regex-case (re default-case)
  "Returns the case as implied by \\c or \\C in regular expression `re'.
If \\c appears anywhere in the pattern, the pattern is case
insenstive, if \\C appears the pattern is case sensitive. Only
the first occurrence of \\c or \\C is used, all others are
ignored. If neither \\c nor \\C appears in the pattern, the
case specified by `default-case' is used. `default-case' should be either
'sensitive, 'insensitive or 'smart. In the latter case the pattern will be
case-sensitive if and only if it contains an upper-case letter, otherwise it
will be case-insensitive."
  (if (string-match "\\\\[cC]" re)
      (if (string= (match-string 0 re) "\\c")
	  'insensitive
	'sensitive)
    (case default-case
      ((sensitive insensitive) default-case)
      (smart (if (isearch-no-upper-case-p re t) 'insensitive 'sensitive))
      (t nil))))

;; The lazy-highlighting framework.
(vim:deflocalvar vim:active-highlights-alist nil
  "An alist of currently active highlights."
  )

(defstruct (vim:hl
            (:constructor vim:make-highlight))
  name	     ;; The name of this highlight.
  pattern    ;; The search pattern.
  face	     ;; The face for this highlights.
  window     ;; The window where this highlight has been started.
  beg        ;; The minimal position for the highlighting.
  end        ;; The maximal position for the highlighting.
  update-hook ;; Hook to be called when the lazy highlighting.
  match-hook ;; Hook to be called when a single lazy highlight pattern has been setup.
  overlays   ;; The currently active overlays.
  )

(defun* vim:make-hl (name &key
			  (face 'lazy-highlight)
			  (win (selected-window))
			  (beg nil)
			  (end nil)
			  (update-hook nil)
			  (match-hook nil))
  "Creates new highlighting object with a certain `name'."
  (when (assoc name vim:active-highlights-alist)
    (vim:delete-hl name))
  (when (null vim:active-highlights-alist)
    (add-hook 'window-scroll-functions #'vim:hl-update-highlights-scroll nil t)
    (add-hook 'window-size-change-functions #'vim:hl-update-highlights-resize nil))
  (push (cons name (vim:make-highlight :name name
				       :pattern nil
				       :face face
				       :overlays nil
				       :window win
				       :beg beg
				       :end end
				       :update-hook update-hook
				       :match-hook match-hook))
	vim:active-highlights-alist))


(defun vim:delete-hl (name)
  "Removes the highlighting object with a certain `name'."
  (let ((hl (cdr-safe (assoc name vim:active-highlights-alist))))
    (when hl
      (mapc #'delete-overlay (vim:hl-overlays hl))
      (setq vim:active-highlights-alist
	    (remove* name vim:active-highlights-alist :key #'car))
      (vim:hl-update-highlights))
    (when (null vim:active-highlights-alist)
      (remove-hook 'window-scroll-functions #'vim:hl-update-highlights-scroll t)
      (remove-hook 'window-size-change-functions #'vim:hl-update-highlights-resize))))


(defun vim:hl-active-p (name)
  "Returns t iff the highlight with a certain name is active."
  (and (assoc name vim:active-highlights-alist) t))


(defun vim:hl-change (name new-pattern)
  "Sets the regular expression of the highlighting object with
name `name' to `new-regex'."
  (let ((hl (cdr-safe (assoc name vim:active-highlights-alist))))
    (when hl
      (setf (vim:hl-pattern hl)
	    (if (zerop (length new-pattern))
		nil
	      new-pattern))
      (vim:hl-idle-update))))


(defun vim:hl-set-region (name beg end)
  (let ((hl (cdr-safe (assoc name vim:active-highlights-alist))))
    (when hl
      (setf (vim:hl-beg hl) beg
	    (vim:hl-end hl) end)
      (vim:hl-idle-update))))


(defun* vim:hl-update-highlights ()
  "Updates the overlays of all active highlights."
  (dolist (hl (mapcar #'cdr vim:active-highlights-alist))
    (let ((old-ovs (vim:hl-overlays hl))
	  new-ovs
	  (pattern (vim:hl-pattern hl))
	  (face (vim:hl-face hl))
	  (match-hook (vim:hl-match-hook hl))
	  result)
      (condition-case lossage
	  (progn
	    (when pattern
	      (dolist (win (if (eq vim:interactive-search-highlight 'all-windows)
			       (get-buffer-window-list (current-buffer) nil t)
			     (list (vim:hl-window hl))))
		(let ((begin (max (window-start win)
				  (or (vim:hl-beg hl) (point-min))))
		      (end (min (window-end win)
				(or (vim:hl-end hl) (point-max))))
		      last-line)
		  (when (< begin end)
		    (save-excursion
		      (goto-char begin)
		      ;; set the overlays for the current highlight, reusing old overlays
		      ;; (if possible)
		      (while (and (vim:search-find-next-pattern pattern)
				  (< (match-beginning 0) (match-end 0))
				  (<= (match-end 0) end))
			(when (or (vim:pattern-whole-line pattern)
				  (not (equal (line-number-at-pos (match-beginning 0)) last-line)))
			  (setq last-line (line-number-at-pos (match-beginning 0)))
			  (push (if old-ovs
				    (progn
				      (move-overlay (car old-ovs)
						    (match-beginning 0)
						    (match-end 0))
				      (overlay-put (car old-ovs) 'face face)
				      (pop old-ovs))
				  (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
				    (overlay-put ov 'face face)
				    (overlay-put ov 'vim:hl (vim:hl-name hl))
				    ov))
				new-ovs)
			  (when match-hook (funcall match-hook (car new-ovs)))
			  )))))))
	    
	    (mapc #'delete-overlay old-ovs)
	    (setf (vim:hl-overlays hl) new-ovs)
	    (setq result (when (and pattern (null new-ovs)) "No match")))
	
	(invalid-regexp
	 (setq result (cadr lossage)))
	
	(search-failed
	 (setq result (nth 2 lossage)))

	(error
	 (setq result (format "%s" lossage))))
      
      (when (vim:hl-update-hook hl)
	(funcall (vim:hl-update-hook hl) result)))))


(defvar vim:hl-update-timer nil
  "Time used for updating highlights.")


(defun vim:hl-idle-update () 
  "Triggers the timer to update the highlights in the current buffer."
  (when (and vim:interactive-search-highlight
	     vim:active-highlights-alist)
    (when vim:hl-update-timer
      (cancel-timer vim:hl-update-timer))
    (setq vim:hl-update-timer
	  (run-at-time 0.1 nil
		       #'vim:hl-do-update-highlight
		       (current-buffer)))))


(defun* vim:hl-do-update-highlight (&optional buffer)
  "Timer function, updating the highlights."
  (with-current-buffer buffer
    (vim:hl-update-highlights))
  (setq vim:hl-update-timer nil))


(defun vim:hl-update-highlights-scroll (win begin)
  "Update highlights after scrolling in some window."
  (with-current-buffer (window-buffer)
    (vim:hl-idle-update)))


(defun vim:hl-update-highlights-resize (frame)
  "Updates highlights after resizing a window."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
	(vim:hl-idle-update)))))


;; Search commands
(defun* vim:search-find-next-pattern (pattern &optional
					      (direction 'forward))
  "Looks for the next occurrence of pattern in a certain direction."
  (let ((case-fold-search (eq (vim:pattern-case-fold pattern) 'insensitive)))
    (case direction
      ('forward (re-search-forward (vim:pattern-regex pattern) nil t))
      ('backward (re-search-backward (vim:pattern-regex pattern) nil t))
      (t (error "Unknown search direction: %s" direction)))))
  

(defun vim:start-word-search (unbounded direction)
  (condition-case nil
      (goto-char (vim:motion-bwd-word-end :count 1))
    (error nil))
 
  (save-excursion
    (re-search-forward (concat "\\<[" vim:word "]+\\>")))
 
  (when (eq direction 'backward)
    (goto-char (1+ (match-end 0))))
  (let ((events (reverse (append (if (eq direction 'forward)
				     "/"
				   "?")
				 (if unbounded
				     (regexp-quote (match-string 0))
				   (concat "\\<" 
					   (regexp-quote (match-string 0))
					   "\\>"))
				 [return]
				 "n"
				 nil))))
    (while events
      (push (car events) unread-command-events)
      (setq events (cdr events)))))


(vim:defcmd vim:search-word (nonrepeatable)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search nil 'forward))
   
   
(vim:defcmd vim:search-word-backward (nonrepeatable)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search nil 'backward))
   
   
(vim:defcmd vim:search-unbounded-word (nonrepeatable)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search t 'forward))
   
   
(vim:defcmd vim:search-unbounded-word-backward (nonrepeatable)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search t 'backward))


;; Substitute
(vim:defcmd vim:cmd-substitute (motion argument nonrepeatable)
  "The VIM substitutde command: [range]s/pattern/replacement/flags"
  (multiple-value-bind (pattern replacement flags) (vim:parse-substitute argument)
    (lexical-let* ((pattern pattern)
                   (replacement replacement)
                   (first-line (if motion (vim:motion-first-line motion) (line-number-at-pos (point))))
                   (last-line (if motion (vim:motion-last-line motion) (line-number-at-pos (point))))
                   (whole-line (and flags (find ?g flags)))
                   (confirm (and flags (find ?c flags)))
                   (ignore-case (and flags (find ?i flags)))
                   (dont-ignore-case (and flags (find ?I flags)))
                   (case-fold-search (or (and case-fold-search
                                              (not dont-ignore-case))
                                         (and (not case-fold-search)
                                              ignore-case)))
                   (case-replace case-fold-search)
                   (last-point (point))
                   (overlay (make-overlay (point) (point)))
                   (next-line (line-number-at-pos (point)))
                   (nreplaced 0))
      
      (unwind-protect
          (if whole-line
              ;; this one is easy, just use the built in function
              (vim:perform-replace pattern replacement confirm t nil nil nil 
                                   (save-excursion
                                     (goto-line first-line)
                                     (line-beginning-position))
                                   (save-excursion
                                     (goto-line last-line)
                                     (line-end-position)))
            (if confirm
                (progn
                  ;; this one is more difficult, we have to do the
                  ;; highlighting and questioning on our own
                  (overlay-put overlay 'face
                               (if (facep 'isearch)
                                   'isearch 'region))
                  (map-y-or-n-p #'(lambda (x)
                                    (set-match-data x)
                                    (move-overlay overlay (match-beginning 0) (match-end 0))
                                    (concat "Query replacing " 
                                            (match-string 0) 
                                            " with "
                                            (vim:match-substitute-replacement replacement case-fold-search)
                                            ": "))
                                #'(lambda (x) 
                                    (set-match-data x) 
                                    (replace-match replacement case-fold-search) 
                                    (incf nreplaced)
                                    (setq last-point (point)))
                                #'(lambda ()
                                    (let ((end (save-excursion 
                                                 (goto-line last-line)
                                                 (line-end-position))))
                                      (goto-line next-line)
                                      (beginning-of-line)
                                      (when (and (> end (point))
                                                 (re-search-forward pattern end t nil))
                                        (setq last-point (point))
                                        (setq next-line (1+ (line-number-at-pos (point))))
                                        (match-data))))))
              
              ;; just replace the first occurences per line
              ;; without highlighting and asking
              (goto-line first-line)
              (beginning-of-line)
              (while (and (<= (line-number-at-pos (point)) last-line)
                          (re-search-forward pattern (save-excursion
                                                       (goto-line last-line)
                                                       (line-end-position))
                                             t nil))
                (incf nreplaced)
                (replace-match replacement)
                (setq last-point (point))
                (forward-line)
                (beginning-of-line)))

            (goto-char last-point)
            (if (= nreplaced 1)
                (message "Replaced 1 occurence")
              (message "Replaced %d occurences" nreplaced)))
           
        ;; clean-up the overlay
        (delete-overlay overlay)))))


(defun vim:parse-substitute (text)
  (when (string-match "\\`\\s-*/\\(\\(?:[^/]\\|\\\\.\\)+\\)/\\(\\(?:[^/]\\|\\\\.\\)*\\)\\(?:/\\([giIc]*\\)\\)?\\s-*\\'"
                      text)
    (let* ((pattern (match-string 1 text))
           (replacement (match-string 2 text))
           (flags (match-string 3 text))
           newrepl
           (idx 0) (n (length replacement)))

      ;; handle escaped chars
      (while (< idx n)
        (if (and (= (aref replacement idx) ?\\)
                 (< (1+ idx) n))
            (let ((c (aref replacement (1+ idx))))
              (case c
                (?n (push ?\n newrepl))
                (?t (push ?\t newrepl))
                (?r (push ?\r newrepl))
                ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\\)
                 (push ?\\ newrepl)
                 (push c newrepl))
                (t (push c newrepl)))
              (incf idx 2))
          (push (aref replacement idx) newrepl)
          (incf idx)))
      
      (values pattern (apply #'string (reverse newrepl)) flags))))

(provide 'vim-search)

;;; vim-search.el ends here
