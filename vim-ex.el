;;; vim-ex.el - Ex-mode.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:
(provide 'vim-ex)

(defvar vim:ex-commands nil
  "List of pairs (command . function).")

(defvar vim:ex-history nil
  "History of ex-commands.")

(defun vim:emap (keys command)
  "Maps an ex-command to some function."
  (let ((pair (find-if (lambda (x) (string= (car x) keys)) vim:ex-commands)))
    (if pair
        (setcdr pair command)
      (setq vim:ex-commands (acons keys command vim:ex-commands)))))

(defvar vim:ex-keymap (make-sparse-keymap)
  "Keymap used in ex-mode.")

(define-key vim:ex-keymap "\t" 'minibuffer-complete)
(define-key vim:ex-keymap [return] 'exit-minibuffer)
(define-key vim:ex-keymap (kbd "RET") 'exit-minibuffer)
(define-key vim:ex-keymap " " 'vim:ex-expect-argument)
(define-key vim:ex-keymap (kbd "C-j") 'vim:ex-execute-command)
(define-key vim:ex-keymap (kbd "C-g") 'abort-recursive-edit)
(define-key vim:ex-keymap [up] 'previous-history-element)
(define-key vim:ex-keymap [down] 'next-history-element)

(defvar vim:ex-keep-reading nil)
(defvar vim:ex-cmdline nil)
(defvar vim:ex-cmd nil)
(defvar vim:ex-beg nil)
(defvar vim:ex-end nil)

(defun vim:ex-split-cmdline (cmdline)
  (multiple-value-bind (cmd-region beg end) (vim:ex-parse cmdline)
    (if (null cmd-region)
        (values "" "" cmdline "" nil nil)
      (let ((range (substring cmdline 0 (car cmd-region)))
            (cmd (substring cmdline (car cmd-region) (cdr cmd-region)))
            (spaces "")
            (arg (substring cmdline (cdr cmd-region))))
    
        ;; skip whitespaces
        (when (string-match "\\`[[:space:]]*" arg)
          (setq spaces (match-string 0 arg)
                arg (substring arg (match-end 0))))
      
        (values range cmd spaces arg beg end)))))

(defun vim:ex-expect-argument (n)
  ;; called if the space separating the command from the argument has
  ;; been pressed
  (interactive "p")
  (let ((cmdline (buffer-substring 2 (point-max)))) 
    (self-insert-command n)
    (multiple-value-bind (range cmd spaces arg beg end) (vim:ex-split-cmdline cmdline)

      (when (and (= (point) (point-max))
                 (zerop (length spaces))
                 (zerop (length arg)))
        (while (stringp cmd)
          (setq cmd (cdr-safe (assoc cmd vim:ex-commands))))
        
        (if (null cmd) (ding)
          (let ((result (case (vim:cmd-arg cmd)
                          (file
                           (vim:ex-complete-file-argument nil nil nil))
                          (buffer
                           (vim:ex-complete-buffer-argument nil nil nil))
                          ((t)
                           (vim:ex-complete-text-argument nil nil nil)))))
            (when result (insert result))))))))
          
(defun vim:ex-complete (cmdline predicate flag)
  (multiple-value-bind (range cmd spaces arg beg end) (vim:ex-split-cmdline cmdline)
    (setq vim:ex-cmd cmd)

    (cond
     ;; only complete at the end of the command
     ((< (point) (point-max)) nil)
       
     ;; if at the end of a command, complete the command
     ((and (zerop (length spaces)) (zerop (length arg)))
      (let ((result (vim:ex-complete-command cmd predicate flag)))
        (cond
         ((null result) nil)
         ((eq t result) t)
         ((stringp result)
          (if flag result (concat range result)))
         ((listp result) (if flag result (map #'(lambda (x) (concat range x)) result)))
         (t (error "Completion returned unexpected value.")))))
              
     ;; otherwise complete the argument
     (t 
      (let ((result (vim:ex-complete-argument arg predicate flag)))
        (cond
         ((null result) nil)
         ((eq t result) t)
         ((stringp result) (if flag result (concat range cmd spaces result)))
         ((listp result) (if flag result (map #'(lambda (x) (concat range cmd spaces x)) result)))
         (t (error "Completion returned unexpected value."))))))))

        
(defun vim:ex-complete-command (cmd predicate flag)
  ;; completes the command
  (cond
   ((null flag) (try-completion cmd vim:ex-commands predicate))
   ((eq t flag) (all-completions cmd vim:ex-commands predicate))
   ((eq 'lambda flag) (test-completion cmd vim:ex-commands predicate))))

(defun vim:ex-complete-argument (arg predicate flag)
  ;; completes the argument
  (let ((cmd vim:ex-cmd))
    (while (stringp cmd)
      (setq cmd (cdr-safe (assoc cmd vim:ex-commands))))

    (if (null cmd) (ding)
      (case (vim:cmd-arg cmd)
        (file
         (vim:ex-complete-file-argument arg predicate flag))
        (buffer
         (vim:ex-complete-buffer-argument arg predicate flag))
        ((t)
         (vim:ex-complete-text-argument arg predicate flag))
        (t (ding))))))

(defun vim:ex-complete-file-argument (arg predicate flag)
  ;; completes a file-name
  (if (null arg)
      default-directory
    (let ((dir (or (file-name-directory arg)
                   (with-current-buffer vim:ex-current-buffer default-directory)))
          (fname (file-name-nondirectory arg)))
      (cond
       ((null dir) (ding))
       ((null flag)
        (let ((result (file-name-completion fname dir predicate)))
        (case result
          ((nil) nil)
          ((t) t)
          (t (concat dir result)))))
         
       ((eq t flag) 
        (file-name-all-completions fname dir))
     
       ((eq 'lambda flag)
        (eq (file-name-completion fname dir predicate) t))))))
      
(defun vim:ex-complete-buffer-argument (arg predicate flag)
  ;; completes a buffer name
  (when arg
    (let ((buffers (mapcar #'buffer-name (buffer-list t))))
      (cond
       ((null flag)
        (try-completion arg buffers predicate))
       ((eq t flag) 
        (all-completions arg buffers predicate))
       ((eq 'lambda flag)
        (test-completion arg buffers predicate))))))

(defun vim:ex-complete-text-argument (arg predicate flag)
  ;; completes an arbitrary text-argument
  (case flag
    ((nil) t)
    ((t) (list arg))
    ('lambda t)))

(defun vim:ex-execute-command (cmdline)
  (interactive)

  (multiple-value-bind (range cmd spaces arg beg end) (vim:ex-split-cmdline cmdline)
    (setq vim:ex-cmd cmd)
    
    (let ((cmd vim:ex-cmd)
          (motion (cond
                   ((and beg end)
                    (vim:make-motion :begin (save-excursion
                                              (goto-line beg)
                                              (line-beginning-position))
                                     :end (save-excursion
                                            (goto-line end)
                                            (line-beginning-position))
                                     :has-begin t
                                     :type 'linewise))
                   (beg
                    (vim:make-motion :begin (save-excursion
                                              (goto-line beg)
                                              (line-beginning-position))
                                     :end (save-excursion
                                            (goto-line beg)
                                            (line-beginning-position))
                                     :has-begin t
                                     :type 'linewise))))
          (count (and (not end) beg)))
      
      (while (stringp cmd)
        (setq cmd (cdr-safe (assoc cmd vim:ex-commands))))

      (when (zerop (length arg))
        (setq arg nil))

      (with-current-buffer vim:ex-current-buffer
        (if cmd
            (case (vim:cmd-type cmd)
              ('complex
               (if (vim:cmd-arg-p cmd)
                   (funcall cmd :motion motion :argument arg)
                 (funcall cmd :motion motion)))
              ('simple
               (when end
                 (error "Command does not take a range: %s" vim:ex-cmd))
               (if (vim:cmd-arg-p cmd)
                   (if (vim:cmd-count-p cmd)
                       (funcall cmd :count beg :argument arg)
                     (funcall cmd :argument arg))
                 (if (vim:cmd-count-p cmd)
                     (funcall cmd :count count)
                   (funcall cmd))))
              (t (error "Unexpected command-type bound to %s" vim:ex-cmd)))
          (ding))))))
    

;; parser for ex-commands
(defun vim:ex-parse (text)
  "Extracts the range-information from `text'.
Returns a list of up to three elements: (cmd beg end)"
  (let (begin
        (begin-off 0)
        sep
        end
        (end-off 0)
        (pos 0)
        (cmd nil))
    
    (multiple-value-bind (beg npos) (vim:ex-parse-address text pos)
      (when npos
        (setq begin beg
              pos npos)))

    (multiple-value-bind (off npos) (vim:ex-parse-offset text pos)
      (when npos
        (unless begin (setq begin 'current-line))
        (setq begin-off off
              pos npos)))

    (when (and (< pos (length text))
               (or (= (aref text pos) ?\,)
                   (= (aref text pos) ?\;)))
      (setq sep (aref text pos))
      (incf pos)
      (multiple-value-bind (e npos) (vim:ex-parse-address text pos)
        (when npos
          (setq end e
          pos npos)))
      
      (multiple-value-bind (off npos) (vim:ex-parse-offset text pos)
        (when npos
          (unless end (setq end 'current-line))
          (setq end-off off
          pos npos))))

    ;; handle the special '%' range
    (when (or (eq begin 'all) (eq end 'all))
      (setq begin 'first-line
            begin-off 0
            end 'last-line
            end-off 0
            sep ?,))
    
    (when (= pos (or (string-match "[[:alnum:]!]+" text pos) -1))
      (setq cmd (cons (match-beginning 0) (match-end 0))))
               
    (multiple-value-bind (start end) (vim:ex-get-range (and begin (cons begin begin-off)) sep (and end (cons end end-off)))
      (values cmd start end))))


(defun vim:ex-parse-address (text pos)
  (cond
   ((>= pos (length text)) nil)
   
   ((= pos (or (string-match "[[:digit:]]+" text pos) -1))
    (values (cons 'abs (string-to-number (match-string 0 text)))
            (match-end 0)))

   ((= (aref text pos) ?\%)
    (values 'all (1+ pos)))
    
   ((= (aref text pos) ?.)
    (values 'current-line (1+ pos)))

   ((= (aref text pos) ?/)
    (values 'next-of-prev-search (1+ pos)))
   
   ((= (aref text pos) ??)
    (values 'prev-of-prev-search (1+ pos)))
   
   ((= (aref text pos) ?&)
    (values 'next-of-prev-subst (1+ pos)))

   ((= (aref text pos) ?')
    (if (>= (1+ pos) (length text))
        nil
      (values `(mark (aref text (1+ pos))) (+ 2 pos))))

   ((= (aref text pos) ?%)
    (values 'all (1+ pos)))
   
   ((= (aref text pos) ?/)
    (error "Regexp-addresses not yet implemented"))
    
   (t nil)))


(defun vim:ex-parse-offset (text pos)
  (let ((off nil))
    (while (= pos (or (string-match "\\([-+]\\)\\([[:digit:]]+\\)?" text pos) -1))
      (if (string= (match-string 1 text) "+")
          (setq off (+ (or off 0) (if (match-beginning 2)
                                      (string-to-number (match-string 2 text))
                                    1)))
                
        (setq off (- (or off 0) (if (match-beginning 2)
                                    (string-to-number (match-string 2 text))
                                  1))))
      (setq pos (match-end 0)))
    (and off (values off pos))))
     

(defun vim:ex-get-range (start sep end)
  (with-current-buffer vim:ex-current-buffer
    (when start
      (setq start (vim:ex-get-line start)))

    (when (and sep end)
      (save-excursion
        (when (= sep ?\;) (goto-line start))
        (setq end (vim:ex-get-line end))))
  
    (values start end)))


(defun vim:ex-get-line (address)
  (let ((base (car address))
        (offset (cdr address)))
    
    (cond
     ((null base) nil)
     ((consp offset)
      (let ((line (vim:ex-get-line (car address))))
        (when line
        (save-excursion
          (goto-line line)
          (vim:ex-get-line (cdr address))))))
     
     (t
      (+ offset
         (case (or (car-safe base) base)
         (abs (cdr base))
           
         ;; TODO: (1- ...) may be wrong if the match is the empty string
         (re-fwd (save-excursion
                   (beginning-of-line 2)
                   (and (re-search-forward (cdr base))
                        (line-number-at-pos (1- (match-end 0))))))
           
         (re-bwd (save-excursion
                   (beginning-of-line 0)
                   (and (re-search-backward (cdr base))
                        (line-number-at-pos (match-beginning 0)))))
           
         (current-line (line-number-at-pos (point)))
         (first-line (line-number-at-pos (point-min)))
         (last-line (line-number-at-pos (point-max)))
         (mark (error "Marks not yet implemented."))
         (next-of-prev-search (error "Next-of-prev-search not yet implemented."))
         (prev-of-prev-search (error "Prev-of-prev-search not yet implemented."))
         (next-of-prev-subst (error "Next-of-prev-subst not yet implemented."))
         (t (error "Invalid address: %s" address))))))))


(defun vim:ex-read-command ()
  "Starts ex-mode."
  (interactive)
  (let ((vim:ex-current-buffer (current-buffer)))
    (let ((minibuffer-local-completion-map vim:ex-keymap))
      (let ((result (completing-read ":" 'vim:ex-complete nil nil nil  'vim:ex-history)))
        (when result
          (vim:ex-execute-command result))))))

(vim:nmap ":" 'vim:ex-read-command)

;;; vim-ex.el ends here
