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
(define-key vim:ex-keymap [return] 'vim:ex-execute-command)
(define-key vim:ex-keymap " " 'vim:ex-expect-argument)
(define-key vim:ex-keymap (kbd "C-j") 'vim:ex-execute-command)
(define-key vim:ex-keymap (kbd "C-g") 'abort-recursive-edit)
(define-key vim:ex-keymap [up] 'next-history-element)
(define-key vim:ex-keymap [down] 'previous-history-element)

(defvar vim:ex-keep-reading nil)
(defvar vim:ex-cmdline nil)
(defvar vim:ex-cmd nil)
(defvar vim:ex-beg nil)
(defvar vim:ex-end nil)


(defun vim:ex-complete-command (cmdline predicate flag)

  (with-current-buffer vim:ex-current-buffer
    (multiple-value-bind (cmd-region beg end) (vim:ex-parse cmdline)
      (let ((cmd (substring cmdline (car cmd-region) (cdr cmd-region)))
            (range (substring cmdline 0 (car cmd-region))))
        (cond
         ((eq nil flag)
          (let ((complete (try-completion cmd vim:ex-commands predicate)))
            (if (stringp complete)
                (concat range complete)
              complete)))
         
         ((eq t flag)
          (all-completions cmd vim:ex-commands predicate))
         
         ((eq 'lambda flag)
          (test-completion cmd vim:ex-commands predicate)))))))

(defun vim:ex-complete-and-get-current-command ()
  (let ((cmdline (buffer-substring-no-properties 2 (point-max))))
    (let (cmd range completion)
      (multiple-value-bind (cmd-region beg end)
          (with-current-buffer vim:ex-current-buffer (vim:ex-parse cmdline))
        (with-current-buffer vim:ex-current-buffer
          (setq cmd (substring cmdline (car cmd-region) (cdr cmd-region))
                range (substring cmdline 0 (car cmd-region))
                completion (try-completion cmd vim:ex-commands nil)))
        (cond
         ((stringp completion)
          (delete-region (+ (car cmd-region) 2) (+ (cdr cmd-region) 2))
          (insert completion)
          (values beg end completion))
         (t (values beg end cmd)))))))

(defun vim:ex-get-current-command ()
  (multiple-value-bind (beg end cmd) (vim:ex-complete-and-get-current-command)
    (if cmd
        (progn
          (setq vim:ex-cmdline (buffer-substring-no-properties 2 (point-max))
                vim:ex-cmd cmd
                vim:ex-beg beg
                vim:ex-end end)
          t)
      nil)))

(defun vim:ex-execute-command ()
  (interactive)
  (if (vim:ex-get-current-command)
      (progn
        (setq vim:ex-keep-reading nil)
        (exit-minibuffer))
    (ding)))

(defun vim:ex-expect-argument ()
  (interactive)
  (if (vim:ex-get-current-command)
      (progn
        (setq vim:ex-keep-reading t)
        (exit-minibuffer))
    (ding)))

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
    
    (when (= pos (or (string-match "[[:alnum:]]+" text pos) -1))
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
  (when start
    (setq start (vim:ex-get-line start)))

  (when (and sep end)
    (save-excursion
      (when (= sep ?\;) (goto-line start))
      (setq end (vim:ex-get-line end))))
  
  (values start end))


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


(defun vim:ex-do-command (cmd)
  (while (stringp cmd)
    (setq cmd (cdr-safe (assoc cmd vim:ex-commands))))

  (case (get 'type cmd)
    ('ex
     (case (get 'argument cmd)
       ('file-argument
        (funcall cmd
                 :begin vim:ex-beg :end vim:ex-end
                 :argument (if vim:ex-keep-reading
                               (read-file-name (concat ":" vim:ex-cmdline " "))
                             t)))
       
       ('buffer-argument
        (funcall cmd
                 :begin vim:ex-beg :end vim:ex-end
                 :argument (if vim:ex-keep-reading
                               (read-buffer (concat ":" vim:ex-cmdline " ") (other-buffer))
                             t)))

       ('argument
        (funcall cmd
                 :begin vim:ex-beg :end vim:ex-end
                 :argument (if vim:ex-keep-reading
                               (read-from-minibuffer (concat ":" vim:ex-cmdline " "))
                             t)))
       
       (t
        (if vim:ex-keep-reading
            (error "Ex-command %s does not accept an argument" vim:ex-cmd)
          (funcall cmd :begin vim:ex-beg :end vim:ex-end)))))

    ('simple
        (if vim:ex-keep-reading
            (error "Ex-command %s does not accept an argument" vim:ex-cmd)
          (funcall cmd)))
    
    (t (error "Invalid ex-command bound to %s" vim:ex-cmd))))


(defun vim:ex-read-command ()
  "Starts ex-mode."
  (interactive)
  (let ((vim:ex-current-buffer (current-buffer))
                                        ;(minibuffer-completion-table 'vim:ex-complete-command))
        (vim:ex-keep-reading t))
    (let ((minibuffer-local-completion-map vim:ex-keymap))
      (completing-read ":" 'vim:ex-complete-command nil nil nil  'vim:ex-history))
    (vim:ex-do-command vim:ex-cmd)))

(vim:nmap ":" 'vim:ex-read-command)

