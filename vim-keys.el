;; TODO:
;;  - special keymap for motions following commands

(provide 'vim-keys)

(defvar vim:current-register
  "The register of the current command."
  nil)

(defvar vim:current-cmd-count
  "The count of the current command."
  nil)

(defvar vim:current-cmd
  "The node of the current command."
  nil)

(defvar vim:current-cmd-arg
  "The argument of the current command."
  nil)

(defvar vim:current-motion-count
  "The count of the current motion."
  nil)

(defvar vim:current-motion
  "The node of the current motion."
  nil)

(defvar vim:current-motion-arg
  "The argument of the current motion."
  nil)

(defvar vim:current-motion-type
  "The type of the current motion (inclusive, exclusive, linewise)."
  nil)

(defvar vim:current-node
  "The currently active node."
  nil)

(defvar vim:current-key-sequence
  "The key-sequence of the current command."
  nil)


(defun vim:reset-key-state ()
  "Resets the current state of the keymap."
  (setq vim:current-register nil
        vim:current-cmd-count nil
        vim:current-cmd nil
        vim:current-cmd-arg nil
        vim:current-motion-count nil
        vim:current-motion nil
        vim:current-motion-arg nil
        vim:current-motion-type nil
        vim:current-node (vim:active-keymap)
        vim:current-key-sequence nil))

(defun vim:input-key (key)
  "Appends the given key to the current command."
  
  (let ((node (vim:get-subnode vim:current-node key)))
    (if node
        ;; if the last command was a complex command, but we've found
        ;; a continuation so the last command should be seen as an
        ;; intermediate node (e.g. the first 'd' of 'dd'), then we
        ;; have to clear the command (or motion) variable!!!
        (progn
          (when (eq vim:current-node vim:current-cmd)
            (setq vim:current-cmd nil))
          (when (eq vim:current-node vim:current-motion)
            (setq vim:current-motion nil))
          (vim:execute-node key node))
      (vim:continue-node key))))

(defun vim:continue-node (key)
  "Called if the current keymap does not contain key, tries to continue the command."
  (unless
      (or
       (eq vim:current-node (vim:active-keymap)) ;; begin a new command
       (eq vim:current-node vim:current-cmd))    ;; begin a motion
    (throw 'vim:unknown-command 1))

  (let ((node (vim:get-subnode (vim:active-keymap) key)))
    (if node
        (vim:execute-node key node)
      (throw 'vim:unknown-command 2))))

(defun vim:execute-node (key node)
  "Execute the command associated with node."

  (let ((cmd (vim:node-cmd node)))
    (cond
     ((null cmd) (vim:go-to-node node))
     
     ((vim:simple-command-p cmd)
      (vim:do-simple-command node))
   
     ((vim:simple-arg-command-p cmd)
      (vim:do-simple-arg-command node))
   
     ((vim:complex-command-p cmd)
      (vim:feed-complex-command node))

     ((vim:motion-command-p cmd)
      (vim:do-motion-command node))

     ((vim:motion-arg-command-p cmd)
      (vim:do-motion-arg-command node))

     ((vim:map-command-p cmd)
      (vim:do-mapping-command cmd))

     ((vim:special-command-p cmd)
      (vim:do-special-command node))

     ((vim:ignore-command-p cmd)
      (throw 'vim:unknown-command 42))
     
     (t (error "Unknown command type")))))
    

;; this command is implemented as a special command
(defun vim:feed-numeric-prefix (node)
  "Saves the numeric character and continues."
  (let ((char (vim:node-key node)))
    (if vim:current-cmd
        (push (- char ?0) vim:current-motion-count)
      (push (- char ?0) vim:current-cmd-count))))


;; this command is implemented as a special command
(defun vim:feed-numeric-prefix-or-bol (node)
  "Saves the numeric character and continues."
  (cond
   ((and (not vim:current-cmd) vim:current-cmd-count)
    (push (- (vim:node-key node) ?0) vim:current-cmd-count))
   
   ((and vim:current-cmd vim:current-motion-count)
    (push (- (vim:node-key node) ?0) vim:current-motion-count))

   (t
    (let ((dummy (vim:make-node :key ?0
                                :cmd (vim:make-command :type 'motion
                                                       :function 'vim:motion-beginning-of-line
                                                       :motion-type 'exclusive))))
      (vim:do-motion-command dummy)))))



(defun vim:feed-complex-command (node)
  "Saves the complex command of node and continues for the motion."
  (when vim:current-cmd
    (error "Command not allowed in operator-pending mode."))
  (setq vim:current-cmd node)
  (vim:go-to-node node))

(defun vim:go-to-node (node)
  "Changes the current node."
  (setq vim:current-node node))


(defun vim:do-simple-command (node)
  "Executes the simple command of node."
  (vim:go-to-node node)
  (setq vim:current-cmd node)
  (vim:execute-current-command)
  (vim:reset-key-state))

(defun vim:do-simple-arg-command (node)
  "Executes the simple-arg with an argument."
  (setq vim:current-cmd-arg (read-char "Argument: "))
  (vim:do-simple-command node))

(defun vim:do-motion-command (node)
  "Executes the motion command of node."
  
  (unless vim:current-cmd
    (setq vim:current-motion-count vim:current-cmd-count)
    (setq vim:current-cmd-count nil))

  (vim:go-to-node node)
  (setq vim:current-motion node)
  (if vim:current-cmd
      (vim:execute-current-command)
    (vim:execute-current-motion))
  (vim:reset-key-state))

(defun vim:do-motion-arg-command (node)
  "Executes the motion-arg command of vim:current-motion with an argument."
  (setq vim:current-motion-arg (read-char "Argument: "))
  (vim:do-motion-command node))
  

(defun vim:do-mapping-command (cmd)
  "Executes the right-hand-side of the mapping command."
  ;; reset key-state to the correct intermediate state
  (setq vim:current-node (or vim:current-cmd
                             (vim:active-keymap)))

  (let ((vim:next-insert-undo vim:last-undo))
    ;; replay the rhs-events
    (execute-kbd-macro (vim:command-function cmd))))


(defun vim:do-special-command (node)
  "Executes the function of a special command without noticing the node otherwise."
  (funcall (vim:command-function (vim:node-cmd node)) node))


(defun vim:convert-command-counts ()
  "Converts the count-lists to numbers."
  (labels
      
      ((convert (rest)
                (if rest
                    (+ (car rest) (* 10 (convert (cdr rest))))
                  0)))
    
    (when vim:current-cmd-count
      (setq vim:current-cmd-count (convert vim:current-cmd-count)))
    (when vim:current-motion-count
      (setq vim:current-motion-count (convert vim:current-motion-count)))))


(defun vim:execute-current-command ()
  "Execute the current full command."
  (vim:convert-command-counts)
  
  (if vim:current-cmd-arg
      (funcall (vim:mode-execute-command vim:active-mode)
               (vim:node-cmd vim:current-cmd)
               vim:current-cmd-count
               (vim:get-current-cmd-motion)
               vim:current-cmd-arg)
    (funcall (vim:mode-execute-command vim:active-mode)
             (vim:node-cmd vim:current-cmd)
             vim:current-cmd-count
             (vim:get-current-cmd-motion)))

  (vim:adjust-point))


(defun vim:execute-current-motion ()
  "Execute the current motion."
  (vim:convert-command-counts)
  (funcall (vim:mode-execute-motion vim:active-mode) (vim:get-current-motion))
  (vim:adjust-point))


(defun vim:get-current-motion ()
  (if (null vim:current-motion)
      nil
    (let ((cmd (vim:node-cmd vim:current-motion))
          (count (if (or vim:current-cmd-count
                         vim:current-motion-count)
                     (* (or vim:current-cmd-count 1)
                        (or vim:current-motion-count 1))
                   nil)))
      (vim:project-motion
       (if (vim:motion-arg-command-p cmd)
           (funcall (vim:command-function cmd) count vim:current-motion-arg)
         (funcall (vim:command-function cmd) count))))))


(defun vim:get-current-cmd-motion ()
  "Returns the motion range for the current command w.r.t. inclusive/exclusive/linewise."
  (if vim:current-motion
      
      (let ((motion (vim:get-current-motion))
            (cmd (vim:node-cmd vim:current-motion)))

        ;; if the motion is only a position, the second position is
        ;; (point)
        (unless (vim:range-p motion)
          (setq motion (cons motion (point))))

        ;; convert possible row-column pairs
        (setq motion (cons (if (vim:coord-p (car motion))
                               (vim:coord-to-pos (car motion))
                             (car motion))
                           (if (vim:coord-p (cdr motion))
                               (vim:coord-to-pos (cdr motion))
                             (cdr motion))))
        
        ;; order the motion
        (setq motion (cons (min (car motion) (cdr motion))
                           (max (car motion) (cdr motion))))
        
        (case (vim:command-motion-type cmd)
          ('inclusive
           (setq vim:current-motion-type 'inclusive)
           (cons (car motion) (vim:adjust-end-of-line-position (cdr motion))))

          ('exclusive
           (if (= (cdr motion)
                  (save-excursion
                    (goto-char (cdr motion))
                    (line-beginning-position)))
               
               (if (save-excursion
                     (goto-char (car motion))
                     (looking-back "^[[:space:]]*"))
                   ;; motion becomes linewise(-exclusive)
                   (progn
                     (setq vim:current-motion-type 'linewise)
                     (cons (save-excursion
                             (goto-char (car motion))
                             (line-beginning-position))
                           (1- (cdr motion)))) ; will move to the previous end-of-line
                 
                 ;; motion becomes inclusive
                 (progn
                   (setq vim:current-motion-type 'inclusive)
                   (cons (car motion) (1- (cdr motion))))) ; will move to the previous end-of-line

             ;; usual exclusive motion; in this case the end-of-motion
             ;; will not be on the first character in a line, so (1-
             ;; (cdr motion)) is save
             (setq vim:current-motion-type 'exclusive)
             (cons (car motion) (1- (cdr motion)))))

          ('linewise
           (message "LINEWISE")
           (setq vim:current-motion-type 'linewise)
           (cons (save-excursion
                   (goto-char (car motion))
                   (line-beginning-position))
                 (save-excursion
                   (goto-char (cdr motion))
                   (line-end-position))))))

    ;; no motion -> return nil
    nil))


(defun vim:handle-key ()
  (interactive)
  (when
      (catch 'vim:unknown-command
        (condition-case err
            (progn
              (setq vim:last-undo buffer-undo-list)
              (vim:input-key last-command-event)
              (vim:connect-undos))
          (error
           (vim:reset-key-state)
           (error err)))
        nil)
    (when (null (and (vim:mode-default-handler vim:active-mode)
                     (funcall (vim:mode-default-handler vim:active-mode))))
      (push last-command-event unread-command-events)
      (add-hook 'post-command-hook 'vim:enable-keymap)
      (setq vim-key-mode nil))))
        ;(execute-kbd-macro (vector last-command-event))))))

(defun vim:enable-keymap ()
  (when (and (not (input-pending-p))
             vim-mode
             (not (eq this-command 'vim:handle-key)))
    (vim:update-position)
    (remove-hook 'post-command-hook 'vim:enable-keymap)
    (setq vim-key-mode t)))


(defvar vim:mode-map nil)
(setq vim:mode-map
      (list 'keymap
            (cons t 'vim:handle-key)))


(define-minor-mode vim-key-mode
  "VIM emulation mode - keymap"
  :lighter nil
  :initial-value nil
  :global nil
  :keymap vim:mode-map)


(add-hook 'vim-key-mode-hook 'vim:update-position)

(defun vim:update-position ()
  (setq vim:current-coord (vim:make-coord-from-pos (point))))
