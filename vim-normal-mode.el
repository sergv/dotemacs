;;; vim-normal-mode.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.2.0
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; TODO:
;;
;;  - searching currently uses isearch.  Although this is quite powerful,
;;    it's only usuably as interactive search and difficult to use with
;;    semi-interactive stuff like the "*" command.  The current implementation
;;    using unread-command-events is quite ugly.

(provide 'vim-normal-mode)

(defconst vim:operator-repeat-keymap (vim:make-keymap vim:override-keymap)
  "Keymap to bind the repeat-operator-event.")
(defconst vim:operator-pending-mode-keymap (vim:make-keymap vim:operator-repeat-keymap)
  "VIM operator-pending-mode keymap.")
(defun vim:omap (keys command)
  "Defines a new operator-pending-mode mapping."
  (vim:map keys command :keymap vim:operator-pending-mode-keymap))

(vim:define-mode operator-pending "VIM operator-pending mode"
                 :ident "O"
                 :keymap vim:operator-pending-mode-keymap
                 :command-function 'vim:operator-pending-mode-command)

(add-hook 'vim:operator-pending-mode-hook 'vim:set-operator-repeat-key)
(defun vim:set-operator-repeat-key ()
  (if vim:operator-pending-mode
      (vim:map (vector last-command-event) 'vim:motion-lines
               :keymap vim:operator-repeat-keymap)
    (setcdr vim:operator-repeat-keymap (keymap-parent vim:operator-repeat-keymap))))


(defun vim:operator-pending-mode-command (command)
  "Executes a complex command in operator-pending mode."
  (unwind-protect
      (case (vim:cmd-type command)
        ('simple (error "No simple-commands allowed in operator-pending mode."))
        ('complex (error "No complex-commands allowed in operator-pending mode."))
        ('map (error "no mapping so far"))
        ('special (error "no special so far"))
        (t (vim:execute-complex-command command)))
    
    (when (vim:operator-pending-mode-p)
      (vim:activate-mode 'normal))))


(defconst vim:normal-mode-keymap (vim:make-keymap vim:operator-pending-mode-keymap)
  "VIM normal-mode keymap.")
(defun vim:nmap (keys command)
  "Defines a new normal-mode mapping."
  (vim:map keys command :keymap vim:normal-mode-keymap))

(vim:define-mode normal "VIM normal mode"
                 :ident "N"
                 :keymap vim:normal-mode-keymap
                 :command-function 'vim:normal-mode-command)

(defun vim:normal-mode-command (command)
  "Executes a motion or simple-command or prepares a complex command."
  (case (vim:cmd-type command)
    ('simple (vim:execute-simple-command command))
    ('complex (vim:prepare-complex-command command))
    ('map (vim:execute-mapping command))
    ('special (error "no special so far"))
    (t (vim:execute-motion command))))


(defun vim:execute-motion (command)
  "Executes a motion."
  (setq vim:current-motion command)

  (when current-prefix-arg
    (setq vim:current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (vim:cmd-arg-p command)
    (setq vim:current-motion-arg (read-char)))

  (unwind-protect
      (let ((motion (vim:get-current-motion)))
        (if (eq (vim:motion-type motion) 'block)
            (progn
              (goto-line (car (vim:motion-end motion)))
              (move-to-column (cdr (vim:motion-end motion))))
          (goto-char (vim:motion-end motion))))
    
    (vim:reset-key-state)
    (vim:clear-key-sequence)
    (vim:adjust-point)))


(defun vim:execute-simple-command (command)
  "Executes a simple command."
  (when current-prefix-arg
    (setq vim:current-cmd-count (prefix-numeric-value current-prefix-arg)))
  
  (when (vim:cmd-arg-p command)
    (setq vim:current-cmd-arg (read-char)))

  (unwind-protect
      (let ((parameters nil)
            (vim:last-undo buffer-undo-list))
        (when (vim:cmd-count-p command)
          (push vim:current-cmd-count parameters)
          (push :count parameters))
        (when (vim:cmd-arg-p command)
          (push vim:current-cmd-arg parameters)
          (push :argument parameters))
        (vim:apply-save-buffer (vim:cmd-function command) parameters)
        (when (vim:cmd-repeatable-p command)
          (setq vim:repeat-events (vconcat vim:current-key-sequence
                                           (this-command-keys))))
        (vim:connect-undos vim:last-undo))

    (vim:reset-key-state)
    (vim:clear-key-sequence)
    (vim:adjust-point)))
    

(defun vim:prepare-complex-command (command)
  "Prepares a complex command, switching to operator-pending mode."
  (when current-prefix-arg
    (setq vim:current-cmd-count (prefix-numeric-value current-prefix-arg)))
  
  (setq vim:current-cmd command)
  (setq vim:current-key-sequence (vconcat vim:current-key-sequence (this-command-keys)))
  (vim:activate-mode 'operator-pending))

(defun vim:execute-complex-command (motion-command)
  "Executes a complex command with a certain motion command."
  (setq vim:current-motion motion-command)

  (when current-prefix-arg
    (setq vim:current-motion-count (prefix-numeric-value current-prefix-arg)))

  (when (or vim:current-motion-count vim:current-cmd-count)
    (setq vim:current-motion-count (* (or vim:current-cmd-count 1)
                                      (or vim:current-motion-count 1)))
    (setq vim:current-cmd-count nil))

  (when (vim:cmd-arg-p motion-command)
    (setq vim:current-motion-arg (read-char)))

  (unwind-protect
      (let ((vim:last-undo buffer-undo-list))
        (vim:funcall-save-buffer (vim:cmd-function vim:current-cmd)
                                 :motion (vim:get-current-cmd-motion))
        (when (vim:cmd-repeatable-p vim:current-cmd)
          (setq vim:repeat-events (vconcat vim:current-key-sequence
                                           (this-command-keys))))
        (vim:connect-undos vim:last-undo))
    
    (vim:reset-key-state)
    (vim:clear-key-sequence)
    (vim:adjust-point)))


(defun vim:execute-mapping (events)
  "Executes certain `events' defined by a mapping."
  (execute-kbd-macro events))


;;(defconst vim:search-mode-keymap (vim:make-node))
;;
;;(vim:deflocalvar vim:search-last-direction nil
;;   "The last search direction, either 'forward or 'backward.")
;;
;;(defun vim:search-mode-activate ()
;;  (setq cursor-type vim:normal-mode-cursor))
;;
;;(defun vim:search-mode-deactivate ()
;;  (isearch-exit))
;;
;;(defun vim:search-default-handler ()
;;  (vim:activate-mode vim:normal-mode)
;;  (push last-command-event unread-command-events)
;;  t)
;;
;;;; Search mode is a very special mode being activate during a search
;;;; command.  Its purpose is to disable highlighting of search results
;;;; if something else than a repeat-search event occurs.
;;(defconst vim:search-mode
;;  (vim:make-mode :name "Search"
;;                 :id "S"
;;		 :activate #'vim:search-mode-activate
;;		 :deactivate #'vim:search-mode-deactivate
;;		 :execute-command #'vim:default-mode-exec-cmd
;;		 :execute-motion #'vim:default-mode-exec-motion
;;		 :keymap 'vim:search-mode-keymap
;;		 :default-handler 'vim:search-default-handler))
;;
;;
;;(vim:defcmd vim:search-start (nonrepeatable)
;;  "Starts an incremental regexp search."
;;  (unless (eq vim:active-mode vim:search-mode)
;;    (vim:activate-mode vim:search-mode))
;;  (setq vim:last-search-direction 'forward)
;;  (isearch-forward t))
;;
;;
;;(vim:defcmd vim:search-start-backward (nonrepeatable)
;;  "Starts an incremental regexp search."
;;  (unless (eq vim:active-mode vim:search-mode)
;;    (vim:activate-mode vim:search-mode))
;;  (setq vim:last-search-direction 'backward)
;;  (isearch-backward t))
;;
;;
;;(vim:defcmd vim:search-repeat (nonrepeatable)
;;  "Repeats the last incremental search."
;;  (unless (eq vim:active-mode vim:search-mode)
;;    (vim:activate-mode vim:search-mode))
;;  (isearch-repeat vim:last-search-direction))
;;
;;
;;(vim:defcmd vim:search-repeat-opposite (nonrepeatable)
;;  "Starts an incremental regexp search."
;;  (unless (eq vim:active-mode vim:search-mode)
;;    (vim:activate-mode vim:search-mode))
;;  (isearch-repeat (case vim:last-search-direction
;;		    ('forward 'backward)
;;		    (t 'forward))))
;;
;;
;;(defun vim:start-word-search (unbounded direction)
;;  
;;  (condition-case nil
;;      (goto-char (vim:motion-bwd-word-end 1))
;;    (error nil))
;;  
;;  (save-excursion
;;    (re-search-forward (concat "\\<[" vim:word "]+\\>")))
;;  
;;  (when (eq direction 'backward)
;;    (goto-char (1+ (match-end 0))))
;;  (let ((events (reverse (append (if (eq direction 'forward)
;;				     "/"
;;				   "?")
;;				 (if unbounded
;;				     (regexp-quote (match-string 0))
;;				   (concat "\\<" 
;;					   (regexp-quote (match-string 0))
;;					   "\\>"))
;;				 [return]
;;				 "n"
;;				 nil))))
;;    (while events
;;      (push (car events) unread-command-events)
;;      (setq events (cdr events)))))
;;
;;
;;(vim:defcmd vim:search-word (nonrepeatable)
;;  "Searches the next occurence of word under the cursor."
;;  (vim:start-word-search nil 'forward))
;;    
;;    
;;(vim:defcmd vim:search-word-backward (nonrepeatable)
;;  "Searches the next occurence of word under the cursor."
;;  (vim:start-word-search nil 'backward))
;;    
;;    
;;(vim:defcmd vim:search-unbounded-word (nonrepeatable)
;;  "Searches the next occurence of word under the cursor."
;;  (vim:start-word-search t 'forward))
;;    
;;    
;;(vim:defcmd vim:search-unbounded-word-backward (nonrepeatable)
;;  "Searches the next occurence of word under the cursor."
;;  (vim:start-word-search t 'backward))
