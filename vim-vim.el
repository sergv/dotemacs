;;; vim-node.el

;; Copyright (C) 2009 Frank Fischer
;; 
;; Version: 0.0.1
;; Keywords: emulations
;; Human-Keywords: vim, emacs
;; Authors: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; License: GPLv2 or later, as described below under "License"

;; Description

;; This file contains the stuff specific for vim-like keybindins.

;; TODO: - currently the next-keymap of a complex command is the keymap itself

(defvar vim:repeat-events
  "The sequence of events for the repeat command."
  nil)

(defun vim:do-not-repeat ()
  "Supresses repeating of the current command."
  (unless executing-kbd-macro
    (setq vim:current-key-sequence nil)))

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


(defun vim:vim-reset-key-state ()
  "Resets the current state of the keymap."
  (vim:reset-key-state)
  (setq vim:current-register nil
        vim:current-cmd-count nil
        vim:current-cmd nil
        vim:current-cmd-arg nil
        vim:current-motion-count nil
        vim:current-motion nil
        vim:current-motion-arg nil
        vim:current-motion-type nil)
  (unless executing-kbd-macro
    (setq vim:current-key-sequence nil)))


;; The type should be nil, map or motion.
(defstruct (vim:command
            (:constructor vim:make-command))
  type        ; The type of the command.
  function    ; Function to be invoked.
  arg         ; If non-nil the command takes an argument.
  )


(defun* vim:def-motion (keys func
                             &key
                             (mode vim:normal-mode)
                             (arg nil))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type 'motion
                                  :function func
                                  :arg arg)
                :function 'vim:execute-motion))

(defun* vim:def-simple (keys func &key (mode vim:normal-mode) (arg nil))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type nil
                                  :function func
                                  :arg arg)
                :function 'vim:execute-command))

(defun* vim:def-complex (keys func &key (mode vim:normal-mode))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type nil
                                  :function func
                                  :arg nil)
                :next-keymap (vim:mode-get-keymap mode)
                :function 'vim:prepare-complex-command))

(defun* vim:def-map (keys rhs &key (mode vim:normal-mode))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type 'map
                                  :function rhs)
                :function 'vim:execute-mapping))

(defun* vim:def-special (keys func &key (mode vim:normal-mode))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type nil
                                  :function func)
                ;; special commands return to their own keymap
                :function 'vim:execute-special))



(defun vim:execute-command (node)
  (when vim:current-cmd
    (error "Unexpected command in operator-pending mode"))
  (vim:go-to-node node)
  (setq vim:current-cmd node)
  (vim:execute-current-command)
  (vim:vim-reset-key-state))


(defun vim:prepare-complex-command (node)
  (when vim:current-cmd
    (error "Expected motion"))
  (setq vim:current-cmd node)
  (vim:go-to-node node))


(defun vim:execute-motion (node)
  "Executes the motion command of node or completes a pending complex command."
  
  (vim:go-to-node node)
  (setq vim:current-motion node)
  
  (unless vim:current-cmd
    (setq vim:current-motion-count vim:current-cmd-count)
    (setq vim:current-cmd-count nil))

  (when (vim:command-arg (vim:node-cmd vim:current-motion))
    (setq vim:current-motion-arg (read-char)))
  
  (if vim:current-cmd
      (vim:execute-current-command)
    (vim:execute-current-motion))
  (vim:vim-reset-key-state))


(defun vim:execute-special (node)
  "Executes the function of a special command without noticing the node otherwise."
  (funcall (vim:command-function (vim:node-cmd node)) node))



;; this command is implemented as a special command
(defun vim:feed-numeric-prefix (node)
  "Saves the numeric character and continues."
  (let ((char (vim:node-key node)))
    (if vim:current-cmd
        (push (- char ?0) vim:current-motion-count)
      (push (- char ?0) vim:current-cmd-count)))
  (vim:go-to-node vim:normal-mode-keymap))


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
                                :cmd (vim:make-command :type 'exclusive
                                                       :function 'vim:motion-beginning-of-line)
                                :function 'vim:execute-command)))
      (vim:execute-motion dummy))))
  (vim:go-to-node vim:normal-mode-keymap))


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

  (when (vim:command-arg (vim:node-cmd vim:current-cmd))
    (setq vim:current-cmd-arg (read-char)))
  
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
      (if (vim:command-arg cmd)
          (funcall (vim:command-function cmd) count vim:current-motion-arg)
        (funcall (vim:command-function cmd) count)))))


(defun vim:get-current-cmd-motion ()
  "Returns the motion range for the current command w.r.t. inclusive/exclusive/linewise."
  (if vim:current-motion
      
      (let ((motion (vim:get-current-motion)))

        ;; order the motion
        (when (> (vim:motion-begin motion)
                 (vim:motion-end motion))
          (setq motion (vim:make-motion :begin (vim:motion-end motion)
                                        :end (vim:motion-begin motion)
                                        :type (vim:motion-type motion))))
        
        (case (vim:motion-type motion)
          ('inclusive
           (setq vim:current-motion-type 'inclusive)
           (vim:make-motion :begin (vim:motion-begin motion)
                            :end (vim:adjust-end-of-line-position (vim:motion-end motion))
                            :type 'inclusive))

          ('exclusive
           (if (save-excursion
                 (goto-char (vim:motion-end motion))
                 (bolp))
               
               (if (save-excursion
                     (goto-char (vim:motion-begin motion))
                     (looking-back "^[[:space:]]*"))
                   ;; motion becomes linewise(-exclusive)
                   (progn
                     (setq vim:current-motion-type 'linewise)
                     (vim:make-motion :begin (save-excursion
                                               (goto-char (vim:motion-begin motion))
                                               (line-beginning-position))
                                      :end (1- (vim:motion-end motion)) ; will move to the previous end-of-line
                                      :type 'linewise))
                 
                 ;; motion becomes inclusive
                 (progn
                   (setq vim:current-motion-type 'inclusive)
                   (vim:make-motion :begin (vim:motion-begin motion)
                                    :end (1- (vim:motion-end motion)) ; will move to the previous end-of-line
                                    :type 'inclusive)))
                                    
             ;; usual exclusive motion; in this case the end-of-motion
             ;; will not be on the first character in a line, so (1-
             ;; (vim:motion-end motion)) is save
             (setq vim:current-motion-type 'exclusive)
             (vim:make-motion :begin (vim:motion-begin motion)
                              :end (1- (vim:motion-end motion))
                              :type 'inclusive)))

          ('linewise
           (setq vim:current-motion-type 'linewise)
           (vim:make-motion :begin (save-excursion
                                     (goto-char (vim:motion-begin motion))
                                     (line-beginning-position))
                            :end (save-excursion
                                   (goto-char (vim:motion-end motion))
                                   (line-end-position))
                            :type 'linewise))))

    ;; no motion -> return nil
    nil))


(defun vim:execute-mapping (node)
  "Executes the right-hand-side of the mapping command."
  ;; reset key-state to the correct intermediate state
  (setq vim:current-node (or vim:current-cmd
                             (vim:active-keymap)))

  (let ((vim:next-insert-undo vim:last-undo))
    ;; replay the rhs-events
    (execute-kbd-macro (vim:command-function (vim:node-cmd node)))))


