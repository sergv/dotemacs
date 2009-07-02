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
        vim:current-motion-type nil
        vim:current-key-sequence nil))


;; The type should be nil, map, inclusive, exclusive or linewise.
;; If type is nil this command is an operation, if it's map its a mapping and
;; otherwise it's a motion.

(defstruct (vim:command
            (:constructor vim:make-command))
  type        ; The type of the command.
  function    ; Function to be invoked.
  arg         ; If non-nil the command takes an argument.
  )


(defun* vim:def-motion (keys func
                             &key
                             (mode vim:normal-mode)
                             (arg nil)
                             (type 'inclusive))
  (vim:add-node (vim:mode-get-keymap mode) keys
                (vim:make-command :type type
                                  :function func
                                  :arg arg)
                :function 'vim:execute-command))

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
  (let ((cmd-type (vim:command-type (vim:node-cmd node))))
    (cond
     ((null cmd-type)  (vim:do-command node))
     ((eq 'map cmd-type) ('map (error "Execute mapping")))
     (t (vim:do-motion-command node)))
    (vim:vim-reset-key-state)))


(defun vim:prepare-complex-command (node)
  (when vim:current-cmd
    (error "Expected motion"))
  (setq vim:current-cmd node)
  (vim:go-to-node node))


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
      (vim:do-motion-command dummy))))
  (vim:go-to-node vim:normal-mode-keymap))


(defun vim:do-command (node)
  "Executes the command of node."
  (when vim:current-cmd
    (error "Unexpected command in operator-pending mode"))
  (vim:go-to-node node)
  (setq vim:current-cmd node)
  (vim:execute-current-command)
  (vim:reset-key-state))


(defun vim:do-motion-command (node)
  "Executes the motion command of node."
  
  (vim:go-to-node node)
  
  (unless vim:current-cmd
    (setq vim:current-motion-count vim:current-cmd-count)
    (setq vim:current-cmd-count nil))

  ;; arguments missing
  
  (vim:go-to-node node)
  (setq vim:current-motion node)
  
  (when (vim:command-arg (vim:node-cmd vim:current-motion))
    (setq vim:current-motion-arg (read-char)))
  
  (if vim:current-cmd
      (vim:execute-current-command)
    (vim:execute-current-motion))
  (vim:vim-reset-key-state))


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
      (vim:project-motion
       (if (vim:command-arg cmd)
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
        
        (case (vim:command-type cmd)
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


(defun vim:execute-mapping (node)
  "Executes the right-hand-side of the mapping command."
  ;; reset key-state to the correct intermediate state
  (setq vim:current-node (or vim:current-cmd
                             (vim:active-keymap)))

  (let ((vim:next-insert-undo vim:last-undo))
    ;; replay the rhs-events
    (execute-kbd-macro (vim:command-function (vim:node-cmd node)))))


