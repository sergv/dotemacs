;; vim-ex.el - Ex-mode. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'subr-x)
  (require 'macro-util))

(require 'persistent-sessions-global-vars)

(require 'vim-macs)
(require 'vim-core)
(require 'vim-compat)
(require 'vim-motions)

(defgroup vim-ex-mode nil
  "Configure ex-mode and search mode."
  :group 'vim-mode)

(defvar vim-ex--commands (make-hash-table :test #'equal)
  "Hash table, keys are string commands, values are functions.")

(defvar-local vim-ex--local-commands nil
  "Buffer-local version of `vim-ex--commands'.")

(defvar vim-ex--minibuffer nil
  "The currenty active ex minibuffer.")

(defvar vim-ex--current-buffer nil
  "The buffer to which the currently active ex session belongs to.")

(defvar vim-ex--current-window nil
  "The window to which the currently active ex session belongs to.")

(defvar vim-ex--history nil
  "History of ex-commands.")

(sessions-mark-global-var-for-save 'vim-ex--history)

(defvar vim-ex--cmd nil
  "The currently parsed command.")
(defvar vim-ex--arg nil
  "The currently parse command.")
(defvar vim-ex--arg-handler nil
  "The currently active argument handler.")
(defvar vim-ex--range nil
  "The currently parsed region.")

(defvar vim-ex-keymap
  (let ((map (make-sparse-keymap)))
    (def-keys-for-map map
      ("<escape>"               abort-recursive-edit)
      ("C-w"                    backward-delete-word)
      ("C-S-w"                  backward-delete-word*)
      ("C-h"                    next-line)
      ("C-t"                    previous-line)
      ("C-p"                    vim-cmd-paste-after-no-adjust)
      ("C-S-p"                  browse-kill-ring)
      ("C-/"                    nil)
      ("C-v"                    set-mark-command)
      ("C-y"                    copy-region-as-kill)
      ("C-d"                    kill-region)
      ("C-f"                    read-and-insert-filename)
      ("<delete>"               delete-char)
      ("<home>"                 beginning-of-line)
      ("<end>"                  end-of-line)
      ("SPC"                    self-insert-command)
      ("S-<delete>"             delete-whitespace-forward)
      ("S-<backspace>"          delete-whitespace-backward)

      ("?"                      self-insert-command)
      ("<tab>"                  minibuffer-complete)
      (("<return>" "RET" "C-j") vim-ex-mode-exit)
      ("C-g"                    vim-ex-mode-abort)
      ("<up>"                   previous-history-element)
      ("<down>"                 next-history-element)

      ("C-<return>"             newline)
      ("<backspace>"            vim-ex-backspace))
    map)
  "Keymap used in ex-mode.")

(defun vim-ex-backspace ()
  (interactive)
  (if (minibufferp)
      (progn
        (if (zerop (- (point-max) (minibuffer-prompt-end)))
            ;; (zerop (length (minibuffer-contents)))
            (exit-minibuffer)
          (pseudoparedit-backspace)))))

(defun vim-ex--contents ()
  "Returns the contents of the ex buffer.
The content is the same as minibuffer-contents would return
except for the info message."
  (with-current-buffer vim-ex--minibuffer
    (vim-ex--strip-ex-info (buffer-substring (minibuffer-prompt-end) (point-max)))))

(defvar vim-ex--global-commands-re-cache nil
  "This variable contains optimized regexp that matches
currently defined ex commands. Should be updated with
`vim-ex--map-command' when new ex commands being defined.")

(defvar-local vim-ex--local-commands-re-cache nil
  "Local variant of `vim-ex--global-commands-re-cache' that includes both global and
local commands.")

(defvar vim-ex--all-known-global-ex-commands nil
  "List of strings of all known ex-mode commands.")

(defvar-local vim-ex--all-known-local-ex-commands nil
  "Buffer-local version of `vim-ex--all-known-global-ex-commands' that includes local
commands only.")

(defvar-local vim-ex--all-known-local-and-global-ex-commands nil
  "Buffer-local version of `vim-ex--all-known-global-ex-commands' that includes both local
and global commands.")

(defun vim-ex--map-command (local keys command tbl)
  "NB Inavlidates `vim-ex--global-commands-re-cache' and `vim-ex--local-commands-re-cache'."
  (cl-assert (hash-table-p tbl))
  (cond
    ((stringp command)
     (aif (gethash command tbl)
       (progn
         (cl-assert (or (functionp it)
                        (vectorp it)))
         (puthash keys it tbl))
       (error "Keys do not refer to command: %s" keys)))
    ((or (functionp command)
         (vectorp command))
     (puthash keys command tbl))
    (t
     (error "Cannot emap command: %S of type %s"
            command
            (type-of command))))
  (if local
    (progn
      (push keys vim-ex--all-known-local-ex-commands)
      (setf vim-ex--all-known-local-and-global-ex-commands nil
            vim-ex--local-commands-re-cache nil))
    (progn
      (push keys vim-ex--all-known-global-ex-commands)
      (setf vim-ex--global-commands-re-cache nil))))

(defun vim-emap (keys command)
  "Maps an ex-command to some function."
  (vim-ex--map-command nil keys command vim-ex--commands))

(defun vim-local-emap (keys command)
  "Maps an ex-command to some function buffer-local."
  (unless vim-ex--local-commands
    (setf vim-ex--local-commands (make-hash-table :test #'equal)))
  (vim-ex--map-command t keys command vim-ex--local-commands))

(defun vim-ex--binding (cmd)
  "Returns the current binding of `cmd'. If no such
binding exists then nil is returned. If no such binding exists
but `cmd' is the beginning of more than one possible completion
the symbol 'incomplete is returned."
  (cond
    ((not (stringp cmd)) cmd)
    ((zerop (length cmd)) nil)
    (t
     (with-current-buffer vim-ex--current-buffer
       (let* ((cmds (vim-ex--complete-command cmd nil t))
              (c (cond
                   ;; no command found
                   ((null cmds) nil)
                   ;; exactly one completion found
                   ((null (cdr cmds)) (car cmds))
                   ;; one exact completion found
                   ((member cmd cmds) cmd)
                   ;; more than one inexact completion found
                   (t 'incomplete))))
         (while (and c (stringp c))
           (setq c (or (gethash c vim-ex--commands)
                       (and
                        vim-ex--local-commands
                        (gethash c vim-ex--local-commands)))))
         c)))))

(defsubst vim-ex--binding-p (binding)
  "Returns t iff the given binding non-nil and not 'incomplete."
  (and binding (not (eq binding 'incomplete))))

(cl-defstruct (vim-arg-handler
               (:constructor vim-make-arg-handler))
  complete   ;; The completion function.
  activate   ;; Called when the argument is activated for the first time.
  deactivate ;; Called when the argument is deactivated.
  update     ;; Called whenever the argument has changed.
  )

(defvar vim--argument-handlers-alist
  `((text . ,(vim-make-arg-handler :complete #'vim-ex--complete-text-argument))
    (file . ,(vim-make-arg-handler :complete #'vim-ex--complete-file-argument))
    (buffer . ,(vim-make-arg-handler :complete #'vim-ex--complete-buffer-argument)))
  "An alist that contains for each argument type the appropriate handler.")

(cl-defun vim--define-arg-handler (arg-type &key
                                            complete
                                            activate
                                            deactivate
                                            update)
  "Defines a new argument handler `arg-type'."
  (declare (indent 1))
  (let ((newah (vim-make-arg-handler :complete complete
                                     :activate activate
                                     :deactivate deactivate
                                     :update update))
        (ah (assoc arg-type vim--argument-handlers-alist)))
    (if ah
        (setcdr ah newah)
      (push (cons arg-type newah) vim--argument-handlers-alist))))

(defun vim-ex--get-arg-handler (cmd)
  "Returns the argument handler of command `cmd'."
  (let ((cmd (vim-ex--binding cmd)))
    (if (vim-ex--binding-p cmd)
        (let* ((arg-type (vim--cmd-arg cmd))
               (arg-handler (assoc arg-type vim--argument-handlers-alist)))
          (if arg-handler (cdr arg-handler)))
      (ding))))


(defun vim-ex--setup ()
  "Initializes the minibuffer for an ex-like mode.
This function should be called as minibuffer-setup-hook when an
ex-mode starts."
  (remove-hook 'minibuffer-setup-hook #'vim-ex--setup) ; Just for the case.
  (paredit-mode -1)
  (setq vim-ex--cmd nil
        vim-ex--arg nil
        vim-ex--arg-handler nil
        vim-ex--range nil
        vim-ex--minibuffer (current-buffer)))

(defun vim-ex--teardown! ()
  "Deinitializes the minibuffer for an ex-like mode.
This function should be called whenever the minibuffer is exited."
  (setq vim-ex--minibuffer nil))

(defun vim-ex--start-session ()
  "Initializes the minibuffer when ex-mode is started."
  (vim-ex--setup)
  (remove-hook 'minibuffer-setup-hook #'vim-ex--start-session)
  (add-hook 'after-change-functions #'vim-ex--change nil t))

(defun vim-ex--stop-session ()
  "Deinitializes the minibuffer when ex-mode is stopped."
  (when-let (arg-deactivate (and vim-ex--arg-handler
                                 (vim-arg-handler-deactivate vim-ex--arg-handler)))
    (let ((format (format "vim:ex-change: error when activating handler %s: %%s"
                          vim-ex--arg-handler)))
      (with-demoted-errors format
        (funcall arg-deactivate))))
  (remove-hook 'after-change-functions #'vim-ex--change t)
  (vim-ex--teardown!))

(defun vim-ex-mode-exit ()
  "Calls `minibuffer-complete-and-exit' and cleanup."
  (interactive)
  (vim-ex--stop-session)
  (exit-minibuffer))

(defun vim-ex-mode-abort ()
  "Calls `abort-recursive-edit' and cleanup."
  (interactive)
  (vim-ex--stop-session)
  (abort-recursive-edit))

(defun vim-ex-mode-keyboard-escape-quit ()
  "Calls `keyboard-escape-quit' and cleanup."
  (interactive)
  (vim-ex--stop-session)
  (keyboard-escape-quit))

(defun vim-ex--change (_beg _end _len)
  "Checks if the command or argument changed and informs the
argument handler. Gets called on every minibuffer change."
  (let* ((split (vim-ex--split-cmdline (vim-ex--contents)))
         (cmd   (vim-ex-command-cmd split))
         (arg   (vim-ex-command-arg split))
         (beg   (vim-ex-command-beg split))
         (end   (vim-ex-command-end split)))
    (cond
      ((not (string= vim-ex--cmd cmd))
       ;; command changed, update argument handler ...
       (setq vim-ex--cmd cmd
             vim-ex--arg arg
             vim-ex--range (cons beg end))
       ;; ... deactivate old handler ...
       (when-let (arg-deactivate (and vim-ex--arg-handler
                                      (vim-arg-handler-deactivate vim-ex--arg-handler)))
         (let ((format (format "vim:ex-change: error when activating handler %s: %%s"
                               vim-ex--arg-handler)))
           (with-demoted-errors format
             (funcall arg-deactivate))))
       ;; ... activate and store new handler ...
       (let ((cmd (vim-ex--binding cmd)))
         (cond
           ((eq cmd 'incomplete)
            )
           ((and (not cmd)
                 (not (zerop (length vim-ex--cmd))))
            )
           (t
            (setq vim-ex--arg-handler
                  (and cmd (vim-ex--get-arg-handler cmd)))
            (when-let (arg-activate (and vim-ex--arg-handler
                                         (vim-arg-handler-activate vim-ex--arg-handler)))
              (let ((format (format "vim:ex-change: error when activating handler %s: %%s"
                                    vim-ex--arg-handler)))
                (with-demoted-errors format
                  (funcall arg-activate))))))))
      ((or (not (string= vim-ex--arg arg))
           (not (equal (cons beg end) vim-ex--range)))
       ;; command remained the same, but argument or range changed
       ;; so inform the argument handler
       (setq vim-ex--arg arg)
       (setq vim-ex--range (cons beg end))
       (let ((arg-update (and vim-ex--arg-handler
                              (vim-arg-handler-update vim-ex--arg-handler))))
         (when arg-update (funcall arg-update)))))))

(cl-defstruct vim-ex-command
  (range  nil :read-only t) ;; the string containing the command's range
  (cmd    nil :read-only t) ;; the string for the command itself
  (spaces nil :read-only t) ;; the string containing the spaces between the command and the argument
  (arg    nil :read-only t)
  (beg    nil :read-only t) ;; the start line of the range
  (end    nil :read-only t) ;; the end line of the range
  (force  nil :read-only t) ;; t iff the command was followed by an exclamation mark
  )

(defun vim-ex--split-cmdline (cmdline)
  "Splits the command line in range, command and argument part.
This function returns multiple values

(range cmd spaces arg beg end force)

with the following meanings.
  `range' is the string containing the command's range
  `cmd' is the string for the command itself
  `spaces' is the string containing the spaces between the command and the argument
  `beg' is the start line of the range
  `end' is the end line of the range
  `force' is t iff the command was followed by an exclamation mark"
  (save-match-data
    (cl-multiple-value-bind (cmd-region beg end force) (vim-ex--parse cmdline)
      (if (null cmd-region)
          (make-vim-ex-command
           :range  cmdline
           :cmd    ""
           :spaces cmdline
           :arg    ""
           :beg    beg
           :end    end
           :force  nil)
        (let ((range (substring cmdline 0 (car cmd-region)))
              (cmd (substring cmdline (car cmd-region) (cdr cmd-region)))
              (spaces "")
              (arg (substring cmdline (if force
                                          (1+ (cdr cmd-region))
                                        (cdr cmd-region)))))
          ;; skip whitespaces
          (when (string-match "\\`\\s-*" arg)
            (setq spaces (match-string-no-properties 0 arg)
                  arg (substring arg (match-end 0))))
          (make-vim-ex-command
           :range  range
           :cmd    cmd
           :spaces spaces
           :arg    arg
           :beg    beg
           :end    end
           :force  force))))))

(defun vim-ex--expect-argument (n)
  "Called if the space separating the command from the argument
has been pressed."
  (interactive "p")
  (let ((cmdline (vim-ex--contents)))
    (self-insert-command n)
    (let* ((split  (vim-ex--split-cmdline cmdline))
           (cmd    (vim-ex-command-cmd split))
           (spaces (vim-ex-command-spaces split))
           (arg    (vim-ex-command-arg split)))
      (when (and (= (point) (point-max))
                 (zerop (length spaces))
                 (zerop (length arg)))
        (setq cmd (vim-ex--binding cmd))
        (if (not (vim-ex--binding-p cmd)) (ding)
          (setq vim-ex--cmd cmd)
          (let ((result (vim-ex--complete-argument nil nil nil)))
            (when result (insert result))))))))

(defun vim-ex--complete (cmdline predicate flag)
  "Called to complete an object in the ex-buffer."
  ;; Check that we're in ex mode because ivy may store this function
  ;; in its state and call it at an unexpected moment after ex session
  ;; has ended
  (when (and (bufferp vim-ex--minibuffer)
             (eq (current-buffer) vim-ex--minibuffer))
    (let* ((split  (vim-ex--split-cmdline cmdline))
           (range  (vim-ex-command-range split))
           (cmd    (vim-ex-command-cmd split))
           (spaces (vim-ex-command-spaces split))
           (arg    (vim-ex-command-arg split))
           (force  (vim-ex-command-force split)))
      (setq vim-ex--cmd cmd)
      (cond
        ;; only complete at the end of the command
        ((< (point) (point-max)) nil)
        ;; if at the end of a command, complete the command
        ((and (zerop (length spaces)) (zerop (length arg)))
         ;; We need to take care of the potential force argument `!'.
         ;; If a ! is given only commands which can be forced are
         ;; considered as completion. Furthermore the result has to be
         ;; modified if no `!' has been given in order to show the possible
         ;; `!' completions.
         (let*
             ((pred
               (cond
                 ((not force) predicate)
                 ((not predicate)
                  (lambda (x)
                    (vim--cmd-force-p (vim-ex--binding (car x)))))
                 (t
                  (lambda (x)
                    (and (funcall predicate x)
                         (vim--cmd-force-p (vim-ex--binding (car x))))))))
              (result (vim-ex--complete-command cmd pred flag)))
           (pcase flag
             ;; try-completion, take case of a unique match which
             ;; may take a force argument
             (`nil
              (cl-case result
                ((nil) nil)
                ((t) (if (and (not force)
                              (vim--cmd-force-p (vim-ex--binding cmd)))
                         cmd
                       t))
                (t (if force (concat result "!") result))))
             ;; all-completions, append exclamation marks
             (`t
              (if force
                  (--map (concat it "!") result)
                (let (newresult)
                  (dolist (r result)
                    (push r newresult)
                    (when (vim--cmd-force-p (vim-ex--binding r))
                      (push (concat r "!") newresult)))
                  newresult)))
             ;; test-completion, handle non-unique case if no force
             ;; argument is given but possible for the command
             (_
              (and result
                   (or force
                       (not (vim--cmd-force-p (vim-ex--binding cmd)))))))))
        ;; otherwise complete the argument
        (t
         (let ((result (vim-ex--complete-argument arg predicate flag)))
           (cond
             ((null result) nil)
             ((eq t result) t)
             ((stringp result) (if flag result (concat range cmd spaces result)))
             ((listp result) (if flag
                                 result
                               (--map (concat range cmd spaces it)
                                      result)))
             (t (error "Completion returned unexpected value")))))))))

(defun vim-ex--complete-command (cmd predicate flag)
  "Called to complete the current command."
  (with-current-buffer vim-ex--current-buffer
    (cond
      ((null flag) (or (try-completion cmd vim-ex--local-commands predicate)
                       (try-completion cmd vim-ex--commands predicate)))
      ((eq t flag) (append (all-completions cmd vim-ex--local-commands predicate)
                           (all-completions cmd vim-ex--commands predicate)))
      ((eq 'lambda flag) (or (test-completion cmd vim-ex--local-commands predicate)
                             (test-completion cmd vim-ex--commands predicate))))))

(defun vim-ex--complete-argument (arg predicate flag)
  "Called to complete the current argument w.r.t. the current command."
  (let* ((cmd vim-ex--cmd)
         (arg-handler (vim-ex--get-arg-handler cmd)))
    (if arg-handler
      (funcall (or (vim-arg-handler-complete arg-handler)
                   #'vim-ex--complete-text-argument)
               arg predicate flag)
      (vim-ex--complete-text-argument arg predicate flag))))

(defun vim-ex--complete-file-argument (arg _predicate flag)
  "Called to complete a file argument."
  (if (null arg)
    default-directory
    (let ((dir (or (file-name-directory arg)
                   (with-current-buffer vim-ex--current-buffer default-directory)))
          (fname (file-name-nondirectory arg)))
      (cond
        ((null dir) (ding))
        ((null flag)
         (let ((result (file-name-completion fname dir)))
           (pcase result
             (`nil nil)
             (`t   t)
             (_    (concat dir result)))))
        ((eq t flag)
         (file-name-all-completions fname dir))
        ((eq 'lambda flag)
         (eq (file-name-completion fname dir) t))))))

(defun vim-ex--complete-buffer-argument (arg predicate flag)
  "Called to complete a buffer name argument."
  (when arg
    (let ((buffers (--map (cons (buffer-name it) nil)
                          (buffer-list t))))
      (cond
        ((null flag)
         (try-completion arg buffers predicate))
        ((eq t flag)
         (all-completions arg buffers predicate))
        ((eq 'lambda flag)
         (test-completion arg buffers predicate))))))

(defun vim-ex--complete-text-argument (arg _predicate flag)
  "Called to complete standard argument, therefore does nothing."
  (when arg
    (pcase flag
      (`nil    t)
      (`t      (list arg))
      (`lambda t))))

(defun vim-ex--strip-ex-info (str)
  "Remove info part from string STR."
  (let ((info-start (text-property-any 0
                                       (length str)
                                       'ex-info
                                       t
                                       str)))
    (if info-start
        (substring str 0 info-start)
      str)))

(defun vim-ex-execute-command (cmdline)
  "Called to execute the current command."
  (interactive)
  (let* ((split      (vim-ex--split-cmdline cmdline))
         (cmd        (vim-ex-command-cmd split))
         (arg        (vim-ex-command-arg split))
         (start-line (vim-ex-command-beg split))
         (end-line   (vim-ex-command-end split))
         (force      (vim-ex-command-force split)))
    (setq vim-ex--cmd cmd)
    (setf arg (vim-ex--strip-ex-info arg))
    (let ((cmd vim-ex--cmd)
          (motion (cond
                    ((and start-line end-line)
                     (vim-make-motion :begin (save-excursion
                                               (goto-line-dumb start-line)
                                               (line-beginning-position))
                                      :end (save-excursion
                                             (goto-line-dumb end-line)
                                             (line-beginning-position))
                                      :has-begin t
                                      :type 'linewise))
                    (start-line
                     (let ((beg-pos (save-excursion
                                      (goto-line-dumb start-line)
                                      (line-beginning-position))))
                       (vim-make-motion :begin beg-pos
                                        :end beg-pos
                                        :has-begin t
                                        :type 'linewise)))))
          (count (and (not end-line) start-line)))
      (setq cmd (vim-ex--binding cmd))
      (when (zerop (length arg))
        (setq arg nil))
      (with-current-buffer vim-ex--current-buffer
        (let (parameters)
          (cond
            ((vim-ex--binding-p cmd)
             (when (vim--cmd-arg-p cmd)
               (setq parameters (cons :argument (cons arg parameters))))
             (when force
               (if (vim--cmd-force-p cmd)
                   (setq parameters (cons :force (cons t parameters)))
                 (error "Command cannot be forced '!'")))
             (pcase (vim--cmd-type cmd)
               (`complex (setq parameters
                               (cons :motion (cons motion parameters))))
               (`simple
                (when end-line
                  (error "Command does not take a range: %s" vim-ex--cmd))
                (when (vim--cmd-count-p cmd)
                  (setq parameters
                        (cons :count (cons (or count
                                               (and arg
                                                    (not (vim--cmd-arg-p cmd))
                                                    (string-to-number arg)))
                                           parameters)))))
               (`nil (error "Command '%s' binds undefined function" vim-ex--cmd))
               (invalid (error "Unexpected command-type bound to %s: %s" vim-ex--cmd invalid)))
             (apply cmd parameters))
            (start-line
             (vim:motion-go-to-first-non-blank-beg :count (or end-line start-line)))
            (t
             (error "Unknown command: %s" (if (zerop (length vim-ex--cmd))
                                              cmdline
                                            vim-ex--cmd)))))))))

;; parser for ex-commands
(defun vim-ex--parse (text)
  "Extracts the range-information from `text'.
Returns four values: (cmd beg end force) where
  `cmd' is a pair of indices for the command substring
  `beg' is the first line of the index range
  `end' is the last line of the index range
  `force' is non-nil iff an exclamation mark followed the command."
  (save-match-data
    (let (begin
          (begin-off 0)
          sep
          end
          (end-off 0)
          (pos 0)
          cmd)
      (cl-multiple-value-bind (beg npos) (vim-ex--parse-address text pos)
        (when npos
          (setq begin beg
                pos npos)))
      (cl-multiple-value-bind (off npos) (vim-ex--parse-offset text pos)
        (when npos
          (unless begin (setq begin 'current-line))
          (setq begin-off off
                pos npos)))
      (when (and (< pos (length text))
                 (or (= (aref text pos) ?\,)
                     (= (aref text pos) ?\;)))
        (setq sep (aref text pos))
        (cl-incf pos)
        (cl-multiple-value-bind (e npos) (vim-ex--parse-address text pos)
          (when npos
            (setq end e
                  pos npos)))
        (cl-multiple-value-bind (off npos) (vim-ex--parse-offset text pos)
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
      (when (= pos (or (string-match (vim-ex--known-commands-re)
                                     text
                                     pos)
                       -1))
        (setq cmd (cons (match-beginning 1) (match-end 1))))
      (cl-multiple-value-bind (start end)
          (vim-ex--get-range (and begin (cons begin begin-off))
                             sep
                             (and end (cons end end-off)))
        (values cmd start end (match-beginning 2))))))

(defun vim-ex--known-commands-re ()
  "Get regexps that matches all ex commands that are known in current context."
  (if vim-ex--all-known-local-ex-commands
      (progn
        (unless vim-ex--all-known-local-and-global-ex-commands
          (setf vim-ex--all-known-local-and-global-ex-commands
                (append vim-ex--all-known-local-ex-commands
                        vim-ex--all-known-global-ex-commands)))
        (unless vim-ex--local-commands-re-cache
          (setf vim-ex--local-commands-re-cache
                (concat "\\("
                        (regexp-opt vim-ex--all-known-local-and-global-ex-commands)
                        "\\)\\(!\\)?")))
        vim-ex--local-commands-re-cache)
    (progn
      (unless vim-ex--global-commands-re-cache
        (setf vim-ex--global-commands-re-cache
              (concat "\\("
                      (regexp-opt vim-ex--all-known-global-ex-commands)
                      "\\)\\(!\\)?")))
      vim-ex--global-commands-re-cache)))

(defun vim-ex--parse-address (text pos)
  "Parses `text' starting at `pos' for an address, returning a two values,
the range and the new position."
  (cond
    ((>= pos (length text)) nil)
    ((= pos (or (string-match "[0-9]+" text pos) -1))
     (values (cons 'abs (string-to-number (match-string-no-properties 0 text)))
             (match-end 0)))
    ((= (aref text pos) ?\$)
     (values 'last-line (1+ pos)))
    ((= (aref text pos) ?\%)
     (values 'all (1+ pos)))
    ((= (aref text pos) ?\.)
     (values 'current-line (1+ pos)))
    ((= (aref text pos) ?\')
     (if (>= (1+ pos) (length text))
       nil
       (values `(mark ,(aref text (1+ pos))) (+ 2 pos))))
    ((= (aref text pos) ?\/)
     (when (string-match "\\([^/]+\\|\\\\.\\)\\(?:/\\|$\\)"
                         text (1+ pos))
       (values (cons 're-fwd (match-string-no-properties 1 text))
               (match-end 0))))
    ((= (aref text pos) ?\?)
     (when (string-match "\\([^?]+\\|\\\\.\\)\\(?:?\\|$\\)"
                         text (1+ pos))
       (values (cons 're-bwd (match-string-no-properties 1 text))
               (match-end 0))))
    ((and (= (aref text pos) ?\\)
          (< pos (1- (length text))))
     (cl-case (aref text (1+ pos))
       (?\/ (values 'next-of-prev-search (1+ pos)))
       (?\? (values 'prev-of-prev-search (1+ pos)))
       (?\& (values 'next-of-prev-subst (1+ pos)))))
    (t nil)))

(defun vim-ex--parse-offset (text pos)
  "Parses `text' starting at `pos' for an offset, returning a two values,
the offset and the new position."
  (let ((off nil))
    (while (= pos (or (string-match "\\([-+]\\)\\([0-9]+\\)?" text pos) -1))
      (if (string= (match-string-no-properties 1 text) "+")
          (setq off (+ (or off 0) (if (match-beginning 2)
                                      (string-to-number (match-string-no-properties 2 text))
                                    1)))

        (setq off (- (or off 0) (if (match-beginning 2)
                                    (string-to-number (match-string-no-properties 2 text))
                                  1))))
      (setq pos (match-end 0)))
    (and off (values off pos))))

(defun vim-ex--get-range (start sep end)
  "Must be called from within ex buffer."
  (with-current-buffer vim-ex--current-buffer
    (when start
      (setq start (vim-ex--get-line start)))
    (when (and sep end)
      (save-excursion
        (when (= sep ?\;)
          (goto-line-dumb start))
        (setq end (vim-ex--get-line end))))
    (values start end)))

(defun vim-ex--get-line (address)
  (let ((base (car address))
        (offset (cdr address)))
    (cond
      ((null base) nil)
      ((consp offset)
       (let ((line (vim-ex--get-line (car address))))
         (when line
           (save-excursion
             (goto-line-dumb line)
             (vim-ex--get-line (cdr address))))))
      (t
       (+ offset
          ;; NB car-safe is essential here to ignore non-lists
          (pcase (or (car-safe base) base)
            (`abs (cdr base))
            ;; TODO: (1- ...) may be wrong if the match is the empty string
            (`re-fwd (save-excursion
                       (beginning-of-line 2)
                       (and (re-search-forward (cdr base))
                            (line-number-at-pos (1- (match-end 0))))))
            (`re-bwd (save-excursion
                       (beginning-of-line 0)
                       (and (re-search-backward (cdr base))
                            (line-number-at-pos (match-beginning 0)))))
            (`current-line        (line-number-at-pos (point)))
            (`first-line          (line-number-at-pos (point-min)))
            (`last-line           (line-number-at-pos (point-max)))
            (`mark                (line-number-at-pos (vim-get-local-mark (cadr base))))
            (`next-of-prev-search (error "Next-of-prev-search not yet implemented"))
            (`prev-of-prev-search (error "Prev-of-prev-search not yet implemented"))
            (`next-of-prev-subst  (error "Next-of-prev-subst not yet implemented"))
            (_                    (error "Invalid address: %s" address))))))))

(defconst vim-ex--prompt ">"
  "Prompt shape for ex mode.")

(defun vim-ex-read-command (&optional initial-input)
  "Starts ex-mode."
  (interactive)
  (let ((vim-ex--current-buffer (current-buffer))
        (vim-ex--current-window (selected-window)))
    (let ((minibuffer-local-completion-map vim-ex-keymap)
          ;; We will add user input to history ourselves, if it's long enough.
          (history-add-new-input nil))
      (add-hook 'minibuffer-setup-hook #'vim-ex--start-session)
      (let ((result
             (completing-read-default vim-ex--prompt
                                      (vim-ex--get-known-commands)
                                      nil
                                      nil
                                      initial-input
                                      'vim-ex--history)))
        (when result
          ;; Filter out commands like "w" and "hs"
          (when (< 2 (length result))
            (push result vim-ex--history))
          (vim-ex-execute-command result))))))

(defun vim-ex--get-known-commands ()
  (if vim-ex--all-known-local-ex-commands
      (progn
        (unless vim-ex--all-known-local-and-global-ex-commands
          (setf vim-ex--all-known-local-and-global-ex-commands
                (append vim-ex--all-known-local-ex-commands
                        vim-ex--all-known-global-ex-commands)))
        vim-ex--all-known-local-and-global-ex-commands)
    vim-ex--all-known-global-ex-commands))

(provide 'vim-ex)

;; Local Variables:
;; End:

;; vim-ex.el ends here
