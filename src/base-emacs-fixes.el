;; base-emacs-fixes.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 12 May 2013
;; Description:
;; Fixes for errors in standard emacs files

(eval-when-compile
  (require 'cl)
  (require 'set-up-platform)
  (require 'el-patch)
  (require 'macro-util))

(require 'el-patch)

;; added cl-remove-duplicates to avoid scenario when two identical
;; hooks get called
;; (defun run-mode-hooks (&rest hooks)
;;   "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
;; If the variable `delay-mode-hooks' is non-nil, does not run any hooks,
;; just adds the HOOKS to the list `delayed-mode-hooks'.
;; Otherwise, runs hooks in the sequence: `change-major-mode-after-body-hook',
;; `delayed-mode-hooks' (in reverse order), HOOKS, and finally
;; `after-change-major-mode-hook'.  Major mode functions should use
;; this instead of `run-hooks' when running their FOO-mode-hook."
;;   (if delay-mode-hooks
;;       ;; Delaying case.
;;       (dolist (hook hooks)
;;         (push hook delayed-mode-hooks))
;;     ;; Normal case, just run the hook as before plus any delayed hooks.
;;     (setq hooks (cl-remove-duplicates (nconc (nreverse delayed-mode-hooks) hooks)
;;                                       :test #'eq))
;;     (setq delayed-mode-hooks nil)
;;     (apply 'run-hooks (cons 'change-major-mode-after-body-hook hooks))
;;     (run-hooks 'after-change-major-mode-hook)))

;; added cl-remove-duplicates to avoid scenario when two identical
;; hooks get called

(when-emacs-version (= it 25)
  (el-patch-defun run-mode-hooks (&rest hooks)
    "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
If the variable `delay-mode-hooks' is non-nil, does not run any hooks,
just adds the HOOKS to the list `delayed-mode-hooks'.
Otherwise, runs hooks in the sequence: `change-major-mode-after-body-hook',
`delayed-mode-hooks' (in reverse order), HOOKS, and finally
`after-change-major-mode-hook'.  Major mode functions should use
this instead of `run-hooks' when running their FOO-mode-hook."
    (if delay-mode-hooks
        ;; Delaying case.
        (dolist (hook hooks)
          (push hook delayed-mode-hooks))
      ;; Normal case, just run the hook as before plus any delayed hooks.
      (setq hooks (el-patch-swap
                    (nconc (nreverse delayed-mode-hooks) hooks)
                    (cl-remove-duplicates (nconc (nreverse delayed-mode-hooks) hooks)
                                          :test #'eq)))
      (setq delayed-mode-hooks nil)
      (apply 'run-hooks (cons 'change-major-mode-after-body-hook hooks))
      (run-hooks 'after-change-major-mode-hook))))

(when-emacs-version (= it 26)
  (el-patch-defun run-mode-hooks (&rest hooks)
    "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
Call `hack-local-variables' to set up file local and directory local
variables.

If the variable `delay-mode-hooks' is non-nil, does not do anything,
just adds the HOOKS to the list `delayed-mode-hooks'.
Otherwise, runs hooks in the sequence: `change-major-mode-after-body-hook',
`delayed-mode-hooks' (in reverse order), HOOKS, then runs
`hack-local-variables', runs the hook `after-change-major-mode-hook', and
finally evaluates the functions in `delayed-after-hook-functions' (see
`define-derived-mode').

Major mode functions should use this instead of `run-hooks' when
running their FOO-mode-hook."
    (if delay-mode-hooks
        ;; Delaying case.
        (dolist (hook hooks)
          (push hook delayed-mode-hooks))
      ;; Normal case, just run the hook as before plus any delayed hooks.
      (setq hooks (el-patch-swap
                    (nconc (nreverse delayed-mode-hooks) hooks)
                    (cl-remove-duplicates (nconc (nreverse delayed-mode-hooks) hooks)
                                          :test #'eq)))
      (setq delayed-mode-hooks nil)
      (apply 'run-hooks (cons 'change-major-mode-after-body-hook hooks))
      (if (buffer-file-name)
          (with-demoted-errors "File local-variables error: %s"
            (hack-local-variables 'no-mode)))
      (run-hooks 'after-change-major-mode-hook)
      (dolist (fun (nreverse delayed-after-hook-functions))
        (funcall fun))
      (setq delayed-after-hook-functions nil))))

(when-emacs-version (= 28 it)
  (el-patch-defun run-mode-hooks (&rest hooks)
    "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
Call `hack-local-variables' to set up file local and directory local
variables.

If the variable `delay-mode-hooks' is non-nil, does not do anything,
just adds the HOOKS to the list `delayed-mode-hooks'.
Otherwise, runs hooks in the sequence: `change-major-mode-after-body-hook',
`delayed-mode-hooks' (in reverse order), HOOKS, then runs
`hack-local-variables', runs the hook `after-change-major-mode-hook', and
finally evaluates the functions in `delayed-after-hook-functions' (see
`define-derived-mode').

Major mode functions should use this instead of `run-hooks' when
running their FOO-mode-hook."
    (if delay-mode-hooks
        ;; Delaying case.
        (dolist (hook hooks)
          (push hook delayed-mode-hooks))
      ;; Normal case, just run the hook as before plus any delayed hooks.
      (setq hooks (el-patch-swap
                    (nconc (nreverse delayed-mode-hooks) hooks)
                    (cl-remove-duplicates (nconc (nreverse delayed-mode-hooks) hooks)
                                          :test #'eq)))
      (and (bound-and-true-p syntax-propertize-function)
           (not (local-variable-p 'parse-sexp-lookup-properties))
           ;; `syntax-propertize' sets `parse-sexp-lookup-properties' for us, but
           ;; in order for the sexp primitives to automatically call
           ;; `syntax-propertize' we need `parse-sexp-lookup-properties' to be
           ;; set first.
           (setq-local parse-sexp-lookup-properties t))
      (setq delayed-mode-hooks nil)
      (apply #'run-hooks (cons 'change-major-mode-after-body-hook hooks))
      (if (buffer-file-name)
          (with-demoted-errors "File local-variables error: %s"
            (hack-local-variables 'no-mode)))
      (run-hooks 'after-change-major-mode-hook)
      (dolist (fun (prog1 (nreverse delayed-after-hook-functions)
                     (setq delayed-after-hook-functions nil)))
        (funcall fun)))))

(when-emacs-version (<= 29 it)
  (el-patch-defun run-mode-hooks (&rest hooks)
    "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
Call `hack-local-variables' to set up file local and directory local
variables.

If the variable `delay-mode-hooks' is non-nil, does not do anything,
just adds the HOOKS to the list `delayed-mode-hooks'.
Otherwise, runs hooks in the sequence: `change-major-mode-after-body-hook',
`delayed-mode-hooks' (in reverse order), HOOKS, then runs
`hack-local-variables' (if the buffer is visiting a file),
runs the hook `after-change-major-mode-hook', and finally
evaluates the functions in `delayed-after-hook-functions' (see
`define-derived-mode').

Major mode functions should use this instead of `run-hooks' when
running their FOO-mode-hook."
    (if delay-mode-hooks
        ;; Delaying case.
        (dolist (hook hooks)
	  (push hook delayed-mode-hooks))
      ;; Normal case, just run the hook as before plus any delayed hooks.
      (setq hooks (el-patch-swap
                    (nconc (nreverse delayed-mode-hooks) hooks)
                    (cl-remove-duplicates (nconc (nreverse delayed-mode-hooks) hooks)
                                          :test #'eq)))
      (and (bound-and-true-p syntax-propertize-function)
           (not (local-variable-p 'parse-sexp-lookup-properties))
           ;; `syntax-propertize' sets `parse-sexp-lookup-properties' for us, but
           ;; in order for the sexp primitives to automatically call
           ;; `syntax-propertize' we need `parse-sexp-lookup-properties' to be
           ;; set first.
           (setq-local parse-sexp-lookup-properties t))
      (setq delayed-mode-hooks nil)
      (apply #'run-hooks (cons 'change-major-mode-after-body-hook hooks))
      (if (buffer-file-name)
          (with-demoted-errors "File local-variables error: %s"
            (hack-local-variables 'no-mode)))
      (run-hooks 'after-change-major-mode-hook)
      (dolist (fun (prog1 (nreverse delayed-after-hook-functions)
                     (setq delayed-after-hook-functions nil)))
        (funcall fun)))))

;;;###autoload
(el-patch-feature autorevert)

(defun autorevert-init ()
  (when-emacs-version (<= 28 it)
    (el-patch-defun auto-revert-notify-add-watch ()
      "Enable file notification for current buffer's associated file."
      ;; We can assume that `auto-revert-notify-watch-descriptor' is nil.
      (unless (or auto-revert-notify-watch-descriptor
                  (el-patch-wrap 2 0
                    (and default-directory
                         ((el-patch-swap string-match string-match-p)
                          auto-revert-notify-exclude-dir-regexp
                          (expand-file-name default-directory))))
                  (el-patch-wrap 2 0
                    (and (or buffer-file-name default-directory)
                         (file-symlink-p (or buffer-file-name default-directory)))))
        (let ((file (if buffer-file-name
		        (expand-file-name buffer-file-name default-directory)
	              (expand-file-name default-directory))))
          (setq auto-revert-notify-watch-descriptor
	        (ignore-errors
	          (file-notify-add-watch
	           file
                   (if buffer-file-name '(change attribute-change) '(change))
                   'auto-revert-notify-handler))))
        (when auto-revert-notify-watch-descriptor
          (setq auto-revert-notify-modified-p t
                auto-revert--buffer-by-watch-descriptor
                (cons (cons auto-revert-notify-watch-descriptor (current-buffer))
                      auto-revert--buffer-by-watch-descriptor))
          (add-hook 'kill-buffer-hook #'auto-revert-notify-rm-watch nil t))))))

(eval-after-load "autorevert" '(autorevert-init))

(when-emacs-version (or (= it 25) (= it 26))
  (el-patch-defun push-mark (&optional location nomsg activate)
    "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information.

In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil."
    (unless (null (mark t))
      (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
      (when (> (length mark-ring) mark-ring-max)
        (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
        (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil)))
    (set-marker (mark-marker) (or location (point)) (current-buffer))
    ;; Now push the mark on the global mark ring.
    (if (and global-mark-ring
             (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
        ;; The last global mark pushed was in this same buffer.
        ;; Don't push another one.
        nil
      (setq global-mark-ring (cons (copy-marker (mark-marker)) global-mark-ring))
      (when (> (length global-mark-ring) global-mark-ring-max)
        (move-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
        (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil)))
    (el-patch-remove
      (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
          (message "Mark set")))
    (if (or activate (not transient-mark-mode))
        (set-mark (mark t)))
    nil))

(when-emacs-version (<= 27 it)
  (el-patch-defun push-mark (&optional location nomsg activate)
    "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information.

In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil."
    (when (mark t)
      (let ((old (nth mark-ring-max mark-ring))
            (history-delete-duplicates nil))
        (add-to-history 'mark-ring (copy-marker (mark-marker)) mark-ring-max t)
        (when old
          (set-marker old nil))))
    (set-marker (mark-marker) (or location (point)) (current-buffer))
    ;; Don't push the mark on the global mark ring if the last global
    ;; mark pushed was in this same buffer.
    (unless (and global-mark-ring
                 (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
      (let ((old (nth global-mark-ring-max global-mark-ring))
            (history-delete-duplicates nil))
        (add-to-history
         'global-mark-ring (copy-marker (mark-marker)) global-mark-ring-max t)
        (when old
          (set-marker old nil))))
    (el-patch-remove
      (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
          (message "Mark set")))
    (if (or activate (not transient-mark-mode))
        (set-mark (mark t)))
    nil))

(autoload 'ansi-color-apply-on-region "ansi-color")

(when-emacs-version (= 28 it)
  (el-patch-defun shell-command-on-region (start end command
				                 &optional output-buffer replace
				                 error-buffer display-error-buffer
				                 region-noncontiguous-p)
    "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer specified
by `shell-command-buffer-name'; prefix arg means replace the region
with it.  Return the exit code of COMMAND.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell command, use \\[universal-coding-system-argument]
before this command.  By default, the input (from the current buffer)
is encoded using coding-system specified by `process-coding-system-alist',
falling back to `default-process-coding-system' if no match for COMMAND
is found in `process-coding-system-alist'.

Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

If the command generates output, the output may be displayed
in the echo area or in a buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.
Otherwise it is displayed in the buffer named by `shell-command-buffer-name'.
The output is available in that buffer in both cases.

If there is output and an error, a message about the error
appears at the end of the output.

Optional fourth arg OUTPUT-BUFFER specifies where to put the
command's output.  If the value is a buffer or buffer name,
erase that buffer and insert the output there; a non-nil value of
`shell-command-dont-erase-buffer' prevent to erase the buffer.
If the value is nil, use the buffer specified by `shell-command-buffer-name'.
Any other non-nil value means to insert the output in the
current buffer after START.

Optional fifth arg REPLACE, if non-nil, means to insert the
output in place of text from START to END, putting point and mark
around it.  If REPLACE is the symbol `no-mark', don't set the mark.

Optional sixth arg ERROR-BUFFER, if non-nil, specifies a buffer
or buffer name to which to direct the command's standard error
output.  If nil, error output is mingled with regular output.
When called interactively, `shell-command-default-error-buffer'
is used for ERROR-BUFFER.

Optional seventh arg DISPLAY-ERROR-BUFFER, if non-nil, means to
display the error buffer if there were any errors.  When called
interactively, this is t.

Non-nil REGION-NONCONTIGUOUS-P means that the region is composed of
noncontiguous pieces.  The most common example of this is a
rectangular region, where the pieces are separated by newline
characters."
    (interactive (let (string)
		   (unless (mark)
		     (user-error "The mark is not set now, so there is no region"))
                   ;; Do this before calling region-beginning
                   ;; and region-end, in case subprocess output
                   ;; relocates them while we are in the minibuffer.
		   (setq string (read-shell-command "Shell command on region: "))
                   ;; call-interactively recognizes region-beginning and
                   ;; region-end specially, leaving them in the history.
		   (list (region-beginning) (region-end)
		         string
		         current-prefix-arg
		         current-prefix-arg
		         shell-command-default-error-buffer
		         t
		         (region-noncontiguous-p))))
    (let ((error-file
	   (if error-buffer
	       (make-temp-file
	        (expand-file-name "scor"
				  (or small-temporary-file-directory
				      temporary-file-directory)))
	     nil))
	  exit-status)
      ;; Unless a single contiguous chunk is selected, operate on multiple chunks.
      (if region-noncontiguous-p
          (let ((input (concat (funcall region-extract-function (when replace 'delete)) "\n"))
                output)
            (with-temp-buffer
              (insert input)
              (call-process-region (point-min) (point-max)
                                   shell-file-name t t
                                   nil shell-command-switch
                                   command)
              (setq output (split-string (buffer-substring
                                          (point-min)
                                          ;; Trim the trailing newline.
                                          (if (eq (char-before (point-max)) ?\n)
                                              (1- (point-max))
                                            (point-max)))
                                         "\n")))
            (cond
              (replace
               (goto-char start)
               (funcall region-insert-function output))
              (t
               (let ((buffer (get-buffer-create
                              (or output-buffer shell-command-buffer-name))))
                 (with-current-buffer buffer
                   (erase-buffer)
                   (funcall region-insert-function output))
                 (display-message-or-buffer buffer)))))
        (if (or replace
                (and output-buffer
                     (not (or (bufferp output-buffer) (stringp output-buffer)))))
            ;; Replace specified region with output from command.
            (let ((swap (and replace (< start end))))
              ;; Don't muck with mark unless REPLACE says we should.
              (goto-char start)
              (when (and replace
                         (not (eq replace 'no-mark)))
                (push-mark (point) 'nomsg))
              (setq exit-status
                    (call-shell-region start end command replace
                                       (if error-file
                                           (list t error-file)
                                         t)))
              ;; It is rude to delete a buffer that the command is not using.
              ;; (let ((shell-buffer (get-buffer shell-command-buffer-name)))
              ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
              ;; 	 (kill-buffer shell-buffer)))
              ;; Don't muck with mark unless REPLACE says we should.
              (when (and replace swap
                         (not (eq replace 'no-mark)))
                (exchange-point-and-mark)))
          ;; No prefix argument: put the output in a temp buffer,
          ;; replacing its entire contents.
          (let ((buffer (get-buffer-create
                         (or output-buffer shell-command-buffer-name))))
            (set-buffer-major-mode buffer) ; Enable globalized modes (bug#38111)
            (unwind-protect
                (if (and (eq buffer (current-buffer))
                         (or (memq shell-command-dont-erase-buffer '(nil erase))
                             (and (not (eq buffer (get-buffer
                                                   shell-command-buffer-name)))
                                  (not (region-active-p)))))
                    ;; If the input is the same buffer as the output,
                    ;; delete everything but the specified region,
                    ;; then replace that region with the output.
                    (progn (setq buffer-read-only nil)
                           (delete-region (max start end) (point-max))
                           (delete-region (point-min) (min start end))
                           (setq exit-status
                                 (call-process-region (point-min) (point-max)
                                                      shell-file-name t
                                                      (if error-file
                                                          (list t error-file)
                                                        t)
                                                      nil shell-command-switch
                                                      command)))
                  ;; Clear the output buffer, then run the command with
                  ;; output there.
                  (let ((directory default-directory))
                    (with-current-buffer buffer
                      (if (not output-buffer)
                          (setq default-directory directory))
                      (shell-command-save-pos-or-erase)))
                  (setq exit-status
                        (call-shell-region start end command nil
                                           (if error-file
                                               (list buffer error-file)
                                             buffer))))
              ;; Report the output.
              (with-current-buffer buffer
                (setq-local revert-buffer-function
                            (lambda (&rest _)
                              (shell-command command)))
                (setq mode-line-process
                      (cond ((null exit-status)
                             " - Error")
                            ((stringp exit-status)
                             (format " - Signal [%s]" exit-status))
                            ((not (equal 0 exit-status))
                             (format " - Exit [%d]" exit-status)))))
              (if (with-current-buffer buffer (> (point-max) (point-min)))
                  ;; There's some output, display it
                  (progn
                    (el-patch-add
                      ;; colorize output
                      (with-current-buffer buffer
                        (ansi-color-apply-on-region (point-min) (point-max))))
                    (display-message-or-buffer buffer)
                    (shell-command-set-point-after-cmd buffer))
                ;; No output; error?
                (let ((output
                       (if (and error-file
                                (< 0 (file-attribute-size
				      (file-attributes error-file))))
                           (format "some error output%s"
                                   (if shell-command-default-error-buffer
                                       (format " to the \"%s\" buffer"
                                               shell-command-default-error-buffer)
                                     ""))
                         "no output")))
                  (cond ((null exit-status)
                         (message "(Shell command failed with error)"))
                        ((equal 0 exit-status)
                         (message "(Shell command succeeded with %s)"
                                  output))
                        ((stringp exit-status)
                         (message "(Shell command killed by signal %s)"
                                  exit-status))
                        (t
                         (message "(Shell command failed with code %d and %s)"
                                  exit-status output))))
                ;; Don't kill: there might be useful info in the undo-log.
                ;; (kill-buffer buffer)
                )))))

      (when (and error-file (file-exists-p error-file))
        (if (< 0 (file-attribute-size (file-attributes error-file)))
	    (with-current-buffer (get-buffer-create error-buffer)
              (goto-char (point-max))
              ;; Insert a separator if there's already text here.
	      (unless (bobp)
	        (insert "\f\n"))
              ;; Do no formatting while reading error file,
              ;; because that can run a shell command, and we
              ;; don't want that to cause an infinite recursion.
	      (format-insert-file error-file nil)
	      (and display-error-buffer
		   (display-buffer (current-buffer)))))
        (delete-file error-file))
      exit-status)))

(when-emacs-version (= 29 it)
  (el-patch-defun shell-command-on-region (start end command
				                 &optional output-buffer replace
				                 error-buffer display-error-buffer
				                 region-noncontiguous-p)
    "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer specified
by `shell-command-buffer-name'; prefix arg means replace the region
with it.  Return the exit code of COMMAND.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell command, use \\[universal-coding-system-argument]
before this command.  By default, the input (from the current buffer)
is encoded using coding-system specified by `process-coding-system-alist',
falling back to `default-process-coding-system' if no match for COMMAND
is found in `process-coding-system-alist'.

Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

If the command generates output, the output may be displayed
in the echo area or in a buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.
Otherwise it is displayed in the buffer named by `shell-command-buffer-name'.
The output is available in that buffer in both cases.
Note that if `shell-command-dont-erase-buffer' is non-nil,
the echo area could display more than just the output of the
last command.

If there is output and an error, a message about the error
appears at the end of the output.

Optional fourth arg OUTPUT-BUFFER specifies where to put the
command's output.  If the value is a buffer or buffer name,
erase that buffer and insert the output there; a non-nil value of
`shell-command-dont-erase-buffer' prevent to erase the buffer.
If the value is nil, use the buffer specified by `shell-command-buffer-name'.
Any other non-nil value means to insert the output in the
current buffer after START.

Optional fifth arg REPLACE, if non-nil, means to insert the
output in place of text from START to END, putting point and mark
around it.  If REPLACE is the symbol `no-mark', don't set the mark.

Optional sixth arg ERROR-BUFFER, if non-nil, specifies a buffer
or buffer name to which to direct the command's standard error
output.  If nil, error output is mingled with regular output.
When called interactively, `shell-command-default-error-buffer'
is used for ERROR-BUFFER.

Optional seventh arg DISPLAY-ERROR-BUFFER, if non-nil, means to
display the error buffer if there were any errors.  When called
interactively, this is t.

Non-nil REGION-NONCONTIGUOUS-P means that the region is composed of
noncontiguous pieces.  The most common example of this is a
rectangular region, where the pieces are separated by newline
characters."
    (interactive (let (string)
		   (unless (mark)
		     (user-error "The mark is not set now, so there is no region"))
		   ;; Do this before calling region-beginning
		   ;; and region-end, in case subprocess output
		   ;; relocates them while we are in the minibuffer.
		   (setq string (read-shell-command "Shell command on region: "))
		   ;; call-interactively recognizes region-beginning and
		   ;; region-end specially, leaving them in the history.
		   (list (region-beginning) (region-end)
		         string
		         current-prefix-arg
		         current-prefix-arg
		         shell-command-default-error-buffer
		         t
		         (region-noncontiguous-p))))
    (let ((error-file
	   (if error-buffer
	       (make-temp-file
	        (expand-file-name "scor"
				  (or small-temporary-file-directory
				      temporary-file-directory)))
	     nil))
	  exit-status)
      ;; Unless a single contiguous chunk is selected, operate on multiple chunks.
      (if region-noncontiguous-p
          (let ((input (concat (funcall region-extract-function (when replace 'delete)) "\n"))
                output)
            (with-temp-buffer
              (insert input)
              (call-process-region (point-min) (point-max)
                                   shell-file-name t t
                                   nil shell-command-switch
                                   command)
              (setq output (split-string (buffer-substring
                                          (point-min)
                                          ;; Trim the trailing newline.
                                          (if (eq (char-before (point-max)) ?\n)
                                              (1- (point-max))
                                            (point-max)))
                                         "\n")))
            (cond
              (replace
               (goto-char start)
               (funcall region-insert-function output))
              (t
               (let ((buffer (get-buffer-create
                              (or output-buffer shell-command-buffer-name))))
                 (with-current-buffer buffer
                   (erase-buffer)
                   (funcall region-insert-function output))
                 (display-message-or-buffer buffer)))))
        (if (or replace
                (and output-buffer
                     (not (or (bufferp output-buffer) (stringp output-buffer)))))
            ;; Replace specified region with output from command.
            (let ((swap (and replace (< start end))))
              ;; Don't muck with mark unless REPLACE says we should.
              (goto-char start)
              (when (and replace
                         (not (eq replace 'no-mark)))
                (push-mark (point) 'nomsg))
              (setq exit-status
                    (call-shell-region start end command replace
                                       (if error-file
                                           (list t error-file)
                                         t)))
              ;; It is rude to delete a buffer that the command is not using.
              ;; (let ((shell-buffer (get-buffer shell-command-buffer-name)))
              ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
              ;; 	 (kill-buffer shell-buffer)))
              ;; Don't muck with mark unless REPLACE says we should.
              (when (and replace swap
                         (not (eq replace 'no-mark)))
                (exchange-point-and-mark)))
          ;; No prefix argument: put the output in a temp buffer,
          ;; replacing its entire contents.
          (let ((buffer (get-buffer-create
                         (or output-buffer shell-command-buffer-name))))
            (set-buffer-major-mode buffer) ; Enable globalized modes (bug#38111)
            (unwind-protect
                (if (and (eq buffer (current-buffer))
                         (or (memq shell-command-dont-erase-buffer '(nil erase))
                             (and (not (eq buffer (get-buffer
                                                   shell-command-buffer-name)))
                                  (not (region-active-p)))))
                    ;; If the input is the same buffer as the output,
                    ;; delete everything but the specified region,
                    ;; then replace that region with the output.
                    (progn (setq buffer-read-only nil)
                           (delete-region (max start end) (point-max))
                           (delete-region (point-min) (min start end))
                           (setq exit-status
                                 (call-process-region (point-min) (point-max)
                                                      shell-file-name t
                                                      (if error-file
                                                          (list t error-file)
                                                        t)
                                                      nil shell-command-switch
                                                      command)))
                  ;; Clear the output buffer, then run the command with
                  ;; output there.
                  (let ((directory default-directory))
                    (with-current-buffer buffer
                      (if (not output-buffer)
                          (setq default-directory directory))
                      (shell-command-save-pos-or-erase)))
                  (setq exit-status
                        (call-shell-region start end command nil
                                           (if error-file
                                               (list buffer error-file)
                                             buffer))))
              ;; Report the output.
              (with-current-buffer buffer
                (setq-local revert-buffer-function
                            (lambda (&rest _)
                              (shell-command command)))
                (setq mode-line-process
                      (cond ((null exit-status)
                             " - Error")
                            ((stringp exit-status)
                             (format " - Signal [%s]" exit-status))
                            ((not (equal 0 exit-status))
                             (format " - Exit [%d]" exit-status)))))
              (if (with-current-buffer buffer (> (point-max) (point-min)))
                  ;; There's some output, display it
                  (progn
                    (el-patch-add
                      ;; colorize output
                      (with-current-buffer buffer
                        (ansi-color-apply-on-region (point-min) (point-max))))
                    (display-message-or-buffer buffer)
                    (shell-command-set-point-after-cmd buffer))
                ;; No output; error?
                (let ((output
                       (if (and error-file
                                (< 0 (file-attribute-size
				      (file-attributes error-file))))
                           (format "some error output%s"
                                   (if shell-command-default-error-buffer
                                       (format " to the \"%s\" buffer"
                                               shell-command-default-error-buffer)
                                     ""))
                         "no output")))
                  (cond ((null exit-status)
                         (message "(Shell command failed with error)"))
                        ((equal 0 exit-status)
                         (message "(Shell command succeeded with %s)"
                                  output))
                        ((stringp exit-status)
                         (message "(Shell command killed by signal %s)"
                                  exit-status))
                        (t
                         (message "(Shell command failed with code %d and %s)"
                                  exit-status output))))
                ;; Don't kill: there might be useful info in the undo-log.
                ;; (kill-buffer buffer)
                )))))

      (when (and error-file (file-exists-p error-file))
        (if (< 0 (file-attribute-size (file-attributes error-file)))
	    (with-current-buffer (get-buffer-create error-buffer)
              (goto-char (point-max))
              ;; Insert a separator if there's already text here.
	      (unless (bobp)
	        (insert "\f\n"))
	      ;; Do no formatting while reading error file,
	      ;; because that can run a shell command, and we
	      ;; don't want that to cause an infinite recursion.
	      (format-insert-file error-file nil)
	      (and display-error-buffer
		   (display-buffer (current-buffer)))))
        (delete-file error-file))
      exit-status)))

(when-emacs-version (<= 30 it)
  (el-patch-defun shell-command-on-region (start end command
				      &optional output-buffer replace
				      error-buffer display-error-buffer
				      region-noncontiguous-p)
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer specified
by `shell-command-buffer-name'; prefix arg means replace the region
with it.  Return the exit code of COMMAND.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell command, use \\[universal-coding-system-argument]
before this command.  By default, the input (from the current buffer)
is encoded using coding-system specified by `process-coding-system-alist',
falling back to `default-process-coding-system' if no match for COMMAND
is found in `process-coding-system-alist'.

Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

If the command generates output, the output may be displayed
in the echo area or in a buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.
Otherwise it is displayed in the buffer named by `shell-command-buffer-name'.
The output is available in that buffer in both cases.
Note that if `shell-command-dont-erase-buffer' is non-nil,
the echo area could display more than just the output of the
last command.

If there is output and an error, a message about the error
appears at the end of the output.

Optional fourth arg OUTPUT-BUFFER specifies where to put the
command's output.  If the value is a buffer or buffer name,
erase that buffer and insert the output there; a non-nil value of
`shell-command-dont-erase-buffer' prevents erasing the buffer.
If the value is nil, use the buffer specified by `shell-command-buffer-name'.
Any other non-nil value means to insert the output in the
current buffer after START.

Optional fifth arg REPLACE, if non-nil, means to insert the
output in place of text from START to END, putting point and mark
around it.  If REPLACE is the symbol `no-mark', don't set the mark.

Optional sixth arg ERROR-BUFFER, if non-nil, specifies a buffer
or buffer name to which to direct the command's standard error
output.  If nil, error output is mingled with regular output.
When called interactively, `shell-command-default-error-buffer'
is used for ERROR-BUFFER.

Optional seventh arg DISPLAY-ERROR-BUFFER, if non-nil, means to
display the error buffer if there were any errors.  When called
interactively, this is t.

Non-nil REGION-NONCONTIGUOUS-P means that the region is composed of
noncontiguous pieces.  The most common example of this is a
rectangular region, where the pieces are separated by newline
characters.

If COMMAND names a shell (e.g., via `shell-file-name'), keep in mind
that behavior of various shells when commands are piped to their
standard input is shell- and system-dependent, and thus non-portable.
The differences are especially prominent when the region includes
more than one line, i.e. when piping to a shell commands with embedded
newlines."
  (interactive (let (string)
		 (unless (mark)
		   (user-error "The mark is not set now, so there is no region"))
		 ;; Do this before calling region-beginning
		 ;; and region-end, in case subprocess output
		 ;; relocates them while we are in the minibuffer.
		 (setq string (read-shell-command "Shell command on region: "))
		 ;; call-interactively recognizes region-beginning and
		 ;; region-end specially, leaving them in the history.
		 (list (region-beginning) (region-end)
		       string
		       current-prefix-arg
		       current-prefix-arg
		       shell-command-default-error-buffer
		       t
		       (region-noncontiguous-p))))
  (let ((error-file
	 (if error-buffer
	     (make-temp-file
	      (expand-file-name "scor"
				(or small-temporary-file-directory
				    temporary-file-directory)))
	   nil))
	exit-status)
    ;; Unless a single contiguous chunk is selected, operate on multiple chunks.
    (if region-noncontiguous-p
        (let ((input (concat (funcall region-extract-function (when replace 'delete)) "\n"))
              output)
          (with-temp-buffer
            (insert input)
            (call-process-region (point-min) (point-max)
                                 shell-file-name t t
                                 nil shell-command-switch
                                 command)
            (setq output (split-string (buffer-substring
                                        (point-min)
                                        ;; Trim the trailing newline.
                                        (if (eq (char-before (point-max)) ?\n)
                                            (1- (point-max))
                                          (point-max)))
                                       "\n")))
          (cond
           (replace
            (goto-char start)
            (funcall region-insert-function output))
           (t
            (let ((buffer (get-buffer-create
                           (or output-buffer shell-command-buffer-name))))
              (with-current-buffer buffer
                (erase-buffer)
                (funcall region-insert-function output))
              (display-message-or-buffer buffer)))))
      (if (or replace
              (and output-buffer
                   (not (or (bufferp output-buffer) (stringp output-buffer)))))
          ;; Replace specified region with output from command.
          (let ((swap (and replace (< start end))))
            ;; Don't muck with mark unless REPLACE says we should.
            (goto-char start)
            (when (and replace
                       (not (eq replace 'no-mark)))
              (push-mark (point) 'nomsg))
            (setq exit-status
                  (call-shell-region start end command replace
                                       (if error-file
                                           (list t error-file)
                                         t)))
            ;; It is rude to delete a buffer that the command is not using.
            ;; (let ((shell-buffer (get-buffer shell-command-buffer-name)))
            ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
            ;; 	 (kill-buffer shell-buffer)))
            ;; Don't muck with mark unless REPLACE says we should.
            (when (and replace swap
                       (not (eq replace 'no-mark)))
              (exchange-point-and-mark)))
        ;; No prefix argument: put the output in a temp buffer,
        ;; replacing its entire contents.
        (let ((buffer (get-buffer-create
                       (or output-buffer shell-command-buffer-name))))
          (set-buffer-major-mode buffer) ; Enable globalized modes (bug#38111)
          (unwind-protect
              (if (and (eq buffer (current-buffer))
                       (or (memq shell-command-dont-erase-buffer '(nil erase))
                           (and (not (eq buffer (get-buffer
                                                 shell-command-buffer-name)))
                                (not (region-active-p)))))
                  ;; If the input is the same buffer as the output,
                  ;; delete everything but the specified region,
                  ;; then replace that region with the output.
                  (progn (setq buffer-read-only nil)
                         (delete-region (max start end) (point-max))
                         (delete-region (point-min) (min start end))
                         (setq exit-status
                               (call-process-region (point-min) (point-max)
                                                    shell-file-name t
                                                    (if error-file
                                                        (list t error-file)
                                                      t)
                                                    nil shell-command-switch
                                                    command)))
                ;; Clear the output buffer, then run the command with
                ;; output there.
                (let ((directory default-directory))
                  (with-current-buffer buffer
                    (if (not output-buffer)
                        (setq default-directory directory))
                    (shell-command-save-pos-or-erase)))
                (setq exit-status
                      (call-shell-region start end command nil
                                           (if error-file
                                               (list buffer error-file)
                                             buffer))))
            ;; Report the output.
            (with-current-buffer buffer
              (setq-local revert-buffer-function
                          (lambda (&rest _)
                            (shell-command command)))
              (setq mode-line-process
                    (cond ((null exit-status)
                           " - Error")
                          ((stringp exit-status)
                           (format " - Signal [%s]" exit-status))
                          ((not (equal 0 exit-status))
                           (format " - Exit [%d]" exit-status)))))
            (if (with-current-buffer buffer (> (point-max) (point-min)))
                ;; There's some output, display it
                (progn
                  (el-patch-add
                    ;; colorize output
                    (with-current-buffer buffer
                      (ansi-color-apply-on-region (point-min) (point-max))))
                  (display-message-or-buffer buffer)
                  (shell-command-set-point-after-cmd buffer))
            ;; No output; error?
              (let ((output
                     (if (and error-file
                              (< 0 (file-attribute-size
				    (file-attributes error-file))))
                         (format "some error output%s"
                                 (if shell-command-default-error-buffer
                                     (format " to the \"%s\" buffer"
                                             shell-command-default-error-buffer)
                                   ""))
                       "no output")))
                (cond ((null exit-status)
                       (message "(Shell command failed with error)"))
                      ((equal 0 exit-status)
                       (message "(Shell command succeeded with %s)"
                                output))
                      ((stringp exit-status)
                       (message "(Shell command killed by signal %s)"
                                exit-status))
                      (t
                       (message "(Shell command failed with code %d and %s)"
                                exit-status output))))
              ;; Don't kill: there might be useful info in the undo-log.
              ;; (kill-buffer buffer)
              )))))

    (when (and error-file (file-exists-p error-file))
      (if (< 0 (file-attribute-size (file-attributes error-file)))
	  (with-current-buffer (get-buffer-create error-buffer)
            (goto-char (point-max))
            ;; Insert a separator if there's already text here.
	    (unless (bobp)
	      (insert "\f\n"))
	    ;; Do no formatting while reading error file,
	    ;; because that can run a shell command, and we
	    ;; don't want that to cause an infinite recursion.
	    (format-insert-file error-file nil)
	    (and display-error-buffer
		 (display-buffer (current-buffer)))))
      (delete-file error-file))
    exit-status)))

;;;###autoload
(defun el-patch--load-indent ()
  ;; The ‘indent.el’ in standard Emacs doesn’t have ‘provide’ so it must be ‘load’ed.
  (load "indent"))

;;;###autoload
(add-hook 'el-patch-pre-validate-hook #'el-patch--load-indent)

(when-emacs-version (= 28 it)
  (el-patch-defun indent-region-line-by-line (start end)
    (save-excursion
      (setq end (copy-marker end))
      (goto-char start)
      (let ((pr (el-patch-swap
                  (unless (minibufferp)
                    (make-progress-reporter "Indenting region..." (point) end))
                  nil)))
        (while (< (point) end)
          (or (and (bolp) (eolp))
              (indent-according-to-mode))
          (forward-line 1)
          (and pr (progress-reporter-update pr (point))))
        (and pr (progress-reporter-done pr))
        (move-marker end nil)))))

(when-emacs-version (<= 29 it)
  (el-patch-defun indent-region-line-by-line (start end)
    (save-excursion
      (setq end (copy-marker end))
      (goto-char start)
      (el-patch-splice 2 0
        (let ((pr (unless (minibufferp)
                    (make-progress-reporter "Indenting region..." (point) end))))
          (while (< (point) end)
            (or (and (bolp) (eolp))
                (indent-according-to-mode t))
            (forward-line 1)
            (el-patch-remove
              (and pr (progress-reporter-update pr (point)))))
          (el-patch-remove
            (and pr (progress-reporter-done pr)))
          (move-marker end nil))))))

;;;###autoload
(el-patch-feature hl-line)

(defun hl-line-init ()
  (el-patch-defun hl-line-make-overlay ()
    (let ((ol (make-overlay (point) (point))))
      (overlay-put ol 'priority hl-line-overlay-priority)
      (overlay-put ol 'face hl-line-face)
      (el-patch-add
        (overlay-put ol 'hl-line t))
      ol)))

(eval-after-load "hl-line" '(hl-line-init))

;;;###autoload
(defun hl-line--fix-state-after-clone ()
  "Fixup ‘hl-line-overlay’ in indirect buffer by detaching from the original buffer."
  (when hl-line-mode
    (with-first-matching-overlay
        ov
        (overlay-get ov 'hl-line)
      (overlay-put ov 'is-fixed-after-clone? t)
      ;; Overlays are already copied, need to only propagate them to correct variables
      (setf hl-line-overlay ov))))

;;;###autoload
(add-hook 'clone-indirect-buffer-hook #'hl-line--fix-state-after-clone)

(defun comint-init ()
  (when-emacs-version (<= 29 it)
    (el-patch-defun comint-output-filter (process string)
      (let ((oprocbuf (process-buffer process)))
        ;; First check for killed buffer or no input.
        (when (and string oprocbuf (buffer-name oprocbuf))
          (with-current-buffer oprocbuf
            (el-patch-wrap 1 0
              (with-disabled-undo
	       ;; Run preoutput filters
	       (let ((functions comint-preoutput-filter-functions))
	         (while (and functions string)
	           (if (eq (car functions) t)
		       (let ((functions
                              (default-value 'comint-preoutput-filter-functions)))
		         (while (and functions string)
		           (setq string (funcall (car functions) string))
		           (setq functions (cdr functions))))
	             (setq string (funcall (car functions) string)))
	           (setq functions (cdr functions))))

	       ;; Insert STRING
	       (let ((inhibit-read-only t)
                     ;; The point should float after any insertion we do.
	             (saved-point (copy-marker (point) t)))

	         ;; We temporarily remove any buffer narrowing, in case the
	         ;; process mark is outside of the restriction
	         (save-restriction
	           (widen)

	           (goto-char (process-mark process))
	           (set-marker comint-last-output-start (point))

                   ;; Before we call `comint--mark-as-output' later,
                   ;; redisplay can be called.  We mark the inserted text as
                   ;; output early, to prevent redisplay from fontifying it
                   ;; as input in case of `comint-fontify-input-mode'.
                   (put-text-property 0 (length string) 'field 'output string)

	           ;; insert-before-markers is a bad thing. XXX
	           ;; Luckily we don't have to use it any more, we use
	           ;; window-point-insertion-type instead.
	           (insert string)

	           ;; Advance process-mark
	           (set-marker (process-mark process) (point))

	           (unless comint-inhibit-carriage-motion
	             ;; Interpret any carriage motion characters (newline, backspace)
	             (comint-carriage-motion comint-last-output-start (point)))

	           ;; Run these hooks with point where the user had it.
	           (goto-char saved-point)
	           (run-hook-with-args 'comint-output-filter-functions string)
	           (set-marker saved-point (point))

	           (goto-char (process-mark process)) ; In case a filter moved it.

	           (unless comint-use-prompt-regexp
                     (comint--mark-as-output comint-last-output-start (point)))

	           ;; Highlight the prompt, where we define `prompt' to mean
	           ;; the most recent output that doesn't end with a newline.
	           (let ((prompt-start (save-excursion (forward-line 0) (point)))
		         (inhibit-read-only t))
	             (when comint-prompt-read-only
		       (with-silent-modifications
		         (or (= (point-min) prompt-start)
		             (get-text-property (1- prompt-start) 'read-only)
		             (put-text-property (1- prompt-start)
					        prompt-start 'read-only 'fence))
		         (add-text-properties prompt-start (point)
				              '(read-only t front-sticky (read-only)))))
	             (when comint-last-prompt
		       ;; There might be some keywords here waiting for
		       ;; fontification, so no `with-silent-modifications'.
		       (font-lock--remove-face-from-text-property
		        (car comint-last-prompt)
		        (cdr comint-last-prompt)
		        'font-lock-face
		        'comint-highlight-prompt))
	             (setq comint-last-prompt
		           (cons (copy-marker prompt-start) (point-marker)))
	             (font-lock-append-text-property prompt-start (point)
					             'font-lock-face
					             'comint-highlight-prompt)
	             (add-text-properties prompt-start (point)
	                                  `(rear-nonsticky
	                                    ,comint--prompt-rear-nonsticky)))
	           (goto-char saved-point)))))))))))

(eval-after-load "comint" '(comint-init))

(when-emacs-version (and (= 30 it) (native-comp-available-p))
  (defvar dump--emacs-dir)
  ;; Overwrite the definition with a new one.
  (defun load--fixup-all-elns ()
    "Fix all compilation unit filename.
This to have it working when installed or if Emacs source
directory got moved.  This is set to be a pair in the form of:
\(rel-filename-from-install-bin . rel-filename-from-local-bin)."
    (when (and load--bin-dest-dir load--eln-dest-dir)
      (setq eln-dest-dir
            (concat load--eln-dest-dir "native-lisp/" comp-native-version-dir "/"))
      (maphash (lambda (_ cu)
                 ;; Changed part: condition of ‘when’ completely replaced.
                 (when (and (stringp (native-comp-unit-file cu))
                            (not
                             ;; Keep my locally-produced .eln files under my .emacs.d directory,
                             ;; don’t
                             (string-prefix-p (expand-file-name
                                               (concat dump--emacs-dir "/compiled"))
                                              (expand-file-name
                                               (native-comp-unit-file cu))
                                              (fold-platform-os-type nil t))))
                   (let* ((file (native-comp-unit-file cu))
                          (preloaded (equal (substring (file-name-directory file)
                                                       -10 -1)
                                            "preloaded"))
                          (eln-dest-dir-eff (if preloaded
                                                (expand-file-name "preloaded"
                                                                  eln-dest-dir)
                                              eln-dest-dir)))
                     (native-comp-unit-set-file
                      cu
	              (cons
                       ;; Relative filename from the installed binary.
                       (file-relative-name (expand-file-name
                                            (file-name-nondirectory
                                             file)
                                            eln-dest-dir-eff)
                                           load--bin-dest-dir)
                       ;; Relative filename from the built uninstalled binary.
                       (file-relative-name file invocation-directory))))))
	       comp-loaded-comp-units-h))))

(when-emacs-version (<= 30 it)
  (el-patch-defun shell-command--same-buffer-confirm (action)
    (let ((help-form
           (format
            "There's a command already running in the default buffer,
so we can't start a new one in the same one.

Answering \"yes\" will %s.

Answering \"no\" will exit without doing anything, and won't
start the new command.

Also see the `async-shell-command-buffer' variable."
            (downcase action))))
      (unless (el-patch-let ((query (format "A command is running in the default buffer.  %s? "
                                            action)))
                (el-patch-swap
                  (yes-or-no-p query)
                  (or (eq async-shell-command-buffer 'confirm-new-buffer)
                      (y-or-n-p query))))
        (user-error "Shell command in progress")))))

(defun base-emacs-fixes--pp-fill--save-match-data (f &rest args)
  "Fix ‘pp-fill’ to not corrupt global match data."
  (save-match-data
    (apply f args)))

(advice-add 'pp-fill :around #'base-emacs-fixes--pp-fill--save-match-data)

(when-emacs-version (<= 30 it)
  (el-patch-defun basic-save-buffer (&optional called-interactively)
    "Save the current buffer in its visited file, if it has been modified.

The hooks `write-contents-functions', `local-write-file-hooks'
and `write-file-functions' get a chance to do the job of saving;
if they do not, then the buffer is saved in the visited file in
the usual way.

Before and after saving the buffer, this function runs
`before-save-hook' and `after-save-hook', respectively."
    (interactive '(called-interactively))
    (save-current-buffer
      ;; In an indirect buffer, save its base buffer instead.
      (if (buffer-base-buffer)
	  (set-buffer (buffer-base-buffer)))
      (if (or (buffer-modified-p)
	      ;; Handle the case when no modification has been made but
	      ;; the file disappeared since visited.
	      (and buffer-file-name
		   (not (file-exists-p buffer-file-name))))
	  (let ((recent-save (recent-auto-save-p))
	        setmodes)
	    (or (null buffer-file-name)
                (verify-visited-file-modtime (current-buffer))
	        (not (file-exists-p buffer-file-name))
	        (yes-or-no-p
	         (format
		  "%s has changed since visited or saved.  Save anyway? "
		  (file-name-nondirectory buffer-file-name)))
	        (user-error "Save not confirmed"))
	    (save-restriction
	      (widen)
	      (save-excursion
	        (and (> (point-max) (point-min))
		     (not find-file-literally)
                     (null buffer-read-only)
		     (/= (char-after (1- (point-max))) ?\n)
		     (not (and (eq selective-display t)
			       (= (char-after (1- (point-max))) ?\r)))
		     (or (eq require-final-newline t)
		         (eq require-final-newline 'visit-save)
		         (and require-final-newline
			      (y-or-n-p
			       (format "Buffer %s does not end in newline.  Add one? "
				       (buffer-name)))))
		     (save-excursion
		       (goto-char (point-max))
		       (insert ?\n))))
	      ;; Don't let errors prevent saving the buffer.
	      (with-demoted-errors "Before-save hook error: %S"
	        (run-hooks 'before-save-hook))
              ;; Give `write-contents-functions' a chance to
              ;; short-circuit the whole process.
	      (unless (run-hook-with-args-until-success 'write-contents-functions)
                ;; If buffer has no file name, ask user for one.
                (or buffer-file-name
                    (let ((filename
                           (expand-file-name
                            (read-file-name "File to save in: "
                                            nil (expand-file-name (buffer-name))))))
                      (if (file-exists-p filename)
                          (if (file-directory-p filename)
                              ;; Signal an error if the user specified the name of an
                              ;; existing directory.
                              (error "%s is a directory" filename)
                            (unless (y-or-n-p (format-message
                                               "File `%s' exists; overwrite? "
                                               filename))
                              (error "Canceled"))))
                      (set-visited-file-name filename)))
                ;; Support VC version backups.
	        (vc-before-save)
	        (or (run-hook-with-args-until-success 'local-write-file-hooks)
	            (run-hook-with-args-until-success 'write-file-functions)
	            ;; If a hook returned t, file is already "written".
	            ;; Otherwise, write it the usual way now.
	            (let ((file (buffer-file-name))
                          (dir (file-name-directory
			        (expand-file-name buffer-file-name))))
                      ;; Some systems have directories (like /content on
                      ;; Android) in which files can exist without a
                      ;; corresponding parent directory.
		      (unless (or (file-exists-p file)
                                  (file-exists-p dir))
		        (if (y-or-n-p
		             (format-message
                              "Directory `%s' does not exist; create? " dir))
		            (make-directory dir t)
		          (error "Canceled")))
		      (setq setmodes (basic-save-buffer-1)))))
	      ;; Now we have saved the current buffer.  Let's make sure
	      ;; that buffer-file-coding-system is fixed to what
	      ;; actually used for saving by binding it locally.
              (when buffer-file-name
	        (if save-buffer-coding-system
		    (setq save-buffer-coding-system last-coding-system-used)
	          (setq buffer-file-coding-system last-coding-system-used))
	        (setq buffer-file-number
		      (file-attribute-file-identifier
                       (file-attributes buffer-file-name)))
	        (if setmodes
		    (condition-case ()
		        (progn
		          (unless
			      (with-demoted-errors "Error setting file modes: %S"
			        (set-file-modes buffer-file-name (car setmodes)))
			    (set-file-extended-attributes buffer-file-name
						          (nth 1 setmodes))))
		      (error nil)))
                ;; Support VC `implicit' locking.
	        (vc-after-save))
              ;; If the auto-save file was recent before this command,
	      ;; delete it now.
	      (delete-auto-save-file-if-necessary recent-save))
	    (run-hooks 'after-save-hook)
            ;; Add explicit return value.
            (el-patch-add t))
        (or noninteractive
            (not called-interactively)
            (files--message "(No changes need to be saved)"))
        ;; Add explicit return value.
        (el-patch-add nil)))))

(provide 'base-emacs-fixes)

;; Local Variables:
;; End:

;; base-emacs-fixes.el ends here
