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
  (require 'el-patch))

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

(when-emacs-version (or (= it 25) (= it 26))
  (el-patch-defun shell-command-on-region (start end command
                                                 &optional output-buffer replace
                                                 error-buffer display-error-buffer
                                                 region-noncontiguous-p)
    "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.  Return the exit code of
COMMAND.

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
Otherwise it is displayed in the buffer `*Shell Command Output*'.
The output is available in that buffer in both cases.

If there is output and an error, a message about the error
appears at the end of the output.

Optional fourth arg OUTPUT-BUFFER specifies where to put the
command's output.  If the value is a buffer or buffer name,
erase that buffer and insert the output there; a non-nil value of
`shell-command-dont-erase-buffer' prevent to erase the buffer.
If the value is nil, use the buffer `*Shell Command Output*'.
Any other non-nil value means to insert the output in the
current buffer after START.

Optional fifth arg REPLACE, if non-nil, means to insert the
output in place of text from START to END, putting point and mark
around it.

Optional sixth arg ERROR-BUFFER, if non-nil, specifies a buffer
or buffer name to which to direct the command's standard error
output.  If nil, error output is mingled with regular output.
When called interactively, `shell-command-default-error-buffer'
is used for ERROR-BUFFER.

Optional seventh arg DISPLAY-ERROR-BUFFER, if non-nil, means to
display the error buffer if there were any errors.  When called
interactively, this is t."
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
          (let ((input (concat (funcall region-extract-function 'delete) "\n"))
                output)
            (with-temp-buffer
              (insert input)
              (call-process-region (point-min) (point-max)
                                   shell-file-name t t
                                   nil shell-command-switch
                                   command)
              (setq output (split-string (buffer-string) "\n")))
            (goto-char start)
            (funcall region-insert-function output))
        (if (or replace
                (and output-buffer
                     (not (or (bufferp output-buffer) (stringp output-buffer)))))
            ;; Replace specified region with output from command.
            (let ((swap (and replace (< start end))))
              ;; Don't muck with mark unless REPLACE says we should.
              (goto-char start)
              (and replace (push-mark (point) 'nomsg))
              (setq exit-status
                    (call-shell-region start end command replace
                                       (if error-file
                                           (list t error-file)
                                         t)))
              ;; It is rude to delete a buffer which the command is not using.
              ;; (let ((shell-buffer (get-buffer "*Shell Command Output*")))
              ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
              ;;          (kill-buffer shell-buffer)))
              ;; Don't muck with mark unless REPLACE says we should.
              (and replace swap (exchange-point-and-mark)))
          ;; No prefix argument: put the output in a temp buffer,
          ;; replacing its entire contents.
          (let ((buffer (get-buffer-create
                         (or output-buffer "*Shell Command Output*"))))
            (unwind-protect
                (if (and (eq buffer (current-buffer))
                         (or (not shell-command-dont-erase-buffer)
                             (and (not (eq buffer (get-buffer "*Shell Command Output*")))
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
                      (shell-command--save-pos-or-erase)))
                  (setq exit-status
                        (call-shell-region start end command nil
                                           (if error-file
                                               (list buffer error-file)
                                             buffer))))
              ;; Report the output.
              (with-current-buffer buffer
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
                    (shell-command--set-point-after-cmd buffer))
                ;; No output; error?
                (let ((output
                       (if (and error-file
                                (< 0 (nth 7 (file-attributes error-file))))
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
        (if (< 0 (nth 7 (file-attributes error-file)))
            (with-current-buffer (get-buffer-create error-buffer)
              (let ((pos-from-end (- (point-max) (point))))
                (or (bobp)
                    (insert "\f\n"))
                ;; Do no formatting while reading error file,
                ;; because that can run a shell command, and we
                ;; don't want that to cause an infinite recursion.
                (format-insert-file error-file nil)
                ;; Put point after the inserted errors.
                (goto-char (- (point-max) pos-from-end)))
              (and display-error-buffer
                   (display-buffer (current-buffer)))))
        (delete-file error-file))
      exit-status)))

(when-emacs-version (= it 27)
  (el-patch-defun shell-command-on-region (start end command
                                                 &optional output-buffer replace
                                                 error-buffer display-error-buffer
                                                 region-noncontiguous-p)
    "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.  Return the exit code of
COMMAND.

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
Otherwise it is displayed in the buffer `*Shell Command Output*'.
The output is available in that buffer in both cases.

If there is output and an error, a message about the error
appears at the end of the output.

Optional fourth arg OUTPUT-BUFFER specifies where to put the
command's output.  If the value is a buffer or buffer name,
erase that buffer and insert the output there; a non-nil value of
`shell-command-dont-erase-buffer' prevent to erase the buffer.
If the value is nil, use the buffer `*Shell Command Output*'.
Any other non-nil value means to insert the output in the
current buffer after START.

Optional fifth arg REPLACE, if non-nil, means to insert the
output in place of text from START to END, putting point and mark
around it.

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
          (let ((input (concat (funcall region-extract-function 'delete) "\n"))
                output)
            (with-temp-buffer
              (insert input)
              (call-process-region (point-min) (point-max)
                                   shell-file-name t t
                                   nil shell-command-switch
                                   command)
              (setq output (split-string (buffer-string) "\n")))
            (goto-char start)
            (funcall region-insert-function output))
        (if (or replace
                (and output-buffer
                     (not (or (bufferp output-buffer) (stringp output-buffer)))))
            ;; Replace specified region with output from command.
            (let ((swap (and replace (< start end))))
              ;; Don't muck with mark unless REPLACE says we should.
              (goto-char start)
              (and replace (push-mark (point) 'nomsg))
              (setq exit-status
                    (call-shell-region start end command replace
                                       (if error-file
                                           (list t error-file)
                                         t)))
              ;; It is rude to delete a buffer that the command is not using.
              ;; (let ((shell-buffer (get-buffer "*Shell Command Output*")))
              ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
              ;;          (kill-buffer shell-buffer)))
              ;; Don't muck with mark unless REPLACE says we should.
              (and replace swap (exchange-point-and-mark)))
          ;; No prefix argument: put the output in a temp buffer,
          ;; replacing its entire contents.
          (let ((buffer (get-buffer-create
                         (or output-buffer "*Shell Command Output*"))))
            (set-buffer-major-mode buffer) ; Enable globalized modes (bug#38111)
            (unwind-protect
                (if (and (eq buffer (current-buffer))
                         (or (memq shell-command-dont-erase-buffer '(nil erase))
                             (and (not (eq buffer (get-buffer "*Shell Command Output*")))
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

(when-emacs-version (<= 28 it)
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

;;;###autoload
(defun el-patch--load-indent ()
  ;; The ‘indent.el’ in standard Emacs doesn’t have ‘provide’ so it must be ‘load’ed.
  (load "indent"))

;;;###autoload
(add-hook 'el-patch-pre-validate-hook #'el-patch--load-indent)

(when-emacs-version (<= 28 it)
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

(provide 'base-emacs-fixes)

;; Local Variables:
;; End:

;; base-emacs-fixes.el ends here
