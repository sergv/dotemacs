;; shell-command+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 28 August 2011
;; Keywords:
;; Requirements:
;; Status:

;; This file is an enchance of shell-command-on-region from simple.el

(defun shell-command-on-region+ (start end command
                                       &optional output-buffer replace
                                       error-buffer display-error-buffer)
  "Execute string COMMAND in inferior shell with region as input.
Normally display output (if any) in temp buffer `*Shell Command Output*';
Prefix arg means replace the region with it.  Return the exit code of
COMMAND.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell command, use \\[universal-coding-system-argument]
before this command.  By default, the input (from the current buffer)
is encoded in the same coding system that will be used to save the file,
`buffer-file-coding-system'.  If the output is going to replace the region,
then it is decoded from that same coding system.

The noninteractive arguments are START, END, COMMAND,
OUTPUT-BUFFER, REPLACE, ERROR-BUFFER, and DISPLAY-ERROR-BUFFER.
Noninteractive callers can specify coding systems by binding
`coding-system-for-read' and `coding-system-for-write'.

If the command generates output, the output may be displayed
in the echo area or in a buffer.
If the output is short enough to display in the echo area
\(determined by the variable `max-mini-window-height' if
`resize-mini-windows' is non-nil), it is shown there.  Otherwise
it is displayed in the buffer `*Shell Command Output*'.  The output
is available in that buffer in both cases.

If there is output and an error, a message about the error
appears at the end of the output.

If there is no output, or if output is inserted in the current buffer,
then `*Shell Command Output*' is deleted.

If the optional fourth argument OUTPUT-BUFFER is non-nil,
that says to put the output in some other buffer.
If OUTPUT-BUFFER is a buffer or buffer name, put the output there.
If OUTPUT-BUFFER is not a buffer and not nil,
insert output in the current buffer.
In either case, the output is inserted after point (leaving mark after it).

If REPLACE, the optional fifth argument, is non-nil, that means insert
the output in place of text from START to END, putting point and mark
around it.

If optional sixth argument ERROR-BUFFER is non-nil, it is a buffer
or buffer name to which to direct the command's standard error output.
If it is nil, error output is mingled with regular output.
If DISPLAY-ERROR-BUFFER is non-nil, display the error buffer if there
were any errors.  (This is always t, interactively.)
In an interactive call, the variable `shell-command-default-error-buffer'
specifies the value of ERROR-BUFFER."
  (interactive (let (string)
		 (unless (mark)
                   (error "The mark is not set now, so there is no region"))
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
                       t)))
  (let ((error-file
         (if error-buffer
           (make-temp-file
            (expand-file-name "scor"
                              (or small-temporary-file-directory
                                  temporary-file-directory)))
           nil))
	exit-status)
    (if (or replace
	    (and output-buffer
                 (not (or (bufferp output-buffer) (stringp output-buffer)))))
      ;; Replace specified region with output from command.
      (let ((swap (and replace (< start end))))
        ;; Don't muck with mark unless REPLACE says we should.
        (goto-char start)
        (and replace (push-mark (point) 'nomsg))
        (setq exit-status
              (call-process-region start end shell-file-name t
                                   (if error-file
                                     (list t error-file)
                                     t)
                                   nil shell-command-switch command))
        ;; It is rude to delete a buffer which the command is not using.
        ;; (let ((shell-buffer (get-buffer "*Shell Command Output*")))
        ;;   (and shell-buffer (not (eq shell-buffer (current-buffer)))
        ;; 	 (kill-buffer shell-buffer)))
        ;; Don't muck with mark unless REPLACE says we should.
        (and replace swap (exchange-point-and-mark)))
      ;; No prefix argument: put the output in a temp buffer,
      ;; replacing its entire contents.
      (let ((buffer (get-buffer-create
                     (or output-buffer "*Shell Command Output*"))))
        (unwind-protect
            (if (eq buffer (current-buffer))
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
                  (setq buffer-read-only nil)
                  (if (not output-buffer)
                    (setq default-directory directory))
                  (erase-buffer)
                  (text-mode)))
              (setq exit-status
                    (call-process-region start end shell-file-name nil
                                         (if error-file
                                           (list buffer error-file)
                                           buffer)
                                         nil shell-command-switch command)))

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
              ;; colorize output
              (require 'ansi-color)
              (with-current-buffer buffer
                (ansi-color-apply-on-region (point-min) (point-max)))
              (display-message-or-buffer buffer))
            ;; No output; error?
            (let ((output
                   (if (and error-file
                            (< 0 (nth 7 (file-attributes error-file))))
                     "some error output"
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
            ))))

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
    exit-status))


;; Local Variables:
;; End:

;; shell-command+.el ends here
