;; comint-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 January 2012
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl)
  (require 'el-patch)
  (require 'set-up-platform)
  (require 'macro-util))

(provide 'comint-setup)
(require 'el-patch)

;;;###autoload
(setf comint-input-ignoredups t
      comint-input-ring-size 2048)
;;;###autoload
(setq-default comint-input-ignoredups t)

;;;###autoload
(eval-after-load "shell" '(require 'comint-setup))
;;;###autoload
(eval-after-load "comint" '(require 'comint-setup))

(el-patch-feature shell)

(defun shell-init ()
  ;; this calls `comint-write-input-ring' from the repl buffer so that it
  ;; will see correct value of `comint-input-ring' which is permamently
  ;; local everywhere
  (el-patch-defun shell-write-history-on-exit (process event)
    "Called when the shell process is stopped.

Writes the input history to a history file
`comint-input-ring-file-name' using `comint-write-input-ring'
and inserts a short message in the shell buffer.

This function is a sentinel watching the shell interpreter process.
Sentinels will always get the two parameters PROCESS and EVENT."
    (el-patch-remove (comint-write-input-ring))
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (el-patch-add (comint-write-input-ring))
          (insert (format "\nProcess %s %s\n" process event)))))))

(eval-after-load "shell" '(shell-init))

(el-patch-feature comint)

(defun comint-init ()
  (when-emacs-version (and (<= 25 it)
                           (<= it 27))
    ;; NOTE: this should be updated regularly rework agressive bolding
    ;; of comint input so that it doesn't replace original face of
    ;; input, but makes it bolded instead
    (el-patch-defun comint-send-input (&optional no-newline artificial)
      "Send input to process.
After the process output mark, sends all text from the process mark to
point as input to the process.  Before the process output mark, calls
value of variable `comint-get-old-input' to retrieve old input, copies
it to the process mark, and sends it.

This command also sends and inserts a final newline, unless
NO-NEWLINE is non-nil.

Any history reference may be expanded depending on the value of the variable
`comint-input-autoexpand'.  The list of function names contained in the value
of `comint-input-filter-functions' is called on the input before sending it.
The input is entered into the input history ring, if the value of variable
`comint-input-filter' returns non-nil when called on the input.

If variable `comint-eol-on-send' is non-nil, then point is moved to the
end of line before sending the input.

After the input has been sent, if `comint-process-echoes' is non-nil,
then `comint-send-input' waits to see if the process outputs a string
matching the input, and if so, deletes that part of the output.
If ARTIFICIAL is non-nil, it inhibits such deletion.
Callers sending input not from the user should use ARTIFICIAL = t.

The values of `comint-get-old-input', `comint-input-filter-functions', and
`comint-input-filter' are chosen according to the command interpreter running
in the buffer.  E.g.,

If the interpreter is the csh,
   `comint-get-old-input' is the default:
       If `comint-use-prompt-regexp' is nil, then
       either return the current input field, if point is on an input
       field, or the current line, if point is on an output field.
       If `comint-use-prompt-regexp' is non-nil, then
       return the current line with any initial string matching the
       regexp `comint-prompt-regexp' removed.
   `comint-input-filter-functions' monitors input for \"cd\", \"pushd\", and
       \"popd\" commands.  When it sees one, it cd's the buffer.
   `comint-input-filter' is the default: returns t if the input isn't all white
       space.

If the Comint is Lucid Common Lisp,
   `comint-get-old-input' snarfs the sexp ending at point.
   `comint-input-filter-functions' does nothing.
   `comint-input-filter' returns nil if the input matches input-filter-regexp,
       which matches (1) all whitespace (2) :a, :c, etc.

Similarly for Soar, Scheme, etc."
      (interactive)
      ;; If we're currently completing, stop.  We're definitely done
      ;; completing, and by sending the input, we might cause side effects
      ;; that will confuse the code running in the completion
      ;; post-command-hook.
      (when completion-in-region-mode
        (completion-in-region-mode -1))
      ;; Note that the input string does not include its terminal newline.
      (let ((proc (get-buffer-process (current-buffer))))
        (if (not proc) (user-error "Current buffer has no process")
          (widen)
          (let* ((pmark (process-mark proc))
                 (intxt (if (>= (point) (marker-position pmark))
                            (progn (if comint-eol-on-send
                                       (if comint-use-prompt-regexp
                                           (end-of-line)
                                         (goto-char (field-end))))
                                   (buffer-substring pmark (point)))
                          (let ((copy (funcall comint-get-old-input)))
                            (goto-char pmark)
                            (insert copy)
                            copy)))
                 (input (if (not (eq comint-input-autoexpand 'input))
                            ;; Just whatever's already there.
                            intxt
                          ;; Expand and leave it visible in buffer.
                          (comint-replace-by-expanded-history t pmark)
                          (buffer-substring pmark (point))))
                 (history (if (not (eq comint-input-autoexpand 'history))
                              input
                            ;; This is messy 'cos ultimately the original
                            ;; functions used do insertion, rather than return
                            ;; strings.  We have to expand, then insert back.
                            (comint-replace-by-expanded-history t pmark)
                            (let ((copy (buffer-substring pmark (point)))
                                  (start (point)))
                              (insert input)
                              (delete-region pmark start)
                              copy))))

            (unless no-newline
              (insert ?\n))

            (comint-add-to-input-history history)

            (run-hook-with-args 'comint-input-filter-functions
                                (if no-newline input
                                  (concat input "\n")))

            (let ((beg (marker-position pmark))
                  (end (if no-newline (point) (1- (point)))))
              (with-silent-modifications
                (when (> end beg)
                  (add-text-properties beg end
                                       (el-patch-swap
                                         '(front-sticky t font-lock-face comint-highlight-input)
                                         '(front-sticky t)))
                  (unless comint-use-prompt-regexp
                    ;; Give old user input a field property of `input', to
                    ;; distinguish it from both process output and unsent
                    ;; input.  The terminating newline is put into a special
                    ;; `boundary' field to make cursor movement between input
                    ;; and output fields smoother.
                    (add-text-properties
                     beg end
                     '(mouse-face highlight
                                  help-echo "mouse-2: insert after prompt as new input"))))
                (unless (or no-newline comint-use-prompt-regexp)
                  ;; Cover the terminating newline
                  (add-text-properties end (1+ end)
                                       '(rear-nonsticky t
                                                        field boundary
                                                        inhibit-line-move-field-capture t)))))

            (comint-snapshot-last-prompt)

            (setq comint-save-input-ring-index comint-input-ring-index)
            (setq comint-input-ring-index nil)
            ;; Update the markers before we send the input
            ;; in case we get output amidst sending the input.
            (set-marker comint-last-input-start pmark)
            (set-marker comint-last-input-end (point))
            (set-marker (process-mark proc) (point))
            ;; clear the "accumulation" marker
            (set-marker comint-accum-marker nil)
            (let ((comint-input-sender-no-newline no-newline))
              (funcall comint-input-sender proc input))

            ;; Optionally delete echoed input (after checking it).
            (when (and comint-process-echoes (not artificial))
              (let ((echo-len (- comint-last-input-end
                                 comint-last-input-start)))
                ;; Wait for all input to be echoed:
                (while (and (> (+ comint-last-input-end echo-len)
                               (point-max))
                            (accept-process-output proc)
                            (zerop
                             (compare-buffer-substrings
                              nil comint-last-input-start
                              (- (point-max) echo-len)
                              ;; Above difference is equivalent to
                              ;; (+ comint-last-input-start
                              ;;    (- (point-max) comint-last-input-end))
                              nil comint-last-input-end (point-max)))))
                (if (and
                     (<= (+ comint-last-input-end echo-len)
                         (point-max))
                     (zerop
                      (compare-buffer-substrings
                       nil comint-last-input-start comint-last-input-end
                       nil comint-last-input-end
                       (+ comint-last-input-end echo-len))))
                    ;; Certain parts of the text to be deleted may have
                    ;; been mistaken for prompts.  We have to prevent
                    ;; problems when `comint-prompt-read-only' is non-nil.
                    (let ((inhibit-read-only t))
                      (delete-region comint-last-input-end
                                     (+ comint-last-input-end echo-len))
                      (when comint-prompt-read-only
                        (save-excursion
                          (goto-char comint-last-input-end)
                          (comint-update-fence)))))))

            ;; This used to call comint-output-filter-functions,
            ;; but that scrolled the buffer in undesirable ways.
            (run-hook-with-args 'comint-output-filter-functions ""))))))

  (when-emacs-version (<= 28 it)
    ;; NOTE: this should be updated regularly
    ;; rework agressive bolding of comint input so that it doesn't replace
    ;; original face of input, but makes it bolded instead
    (el-patch-defun comint-send-input (&optional no-newline artificial)
      "Send input to process.
After the process output mark, sends all text from the process mark to
point as input to the process.  Before the process output mark, calls
value of variable `comint-get-old-input' to retrieve old input, copies
it to the process mark, and sends it.

This command also sends and inserts a final newline, unless
NO-NEWLINE is non-nil.

Any history reference may be expanded depending on the value of the variable
`comint-input-autoexpand'.  The list of function names contained in the value
of `comint-input-filter-functions' is called on the input before sending it.
The input is entered into the input history ring, if the value of variable
`comint-input-filter' returns non-nil when called on the input.

If variable `comint-eol-on-send' is non-nil, then point is moved to the
end of line before sending the input.

After the input has been sent, if `comint-process-echoes' is non-nil,
then `comint-send-input' waits to see if the process outputs a string
matching the input, and if so, deletes that part of the output.
If ARTIFICIAL is non-nil, it inhibits such deletion.
Callers sending input not from the user should use ARTIFICIAL = t.

The values of `comint-get-old-input', `comint-input-filter-functions', and
`comint-input-filter' are chosen according to the command interpreter running
in the buffer.  E.g.,

If the interpreter is the csh,
    `comint-get-old-input' is the default:
	If `comint-use-prompt-regexp' is nil, then
	either return the current input field, if point is on an input
	field, or the current line, if point is on an output field.
	If `comint-use-prompt-regexp' is non-nil, then
	return the current line with any initial string matching the
	regexp `comint-prompt-regexp' removed.
    `comint-input-filter-functions' monitors input for \"cd\", \"pushd\", and
	\"popd\" commands.  When it sees one, it cd's the buffer.
    `comint-input-filter' is the default: returns t if the input isn't all white
	space.

If the Comint is Lucid Common Lisp,
    `comint-get-old-input' snarfs the sexp ending at point.
    `comint-input-filter-functions' does nothing.
    `comint-input-filter' returns nil if the input matches input-filter-regexp,
	which matches (1) all whitespace (2) :a, :c, etc.

Similarly for Soar, Scheme, etc."
      (interactive)
      ;; If we're currently completing, stop.  We're definitely done
      ;; completing, and by sending the input, we might cause side effects
      ;; that will confuse the code running in the completion
      ;; post-command-hook.
      (when completion-in-region-mode
        (completion-in-region-mode -1))
      ;; Note that the input string does not include its terminal newline.
      (let ((proc (get-buffer-process (current-buffer))))
        (if (not proc) (user-error "Current buffer has no process")
          (widen)
          (let* ((pmark (process-mark proc))
                 (intxt (if (>= (point) (marker-position pmark))
                            (progn (if comint-eol-on-send
                                       (if comint-use-prompt-regexp
                                           (end-of-line)
                                         (goto-char (field-end))))
                                   (buffer-substring pmark (point)))
                          (let ((copy (funcall comint-get-old-input)))
                            (goto-char pmark)
                            (insert copy)
                            copy)))
                 (input (if (not (eq comint-input-autoexpand 'input))
                            ;; Just whatever's already there.
                            intxt
                          ;; Expand and leave it visible in buffer.
                          (comint-replace-by-expanded-history t pmark)
                          (buffer-substring pmark (point))))
                 (history (if (not (eq comint-input-autoexpand 'history))
                              input
                            ;; This is messy 'cos ultimately the original
                            ;; functions used do insertion, rather than return
                            ;; strings.  We have to expand, then insert back.
                            (comint-replace-by-expanded-history t pmark)
                            (let ((copy (buffer-substring pmark (point)))
                                  (start (point)))
                              (insert input)
                              (delete-region pmark start)
                              copy))))

            (unless no-newline
              (insert ?\n))

            (comint-add-to-input-history history)

            (run-hook-with-args 'comint-input-filter-functions
                                (if no-newline input
                                  (concat input "\n")))

            (let ((beg (marker-position pmark))
                  (end (if no-newline (point) (1- (point)))))
              (with-silent-modifications
                (when (> end beg)
                  (when comint-highlight-input
                    (add-text-properties beg end
                                         (el-patch-swap
                                           '( font-lock-face comint-highlight-input
                                              front-sticky t )
                                           '(front-sticky t))))
                  (unless comint-use-prompt-regexp
                    ;; Give old user input a field property of `input', to
                    ;; distinguish it from both process output and unsent
                    ;; input.  The terminating newline is put into a special
                    ;; `boundary' field to make cursor movement between input
                    ;; and output fields smoother.
                    (add-text-properties
                     beg end
                     '(mouse-face highlight
                                  help-echo "mouse-2: insert after prompt as new input"))))
                (unless (or no-newline comint-use-prompt-regexp)
                  ;; Cover the terminating newline
                  (add-text-properties end (1+ end)
                                       `(rear-nonsticky
                                         ,comint--prompt-rear-nonsticky
                                         field boundary
                                         inhibit-line-move-field-capture t)))))

            (comint-snapshot-last-prompt)

            (setq comint-save-input-ring-index comint-input-ring-index)
            (setq comint-input-ring-index nil)
            ;; Update the markers before we send the input
            ;; in case we get output amidst sending the input.
            (set-marker comint-last-input-start pmark)
            (set-marker comint-last-input-end (point))
            (set-marker (process-mark proc) (point))
            ;; clear the "accumulation" marker
            (set-marker comint-accum-marker nil)
            (let ((comint-input-sender-no-newline no-newline))
              (funcall comint-input-sender proc input))

            ;; Optionally delete echoed input (after checking it).
            (when (and comint-process-echoes (not artificial))
              (let ((echo-len (- comint-last-input-end
                                 comint-last-input-start)))
                ;; Wait for all input to be echoed:
                (while (and (> (+ comint-last-input-end echo-len)
                               (point-max))
                            (accept-process-output proc)
                            (zerop
                             (compare-buffer-substrings
                              nil comint-last-input-start
                              (- (point-max) echo-len)
                              ;; Above difference is equivalent to
                              ;; (+ comint-last-input-start
                              ;;    (- (point-max) comint-last-input-end))
                              nil comint-last-input-end (point-max)))))
                (if (and
                     (<= (+ comint-last-input-end echo-len)
                         (point-max))
                     (zerop
                      (compare-buffer-substrings
                       nil comint-last-input-start comint-last-input-end
                       nil comint-last-input-end
                       (+ comint-last-input-end echo-len))))
                    ;; Certain parts of the text to be deleted may have
                    ;; been mistaken for prompts.  We have to prevent
                    ;; problems when `comint-prompt-read-only' is non-nil.
                    (let ((inhibit-read-only t))
                      (delete-region comint-last-input-end
                                     (+ comint-last-input-end echo-len))
                      (when comint-prompt-read-only
                        (save-excursion
                          (goto-char comint-last-input-end)
                          (comint-update-fence)))))))

            ;; This used to call comint-output-filter-functions,
            ;; but that scrolled the buffer in undesirable ways.
            (set-marker comint-last-output-start pmark)
            (run-hook-with-args 'comint-output-filter-functions "")))))))

(eval-after-load "comint" '(comint-init))

(defun comint-next-input-or-fwd-paragraph (count)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-next-input count)
    (vim:motion-fwd-paragraph :count count)))

(defun comint-previous-input-or-bwd-paragraph (count)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-previous-input count)
    (vim:motion-bwd-paragraph :count count)))

;;;###autoload
(defun comint-setup ()
  (def-keys-for-map comint-mode-map
    ("<up>"     comint-previous-input-or-bwd-paragraph)
    ("<down>"   comint-next-input-or-fwd-paragraph)
    ("C-<up>"   comint-previous-prompt)
    ("C-<down>" comint-next-prompt)
    ("S-<up>"   comint-previous-prompt)
    ("S-<down>" comint-next-prompt)))

;;;###autoload
(add-hook 'comint-mode-hook #'comint-setup)

;;;###autoload
(defun comint-clear-prompt () ;; shell-clear-prompt
  "Clear shell prompt from input."
  (interactive)
  (comint-bol)
  (when (not (equal (point)
                    (line-end-position)))
    (delete-region (point) (line-end-position))))

;;;###autoload
(defun comint-clear-buffer-above-prompt () ;; shell-clear-buffer
  "Clear everything between start of buffer and line above current one
inclusively."
  (interactive)
  (save-excursion
    (with-inhibited-read-only
     (forward-line -1)
     (delete-region (point-min) (line-end-position))
     (delete-char 1))))

(provide 'comint-setup)

;; Local Variables:
;; End:

;; comint-setup.el ends here
