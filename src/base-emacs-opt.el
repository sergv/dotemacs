;; base-emacs-opt.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 11 December 2021
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'el-patch)
  (require 'macro-util))

(require 'base-emacs-fixes)

(defvar vim--current-universal-argument-provided?)
(defvar vim--universal-argument-provided?)
(defvar thing-at-point-beginning-of-url-regexp)

(when-emacs-version (= 28 it)
  (el-patch-defun kill-new (string &optional replace)
    "Make STRING the latest kill in the kill ring.
Set `kill-ring-yank-pointer' to point to it.
If `interprogram-cut-function' is non-nil, apply it to STRING.
Optional second argument REPLACE non-nil means that STRING will replace
the front of the kill ring, rather than being added to the list.

When `save-interprogram-paste-before-kill' and `interprogram-paste-function'
are non-nil, save the interprogram paste string(s) into `kill-ring' before
STRING.

When the yank handler has a non-nil PARAM element, the original STRING
argument is not used by `insert-for-yank'.  However, since Lisp code
may access and use elements from the kill ring directly, the STRING
argument should still be a \"useful\" string for such uses."
    ;; Allow the user to transform or ignore the string.
    (when (or (not kill-transform-function)
              (setq string (funcall kill-transform-function string)))
      ;; This yank-menu just duplicates kill-ring and grows uncontrollably in size increasing
      ;; Emacs’s heap size.
      (el-patch-remove
        (unless (and kill-do-not-save-duplicates
	             ;; Due to text properties such as 'yank-handler that
	             ;; can alter the contents to yank, comparison using
	             ;; `equal' is unsafe.
	             (equal-including-properties string (car kill-ring)))
          (if (fboundp 'menu-bar-update-yank-menu)
	      (menu-bar-update-yank-menu string (and replace (car kill-ring))))))
      (when save-interprogram-paste-before-kill
        (let ((interprogram-paste (and interprogram-paste-function
                                       (funcall interprogram-paste-function))))
          (when interprogram-paste
            (setq interprogram-paste
                  (if (listp interprogram-paste)
                      ;; Use `reverse' to avoid modifying external data.
                      (reverse interprogram-paste)
		    (list interprogram-paste)))
            (when (or (not (numberp save-interprogram-paste-before-kill))
                      (< (seq-reduce #'+ (mapcar #'length interprogram-paste) 0)
                         save-interprogram-paste-before-kill))
              (dolist (s interprogram-paste)
	        (unless (and kill-do-not-save-duplicates
                             (equal-including-properties s (car kill-ring)))
	          (push s kill-ring)))))))
      (unless (and kill-do-not-save-duplicates
	           (equal-including-properties string (car kill-ring)))
        (if (and replace kill-ring)
	    (setcar kill-ring string)
          (let ((history-delete-duplicates nil))
            (add-to-history 'kill-ring string kill-ring-max t))))
      (setq kill-ring-yank-pointer kill-ring)
      (if interprogram-cut-function
          (funcall interprogram-cut-function string)))))

(when-emacs-version (<= 29 it)
  (el-patch-defun kill-new (string &optional replace)
    "Make STRING the latest kill in the kill ring.
Set `kill-ring-yank-pointer' to point to it.
If `interprogram-cut-function' is non-nil, apply it to STRING.
Optional second argument REPLACE non-nil means that STRING will replace
the front of the kill ring, rather than being added to the list.

When `save-interprogram-paste-before-kill' and `interprogram-paste-function'
are non-nil, save the interprogram paste string(s) into `kill-ring' before
STRING.

When the yank handler has a non-nil PARAM element, the original STRING
argument is not used by `insert-for-yank'.  However, since Lisp code
may access and use elements from the kill ring directly, the STRING
argument should still be a \"useful\" string for such uses."
    ;; Allow the user to transform or ignore the string.
    (when (or (not kill-transform-function)
              (setq string (funcall kill-transform-function string)))
      ;; This yank-menu just duplicates kill-ring and grows uncontrollably in size increasing
      ;; Emacs’s heap size.
      (el-patch-remove
        (unless (and kill-do-not-save-duplicates
	             ;; Due to text properties such as 'yank-handler that
	             ;; can alter the contents to yank, comparison using
	             ;; `equal' is unsafe.
	             (equal-including-properties string (car kill-ring)))
          (if (fboundp 'menu-bar-update-yank-menu)
	      (menu-bar-update-yank-menu string (and replace (car kill-ring))))))
      (when save-interprogram-paste-before-kill
        (let ((interprogram-paste
               (and interprogram-paste-function
                    ;; On X, the selection owner might be slow, so the user might
                    ;; interrupt this. If they interrupt it, we want to continue
                    ;; so we become selection owner, so this doesn't stay slow.
                    (if (eq (window-system) 'x)
                        (ignore-error quit (funcall interprogram-paste-function))
                      (funcall interprogram-paste-function)))))
          (when interprogram-paste
            (setq interprogram-paste
                  (if (listp interprogram-paste)
                      ;; Use `reverse' to avoid modifying external data.
                      (reverse interprogram-paste)
		    (list interprogram-paste)))
            (when (or (not (numberp save-interprogram-paste-before-kill))
                      (< (seq-reduce #'+ (mapcar #'length interprogram-paste) 0)
                         save-interprogram-paste-before-kill))
              (dolist (s interprogram-paste)
	        (unless (and kill-do-not-save-duplicates
                             (equal-including-properties s (car kill-ring)))
	          (push s kill-ring)))))))
      (unless (and kill-do-not-save-duplicates
	           (equal-including-properties string (car kill-ring)))
        (if (and replace kill-ring)
	    (setcar kill-ring string)
          (let ((history-delete-duplicates nil))
            (add-to-history 'kill-ring string kill-ring-max t))))
      (setq kill-ring-yank-pointer kill-ring)
      (if interprogram-cut-function
          (funcall interprogram-cut-function string)))))

(when-emacs-version (= it 27)
  (el-patch-defun command-execute (cmd &optional record-flag keys special)
    ;; BEWARE: Called directly from the C code.
    "Execute CMD as an editor command.
CMD must be a symbol that satisfies the `commandp' predicate.
Optional second arg RECORD-FLAG non-nil
means unconditionally put this command in the variable `command-history'.
Otherwise, that is done only if an arg is read using the minibuffer.
The argument KEYS specifies the value to use instead of (this-command-keys)
when reading the arguments; if it is nil, (this-command-keys) is used.
The argument SPECIAL, if non-nil, means that this command is executing
a special event, so ignore the prefix argument and don't clear it."
    (setq debug-on-next-call nil)
    (let ((prefixarg (unless special
                       ;; FIXME: This should probably be done around
                       ;; pre-command-hook rather than here!
                       (prog1 prefix-arg
                         (setq current-prefix-arg prefix-arg)
                         (setq prefix-arg nil)
                         (el-patch-add
                           (setq vim--current-universal-argument-provided? vim--universal-argument-provided?)
                           (setq vim--universal-argument-provided? nil))
                         (when current-prefix-arg
                           (prefix-command-update))))))
      (if (and (symbolp cmd)
               (get cmd 'disabled)
               disabled-command-function)
          ;; FIXME: Weird calling convention!
          (run-hooks 'disabled-command-function)
        (let ((final cmd))
          (while
              (progn
                (setq final (indirect-function final))
                (if (autoloadp final)
                    (setq final (autoload-do-load final cmd)))))
          (cond
            ((arrayp final)
             ;; If requested, place the macro in the command history.  For
             ;; other sorts of commands, call-interactively takes care of this.
             (when record-flag
               (add-to-history
                'command-history `(execute-kbd-macro ,final ,prefixarg) nil t))
             (execute-kbd-macro final prefixarg))
            (t
             ;; Pass `cmd' rather than `final', for the backtrace's sake.
             (prog1 (call-interactively cmd record-flag keys)
               (when (and (symbolp cmd)
                          (get cmd 'byte-obsolete-info)
                          (not (get cmd 'command-execute-obsolete-warned)))
                 (put cmd 'command-execute-obsolete-warned t)
                 (message "%s" (macroexp--obsolete-warning
                                cmd (get cmd 'byte-obsolete-info) "command")))))))))))

(when-emacs-version (<= 28 it)
  (el-patch-defun command-execute (cmd &optional record-flag keys special)
    ;; BEWARE: Called directly from the C code.
    "Execute CMD as an editor command.
CMD must be a symbol that satisfies the `commandp' predicate.

Optional second arg RECORD-FLAG non-nil means unconditionally put
this command in the variable `command-history'.  Otherwise, that
is done only if an arg is read using the minibuffer.

The argument KEYS specifies the value to use instead of the
return value of the `this-command-keys' function when reading the
arguments; if it is nil, `this-command-keys' is used.

The argument SPECIAL, if non-nil, means that this command is
executing a special event, so ignore the prefix argument and
don't clear it."
    (setq debug-on-next-call nil)
    (let ((prefixarg (unless special
                       ;; FIXME: This should probably be done around
                       ;; pre-command-hook rather than here!
                       (prog1 prefix-arg
                         (setq current-prefix-arg prefix-arg)
                         (setq prefix-arg nil)
                         (el-patch-add
                           (setq vim--current-universal-argument-provided? vim--universal-argument-provided?)
                           (setq vim--universal-argument-provided? nil))
                         (when current-prefix-arg
                           (prefix-command-update))))))
      (if (and (symbolp cmd)
               (get cmd 'disabled)
               disabled-command-function)
          ;; FIXME: Weird calling convention!
          (run-hooks 'disabled-command-function)
        (let ((final cmd))
          (while
              (progn
                (setq final (indirect-function final))
                (if (autoloadp final)
                    (setq final (autoload-do-load final cmd)))))
          (cond
            ((arrayp final)
             ;; If requested, place the macro in the command history.  For
             ;; other sorts of commands, call-interactively takes care of this.
             (when record-flag
               (add-to-history
                'command-history `(execute-kbd-macro ,final ,prefixarg) nil t))
             (execute-kbd-macro final prefixarg))
            (t
             ;; Pass `cmd' rather than `final', for the backtrace's sake.
             (prog1 (call-interactively cmd record-flag keys)
               (when (and (symbolp cmd)
                          (get cmd 'byte-obsolete-info)
                          (not (get cmd 'command-execute-obsolete-warned)))
                 (put cmd 'command-execute-obsolete-warned t)
                 (message "%s" (macroexp--obsolete-warning
                                cmd (get cmd 'byte-obsolete-info) "command")))))))))))

;;;###autoload
(el-patch-feature thingatpt)

(when-emacs-version (<= 28 it)
  (require 'thingatpt)
  (el-patch-defun thing-at-point--bounds-of-well-formed-url (beg end pt)
    (save-excursion
      (goto-char beg)
      (let (url-beg paren-end regexp)
        (save-restriction
	  (narrow-to-region beg end)
	  ;; The scheme component must either match at BEG, or have no
	  ;; other alphanumerical ASCII characters before it.
	  (setq regexp (el-patch-swap
                         (concat "\\(?:\\`\\|[^a-zA-Z0-9]\\)\\("
			         (or thing-at-point-beginning-of-url-regexp
				     (regexp-opt thing-at-point-uri-schemes))
			         "\\)")
                         (if thing-at-point-beginning-of-url-regexp
                             (concat "\\(?:\\`\\|[^a-zA-Z0-9]\\)\\("
                                     thing-at-point-beginning-of-url-regexp
			             "\\)")
                           (eval-when-compile
                             (concat "\\(?:\\`\\|[^a-zA-Z0-9]\\)\\("
			             (regexp-opt thing-at-point-uri-schemes)
			             "\\)")))))
	  (and (re-search-forward regexp end t)
	       ;; URI must have non-empty contents.
	       (< (point) end)
	       (setq url-beg (match-beginning 1))))
        (when url-beg
	  ;; If there is an open paren before the URI, truncate to the
	  ;; matching close paren.
	  (and (> url-beg (point-min))
	       (eq (car-safe (syntax-after (1- url-beg))) 4)
	       (save-restriction
	         (narrow-to-region (1- url-beg) (min end (point-max)))
	         (setq paren-end (ignore-errors
                                   ;; Make the scan work inside comments.
                                   (let ((parse-sexp-ignore-comments nil))
                                     (scan-lists (1- url-beg) 1 0)))))
	       (not (blink-matching-check-mismatch (1- url-beg) paren-end))
	       (setq end (1- paren-end)))
	  ;; Ensure PT is actually within BOUNDARY. Check the following
	  ;; example with point on the beginning of the line:
	  ;;
	  ;; 3,1406710489,https://gnu.org,0,"0"
	  (and (<= url-beg pt end) (cons url-beg end)))))))

(el-patch-defun universal-argument--description ()
  (when prefix-arg
    (concat (el-patch-swap
              "C-u"
              (if vim--universal-argument-provided?
                  "[C-u]"
                "C-u"))
            (pcase prefix-arg
              ('(-) " -")
              (`(,(and (pred integerp) n))
               (let ((str ""))
                 (while (and (> n 4) (= (mod n 4) 0))
                   (setq str (concat str " C-u"))
                   (setq n (/ n 4)))
                 (if (= n 4) str (format " %s" prefix-arg))))
              (_ (format " %s" prefix-arg))))))

(el-patch-defun prefix-command-preserve-state ()
  "Pass the current prefix command state to the next command.
Should be called by all prefix commands.
Runs `prefix-command-preserve-state-hook'."
  (el-patch-swap
    (run-hooks 'prefix-command-preserve-state-hook)
    (setf prefix-arg current-prefix-arg
          vim--universal-argument-provided? vim--current-universal-argument-provided?))
  ;; If the current command is a prefix command, we don't want the next (real)
  ;; command to have `last-command' set to, say, `universal-argument'.
  (setq this-command last-command)
  (setq real-this-command real-last-command)
  (prefix-command-update))

(el-patch-defun universal-argument ()
  "Begin a numeric argument for the following command.
Digits or minus sign following \\[universal-argument] make up the numeric argument.
\\[universal-argument] following the digits or minus sign ends the argument.
\\[universal-argument] without digits or minus sign provides 4 as argument.
Repeating \\[universal-argument] without digits or minus sign
 multiplies the argument by 4 each time.
For some commands, just \\[universal-argument] by itself serves as a flag
that is different in effect from any particular numeric argument.
These commands include \\[set-mark-command] and \\[start-kbd-macro]."
  (interactive)
  (prefix-command-preserve-state)
  (el-patch-add
    (setf vim--universal-argument-provided? t))
  (setq prefix-arg (list 4))
  (universal-argument--mode))

(el-patch-defun universal-argument-more (arg)
  ;; A subsequent C-u means to multiply the factor by 4 if we've typed
  ;; nothing but C-u's; otherwise it means to terminate the prefix arg.
  (interactive "P")
  (prefix-command-preserve-state)
  (el-patch-add
    (setf vim--universal-argument-provided? t))
  (setq prefix-arg (if (consp arg)
                       (list (* 4 (car arg)))
                     (if (eq arg '-)
                         (list -4)
                       arg)))
  (when (consp prefix-arg) (universal-argument--mode)))

(provide 'base-emacs-opt)
;; Local Variables:
;; End:

;; base-emacs-opt.el ends here
