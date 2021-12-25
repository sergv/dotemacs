;; base-emacs-opt.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 11 December 2021
;; Description:

(require 'base-emacs-fixes)

(defvar vim--current-universal-argument-provided?)
(defvar vim--universal-argument-provided?)

(cond
  ((eval-when-compile
     (base-emacs-fixes--is-version 27))

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
  (t
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
                                 cmd (get cmd 'byte-obsolete-info) "command"))))))))))))

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
