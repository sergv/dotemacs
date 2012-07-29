;;; elidoc.el --- show Common Lisp function arglist in echo area


;; Created from eldoc.el by Andras Simon <asimon@math.bme.hu>
;; Original author of eldoc.el: Noah Friedman <friedman@splode.com>
;; Maintainer: asimon@math.bme.hu
;; Keywords: ?
;; Created: 2000-02-20

;; elidoc.el 0.1 2000/02/23

;; This is a modification of eldoc.el. The original copyright follows:

;; Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; The code also uses parts of fi-lep.el, which is
;; Copyright (c) 1987-1993 Franz Inc, Berkeley, Ca.

;;; Commentary:

;; This program was inspired by the behavior of the "mouse documentation
;; window" on many Lisp Machine systems; as you type a function's symbol
;; name as part of a sexp, it will print the argument list for that
;; function.  Behavior is not identical; for example, you need not actually
;; type the function name, you need only move point around in a sexp that
;; calls it.

;; One useful way to enable this minor mode is to put the following in your
;; .emacs:
;;
;;      (autoload 'turn-on-elidoc-mode "elidoc" nil t)
;;      (add-hook 'fi:common-lisp-mode-hook 'turn-on-elidoc-mode)
;;      (add-hook 'fi:inferior-common-lisp-mode-hook 'turn-on-elidoc-mode)
;;      (add-hook 'fi:lisp-listener-mode-hook 'turn-on-elidoc-mode)

;;Changes from eldoc:
;; -there's no variable documentation lookup
;;             (but I left the infrastructure in, just in case)
;; -this is string, rather than symbol based
;; -eldoc is changed to elidoc in function and variable names, to avoid clashes
;;  with the original
;;
;; Functions that aren't just copied from eldoc :
;; elidoc-mode
;; elidoc-print-current-symbol-info
;; elidoc-get-fnsym-args-string
;; elidoc-function-argstring (gets argstring from CL)
;; elidoc-current-symbol (returns string, not symbol)

;; TODO:
;; make elidoc-function-argstring less hackish
;; do variable documentation lookup
;; use more eli functions, less vanilla emacs?
;; get rid of equalp?

;;; Code:

;; Use idle timers if available in the version of emacs running.
;; Please don't change this to use `require'; this package works as-is in
;; XEmacs (which doesn't have timer.el as of 19.14), and I would like to
;; maintain compatibility with that since I must use it sometimes.  --Noah
(or (featurep 'timer)
    (load "timer" t))

(defgroup elidoc nil
  "Show function arglist or variable docstring in echo area."
  :group 'extensions)

;;;###autoload
(defcustom elidoc-mode nil
  "*If non-nil, show the defined parameters for the common lisp function near point.

For the  lisp function at the beginning of the sexp which point is
within, show the defined parameters for the function in the echo area.
This information is extracted via LEP.


This variable is buffer-local."
  :type 'boolean
  :group 'elidoc)
(make-variable-buffer-local 'elidoc-mode)

(defcustom elidoc-idle-delay 0.10
  "*Number of seconds of idle time to wait before printing.
If user input arrives before this interval of time has elapsed after the
last input, no documentation will be printed.

If this variable is set to 0, no idle time is required."
  :type 'number
  :group 'elidoc)

(defcustom elidoc-minor-mode-string " Elidoc"
  "*String to display in mode line when Elidoc Mode is enabled."
  :type 'string
  :group 'elidoc)

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'elidoc-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((elidoc-mode elidoc-minor-mode-string)))))

(defcustom elidoc-argument-case 'upcase
  "Case to display argument names of functions, as a symbol.
This has two preferred values: `upcase' or `downcase'.
Actually, any name of a function which takes a string as an argument and
returns another string is acceptable."
  :type '(radio function
          (function-item upcase)
          (function-item downcase))
  :group 'elidoc)

;; No user options below here.

;; Commands after which it is appropriate to print in the echo area.
;; Elidoc does not try to print function arglists, etc. after just any command,
;; because some commands print their own messages in the echo area and these
;; functions would instantly overwrite them.  But self-insert-command as well
;; as most motion commands are good candidates.
;; This variable contains an obarray of symbols; do not manipulate it
;; directly.  Instead, use `elidoc-add-command' and `elidoc-remove-command'.
(defvar elidoc-message-commands nil)

;; This is used by elidoc-add-command to initialize elidoc-message-commands
;; as an obarray.
;; It should probably never be necessary to do so, but if you
;; choose to increase the number of buckets, you must do so before loading
;; this file since the obarray is initialized at load time.
;; Remember to keep it a prime number to improve hash performance.
(defvar elidoc-message-commands-table-size 31)

;; Bookkeeping; elements are as follows:
;;   0 - contains the last symbol read from the buffer.
;;   1 - contains the string last displayed in the echo area for that
;;       symbol, so it can be printed again if necessary without reconsing.
;;   2 - 'function if function args, 'variable if variable documentation.
(defvar elidoc-last-data (make-vector 3 nil))
(defvar elidoc-last-message nil)

;; Idle timers are supported in Emacs 19.31 and later.
(defvar elidoc-use-idle-timer-p (fboundp 'run-with-idle-timer))

;; elidoc's timer object, if using idle timers
(defvar elidoc-timer nil)

;; idle time delay currently in use by timer.
;; This is used to determine if elidoc-idle-delay is changed by the user.
(defvar elidoc-current-idle-delay elidoc-idle-delay)


;;;###autoload
(defun elidoc-mode (&optional prefix)
  "*Enable or disable elidoc mode.
See documentation for the variable of the same name for more details.

If called interactively with no prefix argument, toggle current condition
of the mode.
If called with a positive or negative prefix argument, enable or disable
the mode, respectively."
  (interactive "P")
  (setq elidoc-last-message nil)
  (cond (elidoc-use-idle-timer-p
	 (elidoc-schedule-timer)
	 ;; ^^ was:   (add-hook 'post-command-hook 'elidoc-schedule-timer)
	 ;; but I don't see why this is needed
         (add-hook 'pre-command-hook 'elidoc-pre-command-refresh-echo-area))
        (t
         ;; Use post-command-idle-hook if defined, otherwise use
         ;; post-command-hook.  The former is only proper to use in Emacs
         ;; 19.30; that is the first version in which it appeared, but it
         ;; was obsolesced by idle timers in Emacs 19.31.
         (add-hook (if (boundp 'post-command-idle-hook)
                       'post-command-idle-hook
                       'post-command-hook)
                   'elidoc-print-current-symbol-info)
         ;; quick and dirty hack for seeing if this is XEmacs
         (and (fboundp 'display-message)
              (add-hook 'pre-command-hook
                        'elidoc-pre-command-refresh-echo-area))))
  (setq elidoc-mode (if prefix
                        (>= (prefix-numeric-value prefix) 0)
                        (not elidoc-mode)))
  (and (interactive-p)
       (if elidoc-mode
           (message "elidoc-mode is enabled")
           (message "elidoc-mode is disabled")))
  elidoc-mode)

;;;###autoload
(defun turn-on-elidoc-mode ()
  "Unequivocally turn on elidoc-mode (see variable documentation)."
  (interactive)
  (elidoc-mode 1))


;; Idle timers are part of Emacs 19.31 and later.
(defun elidoc-schedule-timer ()
  (or (and elidoc-timer
           (memq elidoc-timer timer-idle-list))
      (setq elidoc-timer
            (run-with-idle-timer elidoc-idle-delay t
                                 'elidoc-print-current-symbol-info)))

  ;; If user has changed the idle delay, update the timer.
  (cond ((not (= elidoc-idle-delay elidoc-current-idle-delay))
         (setq elidoc-current-idle-delay elidoc-idle-delay)
         (timer-set-idle-time elidoc-timer elidoc-idle-delay t))))

(defun elidoc-message (&rest args)
  (let ((omessage elidoc-last-message))
    (cond ((equalp (car args) elidoc-last-message))
          ((or (null args)
               (null (car args)))
           (setq elidoc-last-message nil))
          ;; If only one arg, no formatting to do so put it in
          ;; elidoc-last-message so eq test above might succeed on
          ;; subsequent calls.
          ((null (cdr args))
           (setq elidoc-last-message (car args)))
          (t
           (setq elidoc-last-message (apply 'format args))))
    ;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages
    ;; are recorded in a log.  Do not put elidoc messages in that log since
    ;; they are Legion.
    (cond ((fboundp 'display-message)
           ;; XEmacs 19.13 way of preventing log messages.
           (cond (elidoc-last-message
                  (display-message 'no-log elidoc-last-message))
                 (omessage
                  (clear-message 'no-log))))
          (t
           ;; Emacs way of preventing log messages.
           (let ((message-log-max nil))
             (cond (elidoc-last-message
                    (message "%s" elidoc-last-message))
                   (omessage
                    (message nil)))))))
  elidoc-last-message)

;; This function goes on pre-command-hook for XEmacs or when using idle
;; timers in Emacs.  Motion commands clear the echo area for some reason,
;; which make elidoc messages flicker or disappear just before motion
;; begins.  This function reprints the last elidoc message immediately
;; before the next command executes, which does away with the flicker.
;; This doesn't seem to be required for Emacs 19.28 and earlier.
(defun elidoc-pre-command-refresh-echo-area ()
  (and elidoc-last-message
       (if (elidoc-display-message-no-interference-p)
           (elidoc-message elidoc-last-message)
           (setq elidoc-last-message nil))))

;; Decide whether now is a good time to display a message.
(defun elidoc-display-message-p ()
  (and (elidoc-display-message-no-interference-p)
       (cond (elidoc-use-idle-timer-p
              ;; If this-command is non-nil while running via an idle
              ;; timer, we're still in the middle of executing a command,
              ;; e.g. a query-replace where it would be annoying to
              ;; overwrite the echo area.
              (and (not this-command)
                   (symbolp last-command)
                   (intern-soft (symbol-name last-command)
                                elidoc-message-commands)))
             (t
              ;; If we don't have idle timers, this function is
              ;; running on post-command-hook directly; that means the
              ;; user's last command is still on `this-command', and we
              ;; must wait briefly for input to see whether to do display.
              (and (symbolp this-command)
                   (intern-soft (symbol-name this-command)
                                elidoc-message-commands)
                   (sit-for elidoc-idle-delay))))))

(defun elidoc-display-message-no-interference-p ()
  (and elidoc-mode
       (not executing-kbd-macro)
       ;; Having this mode operate in an active minibuffer/echo area causes
       ;; interference with what's going on there.
       (not cursor-in-echo-area)
       (not (eq (selected-window) (minibuffer-window)))))


(defun elidoc-print-current-symbol-info ()
  (and (elidoc-display-message-p)
       (let* ((current-fnsym  (elidoc-fnsym-in-current-sexp))
              (doc (cond ((null current-fnsym)  nil)
			 (t (elidoc-get-fnsym-args-string current-fnsym)))))
	 (elidoc-message doc))))



;; Return a string containing the function parameter list, or 1-line
;; docstring if function is a subr and no arglist is obtainable from the
;; docstring or elsewhere.

(defun elidoc-get-fnsym-args-string (sym)
  (let ((args "")
	(doc nil))
    (cond ((= (aref sym (1- (length sym))) 58)) ;sym ends with a colon
	  ((not  (fi:eval-in-lisp (format "(not (null (fboundp %s)))\n"  (list 'quote (make-symbol  sym))))))
	  ;;^^causes trouble if the symbol ends with colon
	  ((and
	    (equalp sym (aref elidoc-last-data 0))
	    (eq 'function (aref elidoc-last-data 2)))
	   (setq doc (aref elidoc-last-data 1)))
	  (t  (setq args (elidoc-function-argstring sym))))
    (cond ((not (equal args ""))
	   (setq doc (elidoc-docstring-format-sym-doc sym args))
	   (elidoc-last-data-store sym doc 'function)))
    doc))


(defun elidoc-function-argstring (string)
  ;; communication goes through a buffer - this is an  ugly hack,
  ;; but I couldn't make with-output-to-string & friends work.
  ;; There's also a timing problem: fi::make-request seems to work
  ;; asynchronously, and I'm not sure this is the right way around that.
                                        ;  (cancel-timer elidoc-timer) ;paranoia
  (let ((pack fi:package))
    (save-excursion
      (with-current-buffer (get-buffer-create "*arglist-buffer*")
	(let ((fi:package pack))
	  (erase-buffer)(princ "*" (get-buffer "*arglist-buffer*"))
	  (fi::make-request (lep::arglist-session :fspec string)
			    ;; Normal continuation
			    (() (what arglist)
			     (progn  (erase-buffer) (princ arglist (get-buffer "*arglist-buffer*"))))
			    ;; Error continuation
			    ((string) (error) (erase-buffer)))
					; (fi:eval-in-lisp (format "t\n"  ))
	  (while (equal (buffer-string) "*") (sleep-for 0.1))
					;     ^gets into inf loop without this
                                        ;	  (elidoc-schedule-timer) ;paranoia
	  (buffer-string))))))

;; Return a string containing a brief (one-line) documentation string for
;; the variable.
(defun elidoc-get-var-docstring (sym) ;not used
  (cond ((and (eq sym (aref elidoc-last-data 0))
              (eq 'variable (aref elidoc-last-data 2)))
         (aref elidoc-last-data 1))
        (t
         (let ((doc (documentation-property sym 'variable-documentation t)))
           (cond (doc
                  (setq doc (elidoc-docstring-format-sym-doc
                             sym (elidoc-docstring-first-line doc)))
                  (elidoc-last-data-store sym doc 'variable)))
           doc))))

(defun elidoc-last-data-store (symbol doc type)
  (aset elidoc-last-data 0 symbol)
  (aset elidoc-last-data 1 doc)
  (aset elidoc-last-data 2 type))

;; Note that any leading `*' in the docstring (which indicates the variable
;; is a user option) is removed.
(defun elidoc-docstring-first-line (doc)
  (and (stringp doc)
       (substitute-command-keys
        (save-match-data
          (let ((start (if (string-match "^\\*" doc) (match-end 0) 0)))
            (cond ((string-match "\n" doc)
                   (substring doc start (match-beginning 0)))
                  ((zerop start) doc)
                  (t (substring doc start))))))))

;; If the entire line cannot fit in the echo area, the symbol name may be
;; truncated or eliminated entirely from the output to make room for the
;; description.
(defun elidoc-docstring-format-sym-doc (name doc)
  (save-match-data
    (let* ((doclen (+ (length name) (length ": ") (length doc)))
           ;; Subtract 1 from window width since emacs seems not to write
           ;; any chars to the last column, at least for some terminal types.
           (strip (- doclen (1- (window-width (minibuffer-window))))))
      (cond ((> strip 0)
             (let* ((len (length name)))
               (cond ((>= strip len)
                      (format "%s" doc))
                     (t
                      ;;(setq name (substring name 0 (- len strip)))
                      ;;
                      ;; Show the end of the partial symbol name, rather
                      ;; than the beginning, since the former is more likely
                      ;; to be unique given package namespace conventions.
                      (setq name (substring name strip))
                      (format "%s: %s" name doc)))))
            (t
             (format "%s: %s" name doc))))))


(defun elidoc-fnsym-in-current-sexp ()
  (let ((p (point)))
    (elidoc-beginning-of-sexp)
    (prog1
        ;; Don't do anything if current word is inside a string.
        (if (= (or (char-after (1- (point))) 0) ?\")
            nil
            (elidoc-current-symbol))
      (goto-char p))))

(defun elidoc-beginning-of-sexp ()
  (let ((parse-sexp-ignore-comments t))
    (condition-case err
        (while (progn
                 (forward-sexp -1)
                 (or (= (or (char-after (1- (point)))) ?\")
                     (> (point) (point-min)))))
      (error nil))))

;; used to return nil unless current word is an interned symbol
;; but not anymore
(defun elidoc-current-symbol ()
  (let ((c (char-after (point))))
    (and c
         (memq (char-syntax c) '(?w ?_))
         (current-word))))


(defun elidoc-function-argstring-format (arglist)
  (cond ((not (listp arglist))
         (setq arglist nil))
        ((symbolp (car arglist))
         (setq arglist
               (mapcar (function (lambda (s)
                         (if (memq s '(&optional &rest))
                             (symbol-name s)
                             (funcall elidoc-argument-case
                                      (symbol-name s)))))
                       arglist)))
        ((stringp (car arglist))
         (setq arglist
               (mapcar (function (lambda (s)
                         (if (member s '("&optional" "&rest"))
                             s
                             (funcall elidoc-argument-case s))))
                       arglist))))
  (concat "(" (mapconcat 'identity arglist " ") ")"))



;; When point is in a sexp, the function args are not reprinted in the echo
;; area after every possible interactive command because some of them print
;; their own messages in the echo area; the elidoc functions would instantly
;; overwrite them unless it is more restrained.
;; These functions do display-command table management.

(defun elidoc-add-command (&rest cmds)
  (or elidoc-message-commands
      (setq elidoc-message-commands
            (make-vector elidoc-message-commands-table-size 0)))

  (let (name sym)
    (while cmds
      (setq name (car cmds))
      (setq cmds (cdr cmds))

      (cond ((symbolp name)
             (setq sym name)
             (setq name (symbol-name sym)))
            ((stringp name)
             (setq sym (intern-soft name))))

      (and (symbolp sym)
           (fboundp sym)
           (set (intern name elidoc-message-commands) t)))))

(defun elidoc-add-command-completions (&rest names)
  (while names
    (apply 'elidoc-add-command
           (all-completions (car names) obarray 'fboundp))
    (setq names (cdr names))))

(defun elidoc-remove-command (&rest cmds)
  (let (name)
    (while cmds
      (setq name (car cmds))
      (setq cmds (cdr cmds))

      (and (symbolp name)
           (setq name (symbol-name name)))

      (if (fboundp 'unintern)
          (unintern name elidoc-message-commands)
          (let ((s (intern-soft name elidoc-message-commands)))
            (and s
                 (makunbound s)))))))

(defun elidoc-remove-command-completions (&rest names)
  (while names
    (apply 'elidoc-remove-command
           (all-completions (car names) elidoc-message-commands))
    (setq names (cdr names))))


;; Prime the command list.
(elidoc-add-command-completions
 "backward-" "beginning-of-" "delete-other-windows" "delete-window"
 "end-of-" "forward-" "indent-for-tab-command" "goto-" "mouse-set-point"
 "next-" "other-window" "previous-" "recenter" "scroll-"
 "self-insert-command" "split-window-"
 "up-list" "down-list")

(provide 'elidoc)

;;; elidoc.el ends here
