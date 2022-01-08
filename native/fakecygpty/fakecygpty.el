;;; fakecygpty.el --- Support for using cygwin pty with NTEmacs.

;; Copyright (C) 2014, 2016  Daisuke Kobayashi

;; Author: Daisuke Kobayashi <d5884jp@gmail.com>
;; Version: 0.1
;; Keywords: processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package supplies cygwin's pty feature for NTEmacs.
;; If you want to use the command requires pty such as `bash', try this.

;;; Install:

;; 1. Compile `fakecygpty.exe' and `qkill.exe', and copy them into somewhere on `exec-path'.
;; 2. Copy this .el into somewhere on `load-path'.

;;; Usage:

;; Put below into your init.el:

;;   (require 'fakecygpty)
;;   (fakecygpty-activate)

;;; Code:

(defgroup fakecygpty nil
  "Execute cygwin pty commands using pipe."
  :group 'processes)

(defcustom fakecygpty-program (executable-find "fakecygpty")
  "Program file name of fakecygpty."
  :group 'fakecygpty
  :type 'file)

(defcustom fakecygpty-qkill-program (executable-find "qkill")
  "Program file name of qkill."
  :group 'fakecygpty
  :type 'file)

(defcustom fakecygpty-whitelist-program-regexps
  (list (rx bos (or "bash" "sh" "dash" "zsh") (? ".exe") eos))
  "Regexp list for program that run with fakecygpty."
  :group 'fakecygpty
  :type '(repeat regexp))

(defvar fakecygpty--advice-defined nil
  "If t, advices for fakecygpty are defined.")

(defvar fakecygpty--activated nil
  "If t, fakecygpty is activated.")

;;;###autoload
(defun fakecygpty-activate ()
  "Activate fakecygpty features."
  (interactive)
  (when (and (not fakecygpty--activated)
	     (memq system-type '(ms-dos windows-nt))
	     fakecygpty-program
	     (executable-find fakecygpty-program)
	     fakecygpty-qkill-program
	     (executable-find fakecygpty-qkill-program))
    (unless fakecygpty--advice-defined
      (fakecygpty--make-advice)
      (setq fakecygpty--advice-defined t))

    (setq fakecygpty--activated t)))

;;;###autoload
(defun fakecygpty-deactivate ()
  "Deactivate fakecygpty features."
  (interactive)
  (when fakecygpty--activated

    (advice-remove 'start-process #'fakecygpty--start-process)
    (advice-remove 'process-command #'fakecygpty--process-command)
    (advice-remove 'process-tty-name #'fakecygpty--process-tty-name)
    (advice-remove 'process-status #'fakecygpty--process-status)
    (advice-remove 'process-send-eof #'fakecygpty--process-send-eof)
    (advice-remove 'signal-process #'fakecygpty--signal-process)

    (advice-remove 'interrupt-process #'fakecygpty--interrupt-process)
    (advice-remove 'quit-process #'fakecygpty--quit-process)
    (advice-remove 'stop-process #'fakecygpty--stop-process)
    (advice-remove 'continue-process #'fakecygpty--continue-process)
    (advice-remove 'kill-process #'fakecygpty--kill-process)

    (advice-remove 'set-process-window-size #'fakecygpty--set-process-window-size)
    (advice-remove 'gdb-io-interrupt #'fakecygpty--gdb-io-interrupt-workaround)

    (setq fakecygpty--activated nil)))

(defun fakecygpty-process-p (process)
  "Non-nil when PROCESS was invoked by fakecygpty."
  (and (processp process)
       (process-get process :fakecygpty-p)))

(defun fakecygpty-qkill (process sigcode &optional sigval as-winpid tty except)
  "Send PROCESS the signal with code SIGCODE by `fakecygpty-qkill-program'.
PROCESS may also be a number specifying the process id of the
process to signal; in this case, the process need not be a child of
this Emacs.
SIGCODE may be an integer, or a symbol whose name is a signal name.
SIGVAL may be integer.  if it's nil, 0 will be used.
If AS-WINPID is non-nil, PROCESS is considered as windows pid.
If TTY is specified, send signal to TTY's foreground process group.
If EXCEPT is non-nil and TTY is specified, don't send signal when
TTY's foreground process group pgid equals PROCESS pid."
  (let ((pid (cond
	      ((integerp process)
	       process)
	      ((processp process)
	       (process-id process))
	      ((stringp process)
	       (ignore-errors (process-id (get-process process))))
	      ((null process)
	       (ignore-errors (process-id (get-buffer-process
					   (current-buffer)))))
	      (t nil))))
    (when pid
      (zerop (apply 'call-process fakecygpty-qkill-program nil nil nil
		    (delq nil `(,@(if as-winpid
				    (list "-w"))
				"-s" ,(prin1-to-string sigcode t)
				,@(when (integerp sigval)
				    (list "-i" (number-to-string sigval)))
				,@(when tty
				    (list "-t" tty))
				,@(when except
				    (list "-e" (number-to-string pid)))
				,@(unless tty
				    (list (number-to-string pid)))))))
		    )))

(defun fakecygpty-real-process-id (process &optional as-winpid)
  "Return subprocess's process-id if PROCESS was invoked by fakecygpty."
  (if (fakecygpty-process-p process)
      (with-temp-buffer
	(when (zerop
	       (call-process
		"sh" nil (current-buffer) nil
		"-c"
		(format (if as-winpid
			    "cat `dirname \\`grep -l %s /proc/*/ppid 2>/dev/null\\``/winpid"
			  "basename `dirname \\`grep -l %s /proc/*/ppid 2>/dev/null\\``")
			(process-id process))))
	  (ignore-errors
	    (save-match-data
	      (string-to-number (replace-regexp-in-string "\r?\n" ""
							  (buffer-string))))))
	)
    (process-id process)))

(defun fakecygpty--whitelisted-program (program)
  "Return non-nil if PROGRAM is run without fakecygpty on `start-process'.
An ignored pattern is used from `fakecygpty-ignored-program-regexps'"
  (let ((prog (file-name-nondirectory program))
        ;; Ignore case since this whole module is purely for Windows.
        (case-fold-search t)
        (all-match? t)
        (regexps fakecygpty-whitelist-program-regexps))
    (while (and all-match?
                regexps)
      (setf all-match (string-match-p (car regexps) prog)
            regexps (cdr regexps)))
    all-match?))

(defun fakecygpty--normalize-process-arg (target)
  "Return process object of TARGET.
TARGET may be a process, a buffer, or the name of process or buffer.
nil means current buffer's process."
  (cond
   ((processp target)
    target)
   ((or (bufferp target)
	(null target))
    (or (get-buffer-process (or target (current-buffer)))
	(error "Buffer %s has no process" (buffer-name target))))
   ((stringp target)
    (fakecygpty--normalize-process-arg (or (get-process target)
					   (get-buffer target)
					   (error "Process %s does not exist." target))))
   (t
    (signal 'wrong-type-argument (list 'processp target)))))

(defun fakecygpty--process-send-special-char (process type &optional tty-name)
  "Send PROCESS the special char of TYPE from PROCESS's tty."
  (let ((tty (or tty-name (process-tty-name process))))
    (when tty
      (let ((special-char
		 (with-temp-buffer
		   (when (zerop (call-process "stty" nil (current-buffer) nil "-a" "-F" tty))
		     (save-match-data
		       (goto-char (point-min))
                       (let ((re (concat type " = \\(\\^?\\)\\([^;]+\\);")))
                         (when (re-search-forward re nil t)
                           (let ((match-1 (match-string 1))
                                 (match-2 (match-string 2)))
                             (unless (equal match-2 "<undef>")
                               (if (equal match-1 "^")
                                   (logand (aref match-2 0) #o037)
                                 (aref match-2 0)))))))))))
	(when special-char
	  (process-send-string process (char-to-string special-char))
	  t)))))

(defun fakecygpty--start-process (old-start-proc name buf program &rest program-args)
  "If `process-connection-type' is non-nil, invoke PROGRAM by `fakecygpty-program'."
  (if (and process-connection-type      ; if non-nil, required pty.
           ;; program
           (or (not program)
               (fakecygpty--whitelisted-program program)))
      (let ((proc (apply old-start-proc
                         name
                         buf
                         fakecygpty-program
                         ;; insert fakecygpty at program file name position.
                         (if program
                             (cons program program-args)
                           program-args))))
        (when (processp proc)
          (process-put proc
                       :fakecygpty-p
                       (if program-args t 'pty)))
        proc)
    (apply old-start-proc
           name
           buf
           program
           program-args)))

(defun fakecygpty--process-command (old-processs-command proc)
  "Return real command name if PROCESS was invoked by fakecygpty."
  (let ((result (funcall old-processs-command proc)))
    (if (fakecygpty-process-p proc)
        (cdr result)
      result)))

(defun fakecygpty--get-pty-process-tty-name (proc real-proc-id)
  "Return tty name if PROCESS was invoked by fakecygpty."
  (with-temp-buffer
    ;; NTEmacs cannot see cygwin's `/proc' file-system, so using cygwin program.
    ;; Finding fakecygpty's tty-name.
    (if (zerop (call-process
                "cat" nil (current-buffer) nil
                (format "/proc/%s/ctty" real-proc-id)))
        (replace-regexp-in-string "\r?\n"
                                  ""
                                  (buffer-substring-no-properties (point-min) (point-max)))
      "?")))

(defun fakecygpty--process-tty-name-impl (proc real-proc-id)
  "Return tty name if PROCESS was invoked by fakecygpty."
  (if (fakecygpty-process-p proc)
      (fakecygpty--get-pty-process-tty-name proc real-proc-id)
    (funcall old-process-tty-name proc)))

(defun fakecygpty--process-tty-name (old-process-tty-name proc)
  "Return tty name if PROCESS was invoked by fakecygpty."
  (if (fakecygpty-process-p proc)
      (fakecygpty--get-pty-process-tty-name proc (fakecygpty-real-process-id proc))
    (funcall old-process-tty-name proc)))

(defun fakecygpty--process-status (old-process-status proc)
  "Change return value 'exit to 'failed for pty allocation only mode."
  (let ((result (funcall old-process-status proc)))
    (let ((proc (fakecygpty--normalize-process-arg proc)))
      (if (and (eq (fakecygpty-process-p proc) 'pty)
               (memq result '(exit signal)))
          'failed)
      result)))

(defun fakecygpty--process-send-eof (old-process-send-eof &optional process)
  "Send raw C-d code if PROCESS was invoked by fakecygpty."
  (let ((proc (fakecygpty--normalize-process-arg process)))
    (if (fakecygpty-process-p proc)
        (progn
          (fakecygpty--process-send-special-char proc "eof")
          proc)
      (funcall old-process-send-eof process))))

(defun fakecygpty--signal-process (old-signal-process proc sigcode)
  "Send signal by `fakecygpty-qkill' for cygwin process.
So it's able to send any type signal.
For windows process, Emacs native `signal-process' will be invoked."
  (if (fakecygpty-qkill proc sigcode nil t)
      0
    (funcall old-signal-process proc sigcode)))

(defmacro fakecygpty--make-signal-advice (advice-name sig cc)
  `(defun ,advice-name (old-func &optional process current-group)
     ,(format "Send %s signal by `fakecygpty-qkill'" (eval sig))
     (let ((proc (fakecygpty--normalize-process-arg process)))
       (if (and (eq (process-type proc) 'real)
                (let ((current-grp (and (fakecygpty-process-p proc) current-group))
                      (pid (fakecygpty-real-process-id proc)))
                  (if current-grp
                      (let ((tty (if (fakecygpty-process-p proc)
                                     (fakecygpty--get-pty-process-tty-name proc pid)
                                   (process-tty-name proc))))
                        (cond
                          ,@(when cc
                              `(((fakecygpty--process-send-special-char proc ,cc tty)
                                 t)))
                          ((eq current-grp 'lambda)
                           (fakecygpty-qkill pid ,sig nil nil tty t))
                          (t
                           (fakecygpty-qkill pid ,sig nil nil tty))))
                    (fakecygpty-qkill (- pid) ,sig))))
           proc
         (funcall old-func process current-group)))))

(fakecygpty--make-signal-advice fakecygpty--interrupt-process 'SIGINT "intr")
(fakecygpty--make-signal-advice fakecygpty--quit-process 'SIGQUIT "quit")
(fakecygpty--make-signal-advice fakecygpty--stop-process 'SIGTSTP "susp")
(fakecygpty--make-signal-advice fakecygpty--continue-process 'SIGCONT nil)
(fakecygpty--make-signal-advice fakecygpty--kill-process 'SIGKILL nil)

(defun fakecygpty--set-process-window-size (old-set-process-window-size proc height width)
  "Send SIGWINCH signal with a window size information when process is invoked by `fakecygpty'.
The window size information is caluclated by lines * 65536 + columns."
  (if (fakecygpty-process-p proc)
      (fakecygpty-qkill proc 'SIGWINCH
                        (+ (* 65536 height)
                           width))
    (funcall old-set-process-window-size proc height width)))

(defun fakecygpty--gdb-io-interrupt-workaround (old-gdb-io-interrupt)
  (if fakecygpty--activated
      (progn
        (advice-remove 'interrupt-process #'fakecygpty--interrupt-process)
        (funcall old-gdb-io-interrupt)
        (advice-add 'interrupt-process :around #'fakecygpty--interrupt-process))
    (funcall old-gdb-io-interrupt)))

(defun fakecygpty--make-advice ()
  "Make advices for fakecygpty and qkill."

  (advice-add 'start-process :around #'fakecygpty--start-process)
  (advice-add 'process-command :around #'fakecygpty--process-command)
  (advice-add 'process-tty-name :around #'fakecygpty--process-tty-name)
  (advice-add 'process-status :around #'fakecygpty--process-status)
  (advice-add 'process-send-eof :around #'fakecygpty--process-send-eof)
  (advice-add 'signal-process :around #'fakecygpty--signal-process)

  (advice-add 'interrupt-process :around #'fakecygpty--interrupt-process)
  (advice-add 'quit-process :around #'fakecygpty--quit-process)
  (advice-add 'stop-process :around #'fakecygpty--stop-process)
  (advice-add 'continue-process :around #'fakecygpty--continue-process)
  (advice-add 'kill-process :around #'fakecygpty--kill-process)

  (advice-add 'set-process-window-size :around #'fakecygpty--set-process-window-size)
  (advice-add 'gdb-io-interrupt :around #'fakecygpty--gdb-io-interrupt-workaround))

(provide 'fakecygpty)

;;; fakecygpty.el ends here
