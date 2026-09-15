;; tramp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  2 July 2026
;; Description:

(eval-when-compile
  (require 'el-patch)
  (require 'tramp)
  (require 'tramp-cache)
  (require 'tramp-sh))

(autoload 'tramp-barf-if-no-shell-prompt "tramp-sh")
(autoload 'tramp-barf-unless-okay "tramp-sh")
(autoload 'tramp-check-for-regexp "tramp")
(autoload 'tramp-compile-disable-ssh-controlmaster-options "tramp-integration")
(autoload 'tramp-file-name-method "tramp")
(autoload 'tramp-get-connection-process "tramp")
(autoload 'tramp-get-sh-extra-args "tramp-sh")
(autoload 'tramp-inside-emacs "trampver")
(autoload 'tramp-maybe-open-connection "tramp-sh")
(autoload 'tramp-progress-reporter-update "tramp")
(autoload 'tramp-send-command "tramp-sh")
(autoload 'tramp-send-string "tramp")
(autoload 'tramp-setup-debug-buffer "tramp-message")
(autoload 'tramp-shell-quote-argument "tramp")
(autoload 'tramp-wait-for-output "tramp-sh")

(require 'tramp-loaddefs)

(defvar magit-tramp-pipe-stty-settings)
(defvar tramp-compat-temporary-file-directory)
(defvar tramp-connection-timeout)
(defvar tramp-copy-size-limit)
(defvar tramp-debug-font-lock-keywords)
(defvar tramp-echo-mark)
(defvar tramp-histfile-override)
(defvar tramp-methods)
(defvar tramp-remote-path)
(defvar tramp-remote-process-environment)
(defvar tramp-use-scp-direct-remote-copying)
(defvar tramp-verbose)

(require 'common)
(require 'el-patch)

(require 'shell-baseline-emacs-fixes)

;;;###autoload
(add-to-list 'el-patch-features 'tramp-sh)

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      tramp-copy-size-limit (* 2 1024 1024)
      tramp-verbose 2
      tramp-connection-timeout 5
      tramp-histfile-override nil
      ;; tramp-default-remote-shell "/bin/sh"
      )

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

;; Not portable if /bin/sh is not bash
;; (add-to-list 'tramp-sh-extra-args
;;              '("/bin/sh" . "--noprofile --norc --posix"))

;; (add-to-list 'tramp-sh-extra-args
;;              '("/usr/bin/env bash" . "-noediting -norc -noprofile"))
;;
;; (add-to-list 'tramp-connection-properties
;;                (list "/ssh:.*"
;;                      ;; (regexp-quote "/sshx:user@host:")
;;                      "tramp-remote-shell"
;;                      "/usr/bin/env bash"))

;; (defun tramp-cache-init ()
;;   (add-to-list 'tramp-connection-properties
;;                (list "/ssh:sergey@"
;;                      ;; (regexp-quote "/sshx:user@host:")
;;                      "tramp-remote-shell"
;;                      "/etc/profiles/per-user/sergey/bin/bash")))

;; (setf tramp-remote-path (cdr tramp-remote-path))

;; (setf old-ssh-assoc (assoc "ssh" tramp-methods))


(defun tramp-init ()
  ;; (add-to-list 'tramp-remote-path
  ;;              "/etc/profiles/per-user/sergey/bin")

  ;; (setf (cdr (assoc "sshx" tramp-methods)))

  ;; (setf (cdr (assoc "ssh" tramp-methods))
  ;;       (cdr (assoc "sshx" tramp-methods)))

  ;; Make sure user’s PATH will be available. Otherwise only default
  ;; PATH obtained from ‘getconf PATH’ will be set, which is usually
  ;; too restrictive, e.g. it doesn’t include ‘nix’ on typical macos
  ;; install.
  (add-to-list 'tramp-remote-path
               'tramp-own-remote-path)

  (add-to-list 'tramp-remote-process-environment
               "HISTCONTROL=ignorespace:ignoredups:erasedups")

  ;; Don’t pass -l, it’s either redundant on Linux (we’ll ultimately
  ;; read ~/.bashrc which is typically the source of truth we care
  ;; about) or is actively harmful on MacOS (-l will make us read
  ;; /etc/profile which will leave us with a messed up PATH thanks to
  ;; using `/usr/libexec/path_helper`).
  (setf (cdr (assq 'tramp-remote-shell-login (cdr (assoc "ssh" tramp-methods))))
        (remq nil (mapcar (lambda (xs) (remove "-l" xs)) (cdr (assq 'tramp-remote-shell-login (cdr (assoc "ssh" tramp-methods)))))))

  (when (member-recursive "-l" (assq 'tramp-remote-shell-login (cdr (assoc "ssh" tramp-methods))))
    (error "Tramp ssh method is not initialized correctly"))

  ;; Alternative way of resetting tramp-remote-shell-login.
  ;; (add-to-list 'tramp-connection-properties
  ;;              (list "/ssh:.*"
  ;;                    ;; (regexp-quote "/sshx:user@host:")
  ;;                    "tramp-remote-shell-login"
  ;;                    nil))

  ;; (setf (cdr (assoc "ssh" tramp-methods))
  ;;       `((tramp-login-program        "ssh")
  ;;         (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
  ;;                                      ("-e" "none") ("-t" "-t")
  ;;                                      ;; ("-v")
  ;;                                      ;; ("-o" "RemoteCommand=\"/usr/bin/env bash\"")
  ;;                                      ("%h")))
  ;;         (tramp-async-args           (("-q")))
  ;;         (tramp-remote-shell         ,tramp-default-remote-shell)
  ;;         (tramp-remote-shell-login   ("-l"))
  ;;         (tramp-remote-shell-args    ( "-c"))))

  ;; (add-to-list 'tramp-methods
  ;;              `("sshx"
  ;;                (tramp-login-program        "ssh")
  ;;                (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
  ;;                                             ("-e" "none") ("-t" "-t")
  ;;                                             ("-o" "RemoteCommand=\"/usr/bin/env bash\"")
  ;;                                             ("%h")))
  ;;                (tramp-async-args           (("-q")))
  ;;                (tramp-remote-shell         ,tramp-default-remote-shell)
  ;;                (tramp-remote-shell-login   ("-l"))
  ;;                (tramp-remote-shell-args    ("-c"))))
  )

;; (with-eval-after-load 'tramp-cache (tramp-cache-init))
(with-eval-after-load 'tramp (tramp-init))

(with-eval-after-load 'magit-process
  (setq magit-tramp-pipe-stty-settings 'pty))

;; (defun emacs-shell (buffer-name directory history-file)
;;   (let* ((default-directory directory)
;;          (shell-buffer (shell (generate-new-buffer-name buffer-name))))
;;     ;; (setq-local comint-input-ring-size emacs-shell-input-ring-size)
;;     ;; (setq-local comint-input-ring (make-ring comint-input-ring-size))
;;     ;; (setq-local comint-input-ring-file-name (concat emacs-shell-history-directory "/" history-file))
;;     ;; (emacs-shell-fill-input-ring comint-input-ring comint-input-ring-file-name)
;;     (set-process-sentinel (get-buffer-process shell-buffer)
;;                           'shell-process-kill-buffer-sentinel)))
;;
;; ;; (defun my-shell ()
;; ;;   (interactive)
;; ;;   (let ((default-directory "/ssh:user@host:"))
;; ;;     (shell)))
;; (defun tramp-shell (method host &optional history-name directory user)
;;   "Start an interactive shell on HOST using METHOD."
;;   (interactive "sMethod: \nsHost: ")
;;
;;   (let ((default-directory (format "/%s:%s:" method host)))
;;     (shell))
;;
;;   ;; (emacs-shell (concat method "-" host)
;;   ;;              (format "/%s:%s:%s" method (if user (format "%s@%s" user host) host) (or directory ""))
;;   ;;              (concat (or history-name host) "." method))
;;   ;; (hack-connection-local-variables-apply
;;   ;;  `(:application
;;   ;;    'emacs-shell
;;   ;;    :protocol ,method
;;   ;;    :host ,host
;;   ;;    :remote-shell "/etc/profiles/per-user/sergey/bin/bash"))
;;   )
;;
;; (defun ssh-shell (host)
;;   "Start ssh shell."
;;   (interactive (list (read-string "Host: ")))
;;   (let ((default-directory (format "/ssh:%s:" host)))
;;     (shell (get-buffer-create)))
;;   ;; (tramp-shell "ssh" host nil directory)
;;   )

(with-eval-after-load "tramp-sh"
  (el-patch-defun tramp-send-command (vec command &optional neveropen nooutput)
    "Send the COMMAND to connection VEC.
Erases temporary buffer before sending the command.  If optional
arg NEVEROPEN is non-nil, never try to open the connection.  This
is meant to be used from `tramp-maybe-open-connection' only.  The
function waits for output unless NOOUTPUT is set."
    (unless neveropen (tramp-maybe-open-connection vec))
    (let ((p (tramp-get-connection-process vec)))
      (when (tramp-get-connection-property p "remote-echo")
        ;; We mark the command string that it can be erased in the output buffer.
        (tramp-set-connection-property p "check-remote-echo" t)
        ;; If we put `tramp-echo-mark' after a trailing newline (which
        ;; is assumed to be unquoted) `tramp-send-string' doesn't see
        ;; that newline and adds `tramp-rsh-end-of-line' right after
        ;; `tramp-echo-mark', so the remote shell sees two consecutive
        ;; trailing line endings and sends two prompts after executing
        ;; the command, which confuses `tramp-wait-for-output'.
        (when (el-patch-swap
                (and (not (string-empty-p command))
                     (string-equal (substring command -1) "\n"))
                (string-suffix-p "\n" command))
          (setq command (substring command 0 -1)))
        ;; No need to restore a trailing newline here since `tramp-send-string'
        ;; makes sure that the string ends in `tramp-rsh-end-of-line', anyway.
        (setq command (format "%s%s%s" tramp-echo-mark command tramp-echo-mark)))
      (el-patch-add
        (when (string= "ssh" (tramp-file-name-method vec))
          ;; Suppress ssh history.
          (setf command (concat " " command))))
      ;; Send the command.
      (tramp-message vec 6 "%s" command)
      (tramp-send-string vec command)
      (unless nooutput (tramp-wait-for-output p))))

  (el-patch-defun tramp-open-shell (vec shell)
    "Open shell SHELL."
    ;; Find arguments for this shell.
    (with-tramp-progress-reporter
        vec 5 (format-message "Opening remote shell `%s'" shell)
      ;; It is useful to set the prompt in the following command because
      ;; some people have a setting for $PS1 which /bin/sh doesn't know
      ;; about and thus /bin/sh will display a strange prompt.  For
      ;; example, if $PS1 has "${CWD}" in the value, then ksh will
      ;; display the current working directory but /bin/sh will display
      ;; a dollar sign.  The following command line sets $PS1 to a sane
      ;; value, and works under Bourne-ish shells as well as csh-like
      ;; shells.  We also unset the variable $ENV because that is read
      ;; by some sh implementations (eg, bash when called as sh) on
      ;; startup; this way, we avoid the startup file clobbering $PS1.
      ;; $PROMPT_COMMAND is another way to set the prompt in /bin/bash,
      ;; it must be discarded as well.  Some ssh daemons (for example,
      ;; on Android devices) do not acknowledge the $PS1 setting in
      ;; that call, so we make a further sanity check.  (Bug#57044)
      ;; $HISTFILE is set according to `tramp-histfile-override'.  $TERM
      ;; and $INSIDE_EMACS set here to ensure they have the correct
      ;; values when the shell starts, not just processes run within the
      ;; shell.  (Which processes include our initial probes to ensure
      ;; the remote shell is usable.)  For the time being, we assume
      ;; that all shells interpret -i as interactive shell.  Must be the
      ;; last argument, because (for example) bash expects long options
      ;; first.
      (tramp-send-command
       vec (format
	          (concat
	           "exec env TERM='%s' INSIDE_EMACS='%s' "
	           "ENV=%s %s PROMPT_COMMAND='' PS1=%s PS2='' PS3='' %s %s -i")
            tramp-terminal-type (tramp-inside-emacs)
            (or (getenv-internal "ENV" tramp-remote-process-environment) "")
	          (if (stringp tramp-histfile-override)
	              (format "HISTFILE=%s"
		                    (tramp-shell-quote-argument tramp-histfile-override))
	            (if tramp-histfile-override
		              "HISTFILE='' HISTFILESIZE=0 HISTSIZE=0"
	              (el-patch-swap ""
                               "HISTCONTROL=ignorespace:ignoredups:erasedups")))
	          (tramp-shell-quote-argument tramp-end-of-output)
	          shell (or (tramp-get-sh-extra-args shell) ""))
       t t)

      ;; Sanity check.
      (tramp-barf-if-no-shell-prompt
       (tramp-get-connection-process vec) 60
       "Couldn't find remote shell prompt for %s" shell)
      (unless
	        (tramp-check-for-regexp
	         (tramp-get-connection-process vec) (rx (literal tramp-end-of-output)))
        (tramp-wait-for-output (tramp-get-connection-process vec))
        (tramp-message vec 5 "Setting shell prompt")
        (tramp-send-command
         vec (format "PS1=%s PS2='' PS3='' PROMPT_COMMAND=''"
		                 (tramp-shell-quote-argument tramp-end-of-output))
         t t)
        (tramp-barf-if-no-shell-prompt
         (tramp-get-connection-process vec) 60
         "Couldn't find remote shell prompt for %s" shell))
      (tramp-wait-for-output (tramp-get-connection-process vec))

      ;; Check proper HISTFILE setting.  We give up when not working.
      (when (and (stringp tramp-histfile-override)
	               (file-name-directory tramp-histfile-override))
        (tramp-barf-unless-okay
         vec
         (format
	        "(cd %s)"
	        (tramp-shell-quote-argument
	         (file-name-directory tramp-histfile-override)))
         "`tramp-histfile-override' uses invalid file `%s'"
         tramp-histfile-override))

      (tramp-flush-connection-property
       (tramp-get-connection-process vec) "scripts")
      (tramp-set-connection-property
       (tramp-get-connection-process vec) "remote-shell" shell)))

  (el-patch-defun tramp-maybe-open-connection (vec)
    "Maybe open a connection VEC.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
    ;; During completion, don't reopen a new connection.
    ;; Same for slide-in timer or process-{filter,sentinel}.
    (unless (tramp-connectable-p vec)
      (throw 'non-essential 'non-essential))

    (with-tramp-debug-message vec "Opening connection"
      (let ((p (tramp-get-connection-process vec))
	          (process-name (tramp-get-connection-property vec " process-name"))
	          (process-environment
             (el-patch-wrap 2 0
               ;; Shell environment for e.g. ssh running on host.
               (cons "HISTCONTROL=ignorespace:ignoredups:erasedups"
                     (copy-sequence process-environment))))
	          (pos (with-current-buffer (tramp-get-connection-buffer vec) (point))))

        ;; If Tramp opens the same connection within a short time frame,
        ;; there is a problem.  We shall signal this.
        (unless (or (process-live-p p)
                    (and (processp p) (not non-essential))
		                (not (tramp-file-name-equal-p
			                    vec (car tramp-current-connection)))
		                (time-less-p
		                 (time-since (cdr tramp-current-connection))
		                 (or tramp-connection-min-time-diff 0)))
	        (throw 'suppress 'suppress))

        ;; If too much time has passed since last command was sent, look
        ;; whether process is still alive.  If it isn't, kill it.  When
        ;; using ssh, it can sometimes happen that the remote end has
        ;; hung up but the local ssh client doesn't recognize this until
        ;; it tries to send some data to the remote end.  So that's why
        ;; we try to send a command from time to time, then look again
        ;; whether the process is really alive.
        (condition-case nil
	          (when (and (time-less-p
		                    60 (time-since
			                      (tramp-get-connection-property p "last-cmd-time" 0)))
		                   (process-live-p p))
	            (tramp-send-command vec "echo are you awake" t t)
	            (unless (and (process-live-p p)
			                     (tramp-wait-for-output p 10))
	              ;; The error will be caught locally.
	              (tramp-error vec 'remote-file-error "Awake did fail")))
	        (remote-file-error
	         (tramp-cleanup-connection vec t)
	         (setq p nil)))

        ;; New connection must be opened.
        (condition-case err
	          (unless (process-live-p p)
	            (catch 'uname-changed
	              ;; Start new process.
	              (when (and p (processp p))
		              (delete-process p))
	              (setenv "LC_ALL" (tramp-get-local-locale vec))
	              (if (stringp tramp-histfile-override)
		                (setenv "HISTFILE" tramp-histfile-override)
		              (if tramp-histfile-override
		                  (progn
		                    (setenv "HISTFILE")
		                    (setenv "HISTFILESIZE" "0")
		                    (setenv "HISTSIZE" "0"))))
	              (unless (stringp tramp-encoding-shell)
                  (tramp-error
		               vec 'remote-file-error "`tramp-encoding-shell' not set"))
	              (let* ((current-host tramp-system-name)
		                   (target-alist (tramp-compute-multi-hops vec))
		                   (previous-hop tramp-null-hop)
		                   ;; We will apply `tramp-ssh-or-plink-options'
		                   ;; only for the first hop.
		                   (options (tramp-ssh-or-plink-options vec))
		                   (process-connection-type tramp-process-connection-type)
		                   (process-adaptive-read-buffering nil)
		                   ;; There are unfortunate settings for "cmdproxy"
		                   ;; on W32 systems.
		                   (process-coding-system-alist nil)
		                   (coding-system-for-read nil)
		                   (extra-args (tramp-get-sh-extra-args tramp-encoding-shell))
		                   ;; This must be done in order to avoid our file
		                   ;; name handler.
		                   (p (apply
			                     #'tramp-start-process vec
			                     (tramp-get-connection-name vec)
			                     (tramp-get-connection-buffer vec)
			                     (append
			                      `(,tramp-encoding-shell)
			                      (and extra-args (split-string extra-args))
			                      (and tramp-encoding-command-interactive
			                           `(,tramp-encoding-command-interactive))))))

		              ;; Set sentinel.  Initialize variables.
		              (set-process-sentinel p #'tramp-process-sentinel)
		              (setq tramp-current-connection (cons vec (current-time)))

		              ;; Set connection-local variables.
		              (tramp-set-connection-local-variables vec)

		              ;; Check whether process is alive.
		              (tramp-barf-if-no-shell-prompt
		               p 10
		               "Couldn't find local shell prompt for %s"
		               tramp-encoding-shell)

		              ;; Now do all the connections as specified.
		              (while target-alist
		                (let* ((hop (car target-alist))
			                     (l-method (tramp-file-name-method hop))
			                     (l-user (tramp-file-name-user hop))
			                     (l-domain (tramp-file-name-domain hop))
			                     (l-host (tramp-file-name-host hop))
			                     (l-port (tramp-file-name-port hop))
			                     (remote-shell
			                      (tramp-get-method-parameter hop 'tramp-remote-shell))
			                     (extra-args (tramp-get-sh-extra-args remote-shell))
			                     (async-args
			                      (flatten-tree
			                       (tramp-get-method-parameter hop 'tramp-async-args)))
			                     (connection-timeout
			                      (tramp-get-method-parameter
			                       hop 'tramp-connection-timeout
			                       tramp-connection-timeout))
			                     (command (tramp-expand-args hop 'tramp-login-program))
			                     ;; We don't create the temporary file.  In
			                     ;; fact, it is just a prefix for the
			                     ;; ControlPath option of ssh; the real
			                     ;; temporary file has another name, and it is
			                     ;; created and protected by ssh.  It is also
			                     ;; removed by ssh when the connection is
			                     ;; closed.  The temporary file name is cached
			                     ;; in the main connection process, therefore
			                     ;; we cannot use
			                     ;; `tramp-get-connection-process'.
			                     (tmpfile
			                      (with-tramp-connection-property
			                          (tramp-get-process vec) "temp-file"
			                        (tramp-compat-make-temp-name)))
			                     r-shell)

		                  ;; Check, whether there is a restricted shell.
		                  (dolist (elt tramp-restricted-shell-hosts-alist)
		                    (when (string-match-p elt current-host)
			                    (setq r-shell t)))
		                  (setq current-host l-host)

		                  ;; Set hop and password prompt vector.
		                  (tramp-set-connection-property p "hop-vector" hop)
		                  (tramp-set-connection-property
		                   p "pw-vector"
		                   (if (tramp-get-method-parameter
			                      hop 'tramp-password-previous-hop)
			                     (let ((pv (copy-tramp-file-name previous-hop)))
			                       (setf (tramp-file-name-method pv) l-method)
			                       pv)
		                     (make-tramp-file-name
			                    :method l-method :user l-user :domain l-domain
			                    :host l-host :port l-port)))

		                  ;; Set session timeout.
		                  (when-let* ((timeout
				                           (tramp-get-method-parameter
				                            hop 'tramp-session-timeout)))
		                    (tramp-set-connection-property
		                     p "session-timeout" timeout))

		                  ;; Replace `login-args' place holders.
		                  (setq
		                   command
		                   (string-join
		                    (append
		                     ;; We do not want to see the trailing local
		                     ;; prompt in `start-file-process'.
		                     (unless r-shell '("exec"))
		                     `(,command)
		                     ;; Add arguments for asynchronous processes.
		                     (when process-name async-args)
		                     (tramp-expand-args
			                    hop 'tramp-login-args nil
			                    ?h (or l-host "") ?u (or l-user "") ?p (or l-port "")
			                    ?c (format-spec options (format-spec-make ?t tmpfile))
			                    ?n (concat
			                        "2>" (tramp-get-remote-null-device previous-hop))
                          ;; This might be problematic.  We check only for
                          ;; the first hop.  OTOH, checking ssh
                          ;; options for every hop might be to expensive.
                          ?w (tramp-ssh-setenv-term vec)
			                    ?l (concat remote-shell " " extra-args " -i"))
		                     ;; A restricted shell does not allow "exec".
		                     (when r-shell '("&&" "exit")) '("||" "exit"))
		                    " "))

		                  ;; Send the command.
		                  (with-tramp-progress-reporter
			                    vec 3
			                    (format "Opening connection%s for %s%s using %s"
				                          (if (tramp-string-empty-or-nil-p process-name)
				                              "" (concat " " process-name))
				                          (if (tramp-string-empty-or-nil-p l-user)
				                              "" (concat l-user "@"))
				                          (tramp-file-name-host-port hop) l-method)
		                    (tramp-send-command vec command t t)
		                    (tramp-process-actions
		                     p vec
		                     (min
			                    pos (with-current-buffer (process-buffer p) (point-max)))
		                     tramp-actions-before-shell connection-timeout))

		                  ;; Next hop.
		                  (tramp-flush-connection-property p "hop-vector")
		                  (tramp-flush-connection-property p "pw-vector")
		                  (setq options ""
			                      target-alist (cdr target-alist)
			                      previous-hop hop)))

		              ;; Activate session timeout.
		              (when (tramp-get-connection-property p "session-timeout")
		                (run-at-time
		                 (tramp-get-connection-property p "session-timeout") nil
		                 #'tramp-timeout-session vec))

		              ;; Make initial shell settings.
		              (with-tramp-progress-reporter
		                  vec 3
		                  (format "Setup connection%s for %s%s using %s"
			                        (if (tramp-string-empty-or-nil-p process-name)
				                          "" (concat " " process-name))
			                        (if (tramp-string-empty-or-nil-p
				                           (tramp-file-name-user vec))
				                          "" (concat (tramp-file-name-user vec) "@"))
			                        (tramp-file-name-host-port vec)
			                        (tramp-file-name-method vec))
		                (tramp-open-connection-setup-interactive-shell p vec))

		              ;; Mark it as connected.
		              (tramp-set-connection-property p "connected" t))))

	        ;; Cleanup, and propagate the signal.
	        ((error quit)
	         (tramp-cleanup-connection vec t)
	         (signal (car err) (cdr err))))))))

(with-eval-after-load "tramp-message"
  (when-emacs-version (= 30 it)
    (el-patch-defun tramp-setup-debug-buffer ()
      "Function to setup debug buffers."
      (el-patch-remove (declare (tramp-suppress-trace t)))
      ;; (declare (completion tramp-debug-buffer-command-completion-p)
      ;;          (tramp-suppress-trace t))
      (interactive)
      (set-buffer-file-coding-system 'utf-8)
      (setq buffer-undo-list t)
      ;; Activate `outline-mode'.  This runs `text-mode-hook' and
      ;; `outline-mode-hook'.  We must prevent that local processes die.
      ;; Yes: I've seen `flyspell-mode', which starts "ispell".
      ;; `(custom-declare-variable outline-minor-mode-prefix ...)'  raises
      ;; on error in `(outline-mode)', we don't want to see it in the
      ;; traces.
      (let ((default-directory tramp-compat-temporary-file-directory))
        (el-patch-swap
          (outline-mode)
          (text-mode)))
      (setq-local outline-level 'tramp-debug-outline-level)
      (setq-local font-lock-keywords
                  ;; FIXME: This `(t FOO . BAR)' representation in
                  ;; `font-lock-keywords' is supposed to be an internal
                  ;; implementation "detail".  Don't abuse it here!
                  `(t (eval ,tramp-debug-font-lock-keywords t)
                      ,(eval tramp-debug-font-lock-keywords t)))
      ;; I am deciding what buffers we can edit here.
      (el-patch-remove
        ;; Do not edit the debug buffer.
        (use-local-map special-mode-map))
      (set-buffer-modified-p nil)
      ;; For debugging purposes.
      (local-set-key "\M-n" 'clone-buffer)
      (add-hook 'clone-buffer-hook #'tramp-setup-debug-buffer nil 'local)))

  (when-emacs-version (<= 31 it)
    (el-patch-defun tramp-setup-debug-buffer ()
      "Function to setup debug buffers."
      (declare (completion tramp-debug-buffer-command-completion-p)
               (tramp-suppress-trace t))
      (interactive)
      (set-buffer-file-coding-system 'utf-8)
      (setq buffer-undo-list t)
      ;; Activate `outline-mode'.  This runs `text-mode-hook' and
      ;; `outline-mode-hook'.  We must prevent that local processes die.
      ;; Yes: I've seen `flyspell-mode', which starts "ispell".
      ;; `(custom-declare-variable outline-minor-mode-prefix ...)'  raises
      ;; on error in `(outline-mode)', we don't want to see it in the
      ;; traces.
      (let ((default-directory tramp-compat-temporary-file-directory))
        (el-patch-swap
          (outline-mode)
          (text-mode)))
      (setq-local outline-level 'tramp-debug-outline-level)
      (setq-local font-lock-keywords
                  ;; FIXME: This `(t FOO . BAR)' representation in
                  ;; `font-lock-keywords' is supposed to be an internal
                  ;; implementation "detail".  Don't abuse it here!
                  `(t (eval ,tramp-debug-font-lock-keywords t)
                      ,(eval tramp-debug-font-lock-keywords t)))
      ;; Do not edit the debug buffer.
      (el-patch-remove
        (use-local-map special-mode-map))
      (set-buffer-modified-p nil)
      ;; For debugging purposes.
                                        ;(add-hook 'kill-buffer-hook #'debug nil 'local)
      (local-set-key "\M-n" 'clone-buffer)
      (add-hook 'clone-buffer-hook #'tramp-setup-debug-buffer nil 'local))))

(defun tramp-utils--is-tramp-remote-file? (filename)
  (and (file-remote-p filename)
       (eq 'tramp-file-name-handler
           (find-file-name-handler filename #'file-directory-p))))

(defun tramp-utils--is-tramp-connection-alive? (filename)
  (tramp-get-connection-process (tramp-dissect-file-name filename)))

(el-patch-defun tramp-get-method-parameter (vec param &optional default)
  "Return the method parameter PARAM.
If VEC is a vector, check first in connection properties.
Afterwards, check in `tramp-methods'.  If the `tramp-methods'
entry does not exist, return DEFAULT."
  (let ((hash-entry
         (el-patch-swap
           (replace-regexp-in-string (rx bos "tramp-") "" (symbol-name param))
           (strip-string-prefix "tramp-" (symbol-name param)))))
    (if (tramp-connection-property-p vec hash-entry)
        ;; We use the cached property.
        (tramp-get-connection-property vec hash-entry)
      ;; Use the static value from `tramp-methods'.
      (if-let* ((methods-entry
                 (assoc
                  param (assoc (tramp-file-name-method vec) tramp-methods))))
          (cadr methods-entry)
        ;; Return the default value.
        default))))

(provide 'tramp-setup)

;; Local Variables:
;; End:

;; tramp-setup.el ends here
