;; tramp-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  2 July 2026
;; Description:

(eval-when-compile
  (require 'el-patch)
  (require 'tramp)
  (require 'tramp-cache))

(autoload 'tramp-compile-disable-ssh-controlmaster-options "tramp-integration")
(autoload 'tramp-file-name-method "tramp")
(autoload 'tramp-get-connection-process "tramp")
(autoload 'tramp-get-connection-property "tramp-cache")
(autoload 'tramp-maybe-open-connection "tramp-sh")
(autoload 'tramp-message "tramp-message")
(autoload 'tramp-send-string "tramp")
(autoload 'tramp-set-connection-property "tramp-cache")
(autoload 'tramp-setup-debug-buffer "tramp-message")
(autoload 'tramp-wait-for-output "tramp-sh")

(autoload 'tramp-connection-property-p "tramp-cache")

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
      (unless nooutput (tramp-wait-for-output p)))))

(with-eval-after-load "tramp-message"
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
