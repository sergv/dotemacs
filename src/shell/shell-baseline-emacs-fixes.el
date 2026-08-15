;; shell-baseline-emacs-fixes.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 15 August 2026
;; Description:

(eval-when-compile
  (require 'el-patch)
  (require 'shell))

(defvar base-emacs-fixes--shell-exe nil
  "String - executable to start remote shell with that overrides default Emacs mechanism.")
(defvar base-emacs-fixes--shell-args nil
  "List of strings - arguments for remote shell with that override default Emacs mechanism.")

;;;###autoload
(add-to-list 'el-patch-features 'shell)

(with-eval-after-load "shell"
  (when-emacs-version (<= 30 it)
    (el-patch-defun shell (&optional buffer file-name)
      "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
Non-interactively, it can also be specified via the FILE-NAME arg.

If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

\\<shell-mode-map>To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

Make the shell buffer the current buffer, and return it.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
      (interactive
       (let* ((buffer
               (and current-prefix-arg
                    (read-buffer "Shell buffer: "
                                 ;; If the current buffer is an inactive
                                 ;; shell buffer, use it as the default.
                                 (if (and (eq major-mode 'shell-mode)
                                          (null (get-buffer-process
                                                 (current-buffer))))
                                     (buffer-name)
                                   (generate-new-buffer-name "*shell*")))))
              (buf (if (or buffer (not (derived-mode-p 'shell-mode))
                           (comint-check-proc (current-buffer)))
                       (get-buffer-create (or buffer "*shell*"))
                     ;; If the current buffer is a dead shell buffer, use it.
                     (current-buffer))))

         (with-current-buffer buf
           (when (and buffer (file-remote-p default-directory))
             ;; It must be possible to declare a local default-directory.
             (setq default-directory
                   (expand-file-name
                    (read-directory-name
                     "Default directory: " default-directory default-directory
                     t nil))))
           (list
            buffer
            ;; On remote hosts, the local `shell-file-name' might be useless.
            (with-connection-local-variables
             (when (and (file-remote-p default-directory)
                        (null explicit-shell-file-name)
                        (null (getenv "ESHELL")))
               ;; `expand-file-name' shall not add the MS Windows volume letter
               ;; (Bug#49229).
               (replace-regexp-in-string
                "^[[:alpha:]]:" ""
                (file-local-name
                 (expand-file-name
                  (read-file-name "Remote shell path: " default-directory
                                  shell-file-name t shell-file-name
                                  #'file-remote-p))))))))))

      (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                           (comint-check-proc (current-buffer)))
                       (get-buffer-create (or buffer "*shell*"))
                     ;; If the current buffer is a dead shell buffer, use it.
                     (current-buffer)))
      ;; The buffer's window must be correctly set when we call comint
      ;; (so that comint sets the COLUMNS env var properly).
      (with-suppressed-warnings ((obsolete display-comint-buffer-action))
        (pop-to-buffer buffer display-comint-buffer-action))

      (with-connection-local-variables
       (when file-name
         (setq-local explicit-shell-file-name file-name))

       ;; Rain or shine, BUFFER must be current by now.
       (unless (comint-check-proc buffer)
         (let* ((prog (or (el-patch-add base-emacs-fixes--shell-exe)
                          explicit-shell-file-name
                          (getenv "ESHELL") shell-file-name))
                (name (file-name-nondirectory prog))
                (startfile (concat "~/.emacs_" name))
                (xargs-name (el-patch-wrap 3 0
                              ;; Don’t attempt to resolve explicit args if
                              ;; we supplied explicit args via this new mechanism.
                              (if base-emacs-fixes--shell-args
                                  nil
                                (intern-soft (concat "explicit-" name "-args")))))
                (start-point (point)))
           (unless (file-exists-p startfile)
             (setq startfile (locate-user-emacs-file
                              (concat "init_" name ".sh"))))
           (setq-local shell--start-prog (file-name-nondirectory prog))
           (apply #'make-comint-in-buffer "shell" buffer prog
                  nil
                  (el-patch-wrap 3 0
                    (if base-emacs-fixes--shell-args
                        base-emacs-fixes--shell-args
                      (if (and xargs-name (boundp xargs-name))
                          (symbol-value xargs-name)
                        '("-i")))))
           (shell-mode)
           (when (file-exists-p startfile)
             ;; Wait until the prompt has appeared.
             (while (= start-point (point))
               (sleep-for 0.1))
             (shell-eval-command
              (with-temp-buffer
                (insert-file-contents startfile)
                (buffer-string)))))))
      (when shell-kill-buffer-on-exit
        (let* ((buffer (current-buffer))
               (process (get-buffer-process buffer))
               (sentinel (process-sentinel process)))
          (set-process-sentinel
           process
           (lambda (proc event)
             (when sentinel
               (funcall sentinel proc event))
             (unless (buffer-live-p proc)
               (kill-buffer buffer))))))
      buffer)))

(provide 'shell-baseline-emacs-fixes)

;; Local Variables:
;; End:

;; shell-baseline-emacs-fixes.el ends here
