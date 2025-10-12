;; dante-repl.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  2 August 2019
;; Description:

(eval-when-compile
  (require 'macro-util)

  (declare-function sessions-mark-global-var-for-save "persistent-sessions-global-vars"))

(require 'current-column-fixed)
(require 'dante)
(require 'haskell-syntax-table)

(defvar-local dante-repl--command-line-to-use nil
  "Pair of command lines both of which can be used to start current
repl session, created with the help of ‘dante--mk-repl-cmdline’.

One command should be used for a session that should load all
project modules into new repl. Another command should be used to
get a “fresh” repl with no modules loaded.

This variable gets assigned by ‘dante-initialize-method’.")

(defvar-local dante-repl--last-command-line nil
  "Last command line used to start the REPL session.")

(defun dante-repl--command-line-to-use (load-all-on-start)
  (let ((dante-repl-cmdline
         (or dante-repl--command-line-to-use
             (progn (dante-initialize-method) dante-repl--command-line-to-use))))
    (cl-assert (dante--repl-cmdline-p dante-repl-cmdline))
    (if load-all-on-start
        (dante--repl-cmdline-loading-all-modules dante-repl-cmdline)
      (dante--repl-cmdline-loading-no-modules dante-repl-cmdline))))

;;;###autoload
(defun dante-repl-buffer-name ()
  (replace-regexp-in-string
   "^ *\\*dante#"
   "*dante-repl#"
   (dante-buffer-name)))

;;;###autoload
(defun dante-repl-switch-to-repl-buffer ()
  (interactive)
  (let* ((buf-name (dante-repl-buffer-name))
         (repl-buf (get-buffer buf-name)))
    (if (buffer-live-p repl-buf)
        (switch-to-buffer-other-window repl-buf)
      (progn
        (dante-repl--start-with-buffer-name buf-name nil nil)
        (switch-to-buffer-other-window (get-buffer buf-name))))))

(defconst +dante-prompt-re+ "^[\4\5] ")

(defconst dante-repl-syntax-table (copy-syntax-table haskell-mode-syntax-table))

;;;###autoload
(define-derived-mode dante-repl-mode comint-mode "Dante-REPL"
  "Interactive prompt for Dante."
  :syntax-table dante-repl-syntax-table
  (setq-local comint-prompt-regexp +dante-prompt-re+
              ;; warning-suppress-types (cons '(undo discard-info) warning-suppress-types)
              comint-prompt-read-only t)
  (add-hook 'completion-at-point-functions 'dante-repl-completion-at-point nil t)
  (company-mode +1))

(defvar dante-repl-start--command-history nil)
(sessions-mark-global-var-for-save 'dante-repl-start--command-history)

;;;###autoload
(defun dante-repl-start (&optional custom-command)
  "Start dante repl session. Provide universal argument to start with custom command line,
otherwise the command for starting repl will be inferred."
  (interactive "P")
  (let ((command-line (when custom-command
                        (split-shell-command-into-arguments
                         (read-string-no-default "Shell command to start repl: "
                                                 "cabal repl"
                                                 dante-repl-start--command-history)))))
    (dante-repl--start-with-buffer-name (dante-repl-buffer-name) command-line default-directory)))

;;;###autoload
(defun dante-repl-restart (select-new-command?)
  (interactive "P")
  (let* ((inside-repl-buffer? (eq major-mode 'dante-repl-mode))
         (buf-name (if inside-repl-buffer?
                       (buffer-name (current-buffer))
                     (dante-repl-buffer-name))))

    (when-let ((buf (get-buffer buf-name))
               (proc (get-buffer-process buf)))
      (when (and (buffer-live-p buf)
                 (process-live-p proc))
        (delete-process proc)))

    (let ((repl-buf (get-buffer-create buf-name)))
      (condition-case err
          (let ((command-line
                 (cond
                   (select-new-command?
                    (dante-repl--command-line-to-use (y-or-n-p "Load all modules on REPL start?")))
                   (t
                    (if dante-repl--last-command-line
                        dante-repl--last-command-line
                      (error "No previous REPL command available"))))))
            (dante-repl--start-in-buffer-with-command-line repl-buf command-line nil (and inside-repl-buffer? default-directory))
            (unless inside-repl-buffer?
              (switch-to-buffer-other-window repl-buf)))
        (error
         (kill-buffer repl-buf)
         (signal (car err) (cdr err)))))))

(defun dante-repl--start-with-buffer-name (buf-name command current-dir)
  (let ((repl-buf (get-buffer-create buf-name)))
    (condition-case err
        (progn
          (dante-repl--start-in-buffer repl-buf nil t command current-dir)
          (switch-to-buffer-other-window repl-buf))
      (error
       (kill-buffer repl-buf)
       (signal (car err) (cdr err))))))

(font-lock-add-keywords
 'dante-repl-mode
 '(("\\(\4\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?>))))
   ("\\(\5\\)"
    (0 (prog1 ()
         (compose-region (match-beginning 1)
                         (match-end 1)
                         ?\|))))))

(defun dante-repl--start-in-buffer (repl-buf initial-repl-command load-all-on-start command current-dir)
  (let ((command-line
         (or command
             (-non-nil (-map #'eval (dante-repl--command-line-to-use load-all-on-start))))))
    (dante-repl--start-in-buffer-with-command-line repl-buf command-line initial-repl-command current-dir)))

(defun dante-repl--start-in-buffer-with-command-line (repl-buf command-line initial-repl-command current-dir)
  (let ((dir (or current-dir (dante-project-root))))
    (with-current-buffer repl-buf
      (cd dir)
      (dante-repl-mode)
      (setq-local dante-repl--last-command-line command-line)
      (let ((proc
             (get-buffer-process
              (apply #'make-comint-in-buffer
                     "dante"
                     repl-buf
                     (car command-line)
                     nil
                     (cdr command-line)))))
        (when (process-live-p proc)
          (set-process-query-on-exit-flag proc nil)
          (let ((cmd
                 ;; ":set -fbyte-code -fdiagnostics-color=always -Wno-missing-home-modules -dsuppress-modules-prefixes"
                 ;; :set prompt \"\"
                 ;; :set -fbyte-code
                 ;; :set -fdiagnostics-color=always -Wno-missing-home-modules -dsuppress-modules-prefixes -fshow-loaded-modules
                 ;; :set -XOverloadedStrings
                 ":set prompt \"\\4 \"\n:set prompt-cont \"\\5 \""))
            (comint-simple-send proc (if initial-repl-command
                                         (concat cmd "\n" initial-repl-command)
                                       cmd)))
          (message "Started REPL with %s" (s-join " " command-line))
          t)))))

(defun dante-repl-completion-at-point ()
  (save-match-data
    (let* ((beg (save-excursion (dante-repl-beginning-of-line) (point)))
           (end (point))
           (str (buffer-substring-no-properties beg end))
           (repl-buffer (current-buffer))
           (proc (get-buffer-process (current-buffer))))
      (with-temp-buffer
        (comint-redirect-send-command-to-process
         (format ":complete repl %S" str) ;; command
         (current-buffer)                 ;; output buffer
         proc                             ;; target process
         nil                              ;; echo
         t)                               ;; no-display
        (while (not (with-current-buffer repl-buffer
                      comint-redirect-completed))
          (sleep-for 0.01))
        (let* ((completions (dante-completion-response-to-list (buffer-substring-no-properties (point-min) (point-max))))
               (first-line (car completions)))
          (when (string-match "[^ ]* [^ ]* " first-line) ;; "2 2 :load src/"
            (setq first-line (replace-match "" nil nil first-line))
            (list (+ beg (length first-line)) end (cdr completions))))))))

(defun dante-repl-beginning-of-line ()
  "Go to the beginning of current line, excluding the prompt."
  (interactive)
  (if (search-backward-regexp +dante-prompt-re+ (line-beginning-position) t 1)
      (goto-char (+ 2 (line-beginning-position)))
    (call-interactively #'move-beginning-of-line)))

(defun dante-completion-response-to-list (reply)
  "Convert the REPLY from a backend completion to a list."
  (if (string-match-p "^*** Exception" reply)
      (list)
    (--map
     (replace-regexp-in-string "\\\"" "" it)
     (split-string reply "\n" t))))

;;;###autoload
(defun dante-repl-load-file ()
  (interactive)
  (let* ((repl-buf-name (dante-repl-buffer-name))
         (repl-buf (get-buffer repl-buf-name)))
    (aif (buffer-file-name)
        ;; Buffer backed by a file.
        (dante-repl-load-file--send-load-command repl-buf-name
                                                 repl-buf
                                                 (dante-repl-get-file-to-load (current-buffer)))
      ;; Temporary buffer without file counterpart.
      (with-temporary-file tmp-file
          (shell-quote-argument (file-name-nondirectory (buffer-name)))
          "tmp"
          nil
        (let ((noninteractive t))
          (write-region (point-min) (point-max) tmp-file))
        (dante-repl-load-file--send-load-command repl-buf-name repl-buf tmp-file)))
    (switch-to-buffer-other-window (get-buffer repl-buf-name))))

(defvar dante-repl-get-file-to-load--impl #'dante-repl-get-file-to-load--default-impl)

(defun dante-repl-get-file-to-load (buf)
  (funcall dante-repl-get-file-to-load--impl buf))

(defun dante-repl-get-file-to-load--default-impl (buf)
  (buffer-file-name buf))

(defvar-local dante-repl--file-name-to-load-instead nil)

(defun dante-repl-get-component-build-dir (buf)
  (let ((method (buffer-local-value 'dante--selected-method buf)))
    ;; Must be already initialized.
    (cl-assert method)
    (dante-get-component-build-dir
     buf
     (when-let ((f (dante-method-get-repl-build-dir method)))
       (funcall f (eproj-get-project-for-buf-lax buf))))))

(defun dante-repl-get-file-to-load--hsc2hs-impl (buf)
  (with-current-buffer buf
    (or dante-repl--file-name-to-load-instead
        (setq-local dante-repl--file-name-to-load-instead
                    (concat
                     (dante-repl-get-component-build-dir buf)
                     "/"
                     (replace-regexp-in-string "[.]"
                                               "/"
                                               (treesit-haskell-get-buffer-module-name))
                     ".hs")))))

(defun dante-repl-load-file--send-load-command (repl-buf-name repl-buf file-to-load)
  (let ((cmd (concat ":load \"*" file-to-load "\"")))
    (if (and repl-buf
             (buffer-live-p repl-buf))
        (let ((proc (get-buffer-process repl-buf)))
          (if (process-live-p proc)
              (comint-simple-send proc cmd)
            (dante-repl--start-in-buffer repl-buf cmd nil nil nil)))
      (let ((buf (get-buffer-create repl-buf-name)))
        (dante-repl--start-in-buffer buf cmd nil nil nil)))))

(defun dante-repl-get-last-output ()
  "Return last output as string."
  (comint-get-last-output))

(defun dante-repl-clear-buffer-above-prompt ()
  (interactive)
  (let ((col (current-column-fixed)))
    (forward-line -1)
    (end-of-line)
    (forward-char 1)
    (with-inhibited-read-only
     (set-text-properties (point-min) (point) nil)
     (remove-overlays (point-min) (line-end-position))
     (delete-region (point-min) (line-end-position)))
    ;; Skip prompt.
    (end-of-line)
    (move-to-column col)))

(defun dante-repl-newline ()
  "Insert newlie at point like ‘newline-and-indent’ would do.
Ignores read-only property of underlying next."
  (interactive)
  (with-inhibited-read-only
   (newline-and-indent)))

(provide 'dante-repl)

;; Local Variables:
;; End:

;; dante-repl.el ends here
