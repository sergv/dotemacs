;; dante-repl.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created:  2 August 2019
;; Description:

(require 'dante)

(defvar-local dante-repl--command-line-to-use nil
  "Command line used to start current repl session.")

(defun dante-repl--command-line-to-use ()
  (or dante-repl--command-line-to-use
      (progn (dante-initialize-method) dante-repl--command-line-to-use)))

;;;###autoload
(defun dante-repl-buffer-name ()
  (replace-regexp-in-string
   "^ *\\*dante#"
   "*dante-repl#"
   (dante-buffer-name)))

;;;###autoload
(defun dante-repl-switch-to-repl-buffer ()
  (interactive)
  (unless dante-mode
    (error "dante is not enabled"))
  (let ((repl-buf (get-buffer (dante-repl-buffer-name))))
    (if (buffer-live-p repl-buf)
        (switch-to-buffer-other-window repl-buf))
    (awhen (dante-buffer-p)
      (dante-repl-start)
      (switch-to-buffer-other-window it))))

(defconst +dante-prompt-re+ "^\4 ")

;;;###autoload
(define-derived-mode dante-repl-mode comint-mode "Dante-REPL"
  "Interactive prompt for Dante."
  (setq-local comint-prompt-regexp +dante-prompt-re+
              ;; warning-suppress-types (cons '(undo discard-info) warning-suppress-types)
              comint-prompt-read-only t)
  (add-hook 'completion-at-point-functions 'dante-repl-completion-at-point nil t)
  (company-mode +1))

;;;###autoload
(defun dante-repl-start ()
  (interactive)
  (dante-repl--start-with-buffer-name (dante-repl-buffer-name)))

;;;###autoload
(defun dante-repl-restart ()
  (interactive)
  (let* ((inside-repl-buffer? (eq major-mode 'dante-repl-mode))
         (buf-name (if inside-repl-buffer?
                       (buffer-name (current-buffer))
                     (dante-repl-buffer-name))))
    (when-let ((buf (get-buffer buf-name))
               (proc (get-buffer-process buf)))
      (when (and (buffer-live-p buf)
                 (process-live-p proc))
        (kill-process proc)))

    ;; (dante-repl--start-with-buffer-name buf-name)
    (let ((repl-buf (if inside-repl-buffer?
                        (get-buffer-create buf-name)
                      (current-buffer))))
      (condition-case err
          (progn
            (if dante-repl--command-line-to-use
                (dante-repl--start-in-buffer-with-command-line repl-buf dante-repl--command-line-to-use nil)
              (dante-repl--start-in-buffer repl-buf nil))
            (unless inside-repl-buffer?
              (switch-to-buffer-other-window repl-buf)))
        (error
         (kill-buffer repl-buf)
         (signal (car err) (cdr err)))))))

(defun dante-repl--start-with-buffer-name (buf-name)
  (let ((repl-buf (get-buffer-create buf-name)))
    (condition-case err
        (progn
          (dante-repl--start-in-buffer repl-buf nil)
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
                         ?>
                         ;; ?λ
                         ))))))

(defun dante-repl--start-in-buffer (repl-buf extra-command)
  (let ((dante-buf (dante-buffer-p)))
    (if dante-buf
        (let ((command-line
               (-non-nil (-map #'eval (dante-repl--command-line-to-use)))))
          (dante-repl--start-in-buffer-with-command-line repl-buf command-line extra-command))
      (error "Dante not started - don't have command line for GHCI REPL yet."))))

(defun dante-repl--start-in-buffer-with-command-line (repl-buf command-line extra-command)
  (with-current-buffer repl-buf
    (cd (dante-project-root))
    (dante-repl-mode)
    (setq-local dante-repl--command-line-to-use command-line)
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
               ":set prompt \"\\4 \""))
          (comint-simple-send proc (if extra-command
                                       (concat cmd "\n" extra-command)
                                     cmd)))
        (message "Started REPL with %s" (s-join " " command-line))
        t))))

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
         (current-buffer) ;; output buffer
         proc ;; target process
         nil  ;; echo
         t)   ;; no-display
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
        (dante-repl--send-load-command repl-buf-name repl-buf it)
      (with-temporary-file tmp-file
          (shell-quote-argument (file-name-nondirectory (buffer-name)))
          "tmp"
          (buffer-substring-no-properties (point-min) (point-max))
        (dante-repl--send-load-command repl-buf-name repl-buf tmp-file)))
    (switch-to-buffer-other-window (get-buffer repl-buf-name))))

(defun dante-repl--send-load-command (repl-buf-name repl-buf file-to-load)
  (let ((cmd (concat ":load \"" file-to-load "\"")))
    (if (and repl-buf
             (buffer-live-p repl-buf))
        (let ((proc (get-buffer-process repl-buf)))
          (if (process-live-p proc)
              (comint-simple-send proc cmd)
            (dante-repl--start-in-buffer repl-buf cmd)))
      (let ((buf (get-buffer-create repl-buf-name)))
        (dante-repl--start-in-buffer buf cmd)))))

(defun dante-repl-clear-buffer-above-prompt ()
  (interactive)
  (let ((col (current-column)))
    (forward-line -1)
    (end-of-line)
    (forward-char 1)
    (let ((inhibit-read-only t))
      (set-text-properties (point-min) (point) nil))
    (remove-overlays (point-min) (line-end-position))
    (delete-region (point-min) (line-end-position))
    ;; Skip prompt.
    (end-of-line)
    ;; `current-column' doesn't count prompt width, but
    ;; `move-to-column' does and that's why we don't use it here.
    (forward-char col)))

(provide 'dante-repl)

;; Local Variables:
;; End:

;; dante-repl.el ends here
