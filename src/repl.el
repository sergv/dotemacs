;;; repl.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 11 November 2011
;; Keywords:
;; Requirements:
;; Status:

;; Utility macro to define convenient custom interaction modes
;; with various interpreters, abstracted from inf-haskell.el ghci interaction.
;; Currently is not even alpha since I still never tried it and it misses
;; variable declaration.
;;
;; Also it's arguable whether it's of any use since for Haskell we
;; already have decent repl and for Common Lisp there's SLIME already.
;; I thought it would be useful to use with Scheme, but I don't use
;; Scheme that much.




(defun repl-spot-prompt (string)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (save-excursion
       (goto-char (process-mark proc))
       (if (re-search-backward comint-prompt-regexp
                               (line-beginning-position) t)
         (setq repl-seen-prompt t))))))

(defun repl-wait-for-prompt (proc &optional timeout)
  "Wait until PROC sends us a prompt.
The process PROC should be associated to a comint buffer."
  (setq timeout (or timeout 10))
  (with-current-buffer (process-buffer proc)
    (while (progn
             (goto-char comint-last-input-end)
             (not (or repl-seen-prompt
                      (setq repl-seen-prompt
                            (re-search-forward comint-prompt-regexp nil t))
                      (not (accept-process-output proc timeout))))))
    (unless repl-seen-prompt
      (error "Can't find the prompt"))))


(define-derived-mode repl-mode comint-mode "REPL"
  "Major mode for interacting with any REPL."
  (setq-local comint-prompt-regexp
              ;; Whay the backslash in [\\._[:alnum:]]?
              "^> \\|^\\*?[[:upper:]][\\._[:alnum:]]*\\(?: \\*?[[:upper:]][\\._[:alnum:]]*\\)*> ")
  (setq-local comint-input-autoexpand nil)
  (add-hook 'comint-output-filter-functions 'repl-spot-prompt nil t)

  ;; Setup directory tracking.
  (setq-local shell-cd-regexp nil) ;; ???

  (condition-case nil
      (shell-dirtrack-mode 1)
    (error      ;The minor mode function may not exist or not accept an arg.
     (setq-local shell-dirtrackp t)
     (add-hook 'comint-input-filter-functions 'shell-directory-tracker
               nil 'local)))

  ;; Setup `compile' support so you can just use C-x ` and friends.
  (setq-local compilation-error-regexp-alist
              repl-error-regexp-alist)
  (setq-local compilation-first-column 1) ;GHCI counts from 0. ; upd: ghci counts from 1
  (if (and (not (boundp 'minor-mode-overriding-map-alist))
           (fboundp 'compilation-shell-minor-mode))
    ;; If we can't remove compilation-minor-mode bindings, at least try to
    ;; use compilation-shell-minor-mode, so there are fewer
    ;; annoying bindings.
    (compilation-shell-minor-mode 1)
    ;; Else just use compilation-minor-mode but without its bindings because
    ;; things like mouse-2 are simply too annoying.
    (compilation-minor-mode 1)
    (let ((map (make-sparse-keymap)))
      (dolist (keys '([menu-bar] [follow-link]))
        ;; Preserve some of the bindings.
        (define-key map keys (lookup-key compilation-minor-mode-map keys)))
      (add-to-list 'minor-mode-overriding-map-alist
                   (cons 'compilation-minor-mode map)))))

(defun repl-start-process (command)
  "Start an inferior haskell process.
With universal prefix \\[universal-argument], prompts for a COMMAND,
otherwise uses `haskell-program-name'.
It runs the hook `repl-hook' after starting the process and
setting up the inferior-haskell buffer."
  (setq repl-buffer
	(apply 'make-comint "repl" (car command) nil (cdr command)))
  (with-current-buffer repl-buffer
    (repl-mode)
    (run-hooks 'repl-hook)))



(defun repl-process (&optional arg)
  (or (if (buffer-live-p repl-buffer)
        (get-buffer-process repl-buffer))
      (progn
	(let ((current-prefix-arg arg))
   (call-interactively 'repl-start-process))
	;; Try again.
	(repl-process arg))))


(defun repl-send-command (proc str)
  ;; (setq str (concat str "\n"))
  (with-current-buffer (process-buffer proc)
    (repl-wait-for-prompt proc)
    (goto-char (process-mark proc))
    (insert-before-markers str)
    (move-marker comint-last-input-end (point))
    (setq repl-seen-prompt nil)
    (comint-send-string proc str)))





(defun switch-to-haskell (&optional arg)
  "Show the inferior-haskell buffer.  Start the process if needed."
  (interactive "P")
  (let ((proc (repl-process arg)))
    (pop-to-buffer (process-buffer proc))))


;;; repl.el ends here
