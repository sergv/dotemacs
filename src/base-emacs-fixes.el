;; base-emacs-fixes.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 12 May 2013
;; Description:
;; Fixes for errors in standard emacs files

(eval-when-compile (require 'cl-lib))

(require 'el-patch)

;; added cl-remove-duplicates to avoid scenario when two identical
;; hooks get called
;; (defun run-mode-hooks (&rest hooks)
;;   "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
;; If the variable `delay-mode-hooks' is non-nil, does not run any hooks,
;; just adds the HOOKS to the list `delayed-mode-hooks'.
;; Otherwise, runs hooks in the sequence: `change-major-mode-after-body-hook',
;; `delayed-mode-hooks' (in reverse order), HOOKS, and finally
;; `after-change-major-mode-hook'.  Major mode functions should use
;; this instead of `run-hooks' when running their FOO-mode-hook."
;;   (if delay-mode-hooks
;;       ;; Delaying case.
;;       (dolist (hook hooks)
;;         (push hook delayed-mode-hooks))
;;     ;; Normal case, just run the hook as before plus any delayed hooks.
;;     (setq hooks (cl-remove-duplicates (nconc (nreverse delayed-mode-hooks) hooks)
;;                                       :test #'eq))
;;     (setq delayed-mode-hooks nil)
;;     (apply 'run-hooks (cons 'change-major-mode-after-body-hook hooks))
;;     (run-hooks 'after-change-major-mode-hook)))

;; added cl-remove-duplicates to avoid scenario when two identical
;; hooks get called

(cond
  ((eval-when-compile
     (string-match-p "^25\\.[0-9]+\\.[0-9]+$" emacs-version))
   (el-patch-defun run-mode-hooks (&rest hooks)
     "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
If the variable `delay-mode-hooks' is non-nil, does not run any hooks,
just adds the HOOKS to the list `delayed-mode-hooks'.
Otherwise, runs hooks in the sequence: `change-major-mode-after-body-hook',
`delayed-mode-hooks' (in reverse order), HOOKS, and finally
`after-change-major-mode-hook'.  Major mode functions should use
this instead of `run-hooks' when running their FOO-mode-hook."
     (if delay-mode-hooks
         ;; Delaying case.
         (dolist (hook hooks)
           (push hook delayed-mode-hooks))
       ;; Normal case, just run the hook as before plus any delayed hooks.
       (setq hooks (el-patch-swap
                     (nconc (nreverse delayed-mode-hooks) hooks)
                     (cl-remove-duplicates (nconc (nreverse delayed-mode-hooks) hooks)
                                           :test #'eq)))
       (setq delayed-mode-hooks nil)
       (apply 'run-hooks (cons 'change-major-mode-after-body-hook hooks))
       (run-hooks 'after-change-major-mode-hook))))
  (t
   (el-patch-defun run-mode-hooks (&rest hooks)
     "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
Call `hack-local-variables' to set up file local and directory local
variables.

If the variable `delay-mode-hooks' is non-nil, does not do anything,
just adds the HOOKS to the list `delayed-mode-hooks'.
Otherwise, runs hooks in the sequence: `change-major-mode-after-body-hook',
`delayed-mode-hooks' (in reverse order), HOOKS, then runs
`hack-local-variables', runs the hook `after-change-major-mode-hook', and
finally evaluates the functions in `delayed-after-hook-functions' (see
`define-derived-mode').

Major mode functions should use this instead of `run-hooks' when
running their FOO-mode-hook."
     (if delay-mode-hooks
         ;; Delaying case.
         (dolist (hook hooks)
           (push hook delayed-mode-hooks))
       ;; Normal case, just run the hook as before plus any delayed hooks.
       (setq hooks (el-patch-swap
                     (nconc (nreverse delayed-mode-hooks) hooks)
                     (cl-remove-duplicates (nconc (nreverse delayed-mode-hooks) hooks)
                                           :test #'eq)))
       (setq delayed-mode-hooks nil)
       (apply 'run-hooks (cons 'change-major-mode-after-body-hook hooks))
       (if (buffer-file-name)
           (with-demoted-errors "File local-variables error: %s"
             (hack-local-variables 'no-mode)))
       (run-hooks 'after-change-major-mode-hook)
       (dolist (fun (nreverse delayed-after-hook-functions))
         (funcall fun))
       (setq delayed-after-hook-functions nil)))))

;;;###autoload
(el-patch-feature autorevert)

(eval-after-load "autorevert"
  '(progn
     (el-patch-defun auto-revert-notify-add-watch ()
       "Enable file notification for current buffer's associated file."
       ;; We can assume that `buffer-file-name' and
       ;; `auto-revert-use-notify' are non-nil.
       (if (or (el-patch-wrap 2 0
                 (and default-directory
                      (string-match auto-revert-notify-exclude-dir-regexp
                                    (expand-file-name default-directory))))
               (el-patch-wrap 2 0
                 (and (or buffer-file-name default-directory)
                      (file-symlink-p (or buffer-file-name default-directory)))))

           ;; Fallback to file checks.
           (setq-local auto-revert-use-notify nil)

         (when (not auto-revert-notify-watch-descriptor)
           (setq auto-revert-notify-watch-descriptor
	         (ignore-errors
	           (if buffer-file-name
		       (file-notify-add-watch
		        (expand-file-name buffer-file-name default-directory)
		        '(change attribute-change)
		        'auto-revert-notify-handler)
		     (file-notify-add-watch
		      (expand-file-name default-directory)
		      '(change)
		      'auto-revert-notify-handler))))
           (if auto-revert-notify-watch-descriptor
	       (progn
	         (puthash
	          auto-revert-notify-watch-descriptor
	          (cons (current-buffer)
		        (gethash auto-revert-notify-watch-descriptor
			         auto-revert-notify-watch-descriptor-hash-list))
	          auto-revert-notify-watch-descriptor-hash-list)
	         (add-hook 'kill-buffer-hook
		           #'auto-revert-notify-rm-watch nil t))
             ;; Fallback to file checks.
	     (setq-local auto-revert-use-notify nil)))))))

(el-patch-defun push-mark (&optional location nomsg activate)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information.

In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil."
  (unless (null (mark t))
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (when (> (length mark-ring) mark-ring-max)
      (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
      (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil)))
  (set-marker (mark-marker) (or location (point)) (current-buffer))
  ;; Now push the mark on the global mark ring.
  (if (and global-mark-ring
	   (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
      ;; The last global mark pushed was in this same buffer.
      ;; Don't push another one.
      nil
    (setq global-mark-ring (cons (copy-marker (mark-marker)) global-mark-ring))
    (when (> (length global-mark-ring) global-mark-ring-max)
      (move-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
      (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil)))
  (el-patch-remove
    (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
        (message "Mark set")))
  (if (or activate (not transient-mark-mode))
      (set-mark (mark t)))
  nil)

(provide 'base-emacs-fixes)

;; Local Variables:
;; End:

;; base-emacs-fixes.el ends here
