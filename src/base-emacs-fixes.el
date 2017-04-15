;; base-emacs-fixes.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 12 May 2013
;; Description:
;; Fixes for errors in standard emacs files

(eval-when-compile (require 'cl-lib))

;; added cl-remove-duplicates to avoid scenario when two identical
;; hooks get called
(defun run-mode-hooks (&rest hooks)
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
    (setq hooks (cl-remove-duplicates (nconc (nreverse delayed-mode-hooks) hooks)
                                      :test #'eq))
    (setq delayed-mode-hooks nil)
    (apply 'run-hooks (cons 'change-major-mode-after-body-hook hooks))
    (run-hooks 'after-change-major-mode-hook)))

(provide 'base-emacs-fixes)

;; Local Variables:
;; End:

;; base-emacs-fixes.el ends here
