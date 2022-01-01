;; flycheck-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 May 2016
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist)
  (require 'macro-util))

(provide 'flycheck-setup)

(defvar-local flycheck-enhancements--get-project-root-for-current-buffer (lambda () nil)
  "Function that should return potential project root this buffer is part of.")

;;;###autoload
(eval-after-load "flycheck" '(require 'flycheck-setup))

(eval-after-load "flycheck"
  '(progn
     ;; Don't show errors on fringes.
     (setf flycheck-indication-mode nil
           ;; Highlight whole line with error
           flycheck-highlighting-mode 'lines
           flycheck-display-errors-delay 0
           flycheck-check-syntax-automatically '(save mode-enabled)
           ;; Display all errors & warnings from all relevant files.
           flycheck-relevant-error-other-file-minimum-level nil)))

(defun flycheck-mode-line--propertise-as-comments (str)
  (propertize str 'face 'font-lock-comment-face))

;;;###autoload
(defun flycheck-pretty-mode-line ()
  (when flycheck-mode
    (pcase flycheck-last-status-change
      (`not-checked (flycheck-mode-line--propertise-as-comments "flycheck:unchecked"))
      (`no-checker  (flycheck-mode-line--propertise-as-comments "flycheck:no-checker"))
      (`running     (flycheck-mode-line--propertise-as-comments "flycheck:running"))
      (`errored     "flycheck:error while checking")
      (`finished
       (let-alist (flycheck-count-errors flycheck-current-errors)
         (when (or .error .warning .info)
           (format "%s/%s/%s"
                   (propertize (format "%d" (or .error 0)) 'face 'compilation-error)
                   (propertize (format "%d" (or .warning 0)) 'face 'compilation-warning)
                   (propertize (format "%d" (or .info 0)) 'face 'compilation-info)))))
      (`interrupted (flycheck-mode-line--propertise-as-comments "flycheck:interrupted"))
      (`suspicious  "flycheck:suspicious output from checker")
      (_ (propertize "flycheck:unknown" 'face 'error)))))

;;;###autoload
(defun flycheck-error-list-setup ()
  (def-keys-for-map flycheck-error-list-mode-map
    +vim-search-keys+
    +vim-special-keys+
    (("h" "<down>") flycheck-error-list-next-error)
    (("t" "<up>")   flycheck-error-list-previous-error)
    ("H"            flycheck-error-list-check-source)
    ("<escape>"     quit-window)))

;;;###autoload
(add-hook 'flycheck-error-list-mode-hook #'flycheck-error-list-setup)

(defadvice flycheck-display-error-messages
    (before
     flycheck-display-error-messages-set-mode-in-error-buffer
     activate
     compile)
  (unless (buffer-live-p flycheck-error-message-buffer)
    (with-current-buffer (get-buffer-create flycheck-error-message-buffer)
      (text-mode))))

;;;###autoload
(defun flycheck-eligible-checker? (checker)
  "Similar to `flycheck-may-use-checker' but doesn’t pay attention to checker’s predicate."
  (and (flycheck-valid-checker-p checker)
       (flycheck-checker-supports-major-mode-p checker)
       (flycheck-may-enable-checker checker)))

(vim:defcmd vim:flycheck-run (nonrepeatable)
  (flycheck-buffer))

(vim:defcmd vim:flycheck-compile (nonrepeatable)
  (call-interactively #'flycheck-compile))

(vim:defcmd vim:flycheck-clear (nonrepeatable)
  (flycheck-clear
   t ;; interrupt running process
   ))

(vim:defcmd vim:flycheck-list-errors (nonrepeatable)
  (flycheck-list-errors))

;;;###autoload
(cl-defun flycheck-install-ex-commands! (&key install-flycheck load-func reset-func)
  (when install-flycheck
    (vim:local-emap "ff" #'vim:flycheck-compile)
    (dolist (cmd '("check" "ch"))
      (vim:local-emap cmd #'vim:flycheck-run))
    (dolist (cmd '("re" "reset"))
      (vim:local-emap cmd
                      (or reset-func
                          #'vim:flycheck-clear)))
    (dolist (cmd '("errors" "errs"))
      (vim:local-emap cmd #'vim:flycheck-list-errors)))
  (when load-func
    (dolist (cmd '("load" "lo" "l"))
      (vim:local-emap cmd load-func))))

(defun flycheck-error< (a b)
  (let ((filename-a (expand-file-name (flycheck-error-filename a)))
        (filename-b (expand-file-name (flycheck-error-filename b)))
        (line-a     (flycheck-error-line a))
        (line-b     (flycheck-error-line b))
        (column-a   (flycheck-error-column a))
        (column-b   (flycheck-error-column b)))
    (or (string< filename-a filename-b)
        (and (string= filename-a filename-b)
             (or (extended< line-a line-b)
                 (and (or (and (null line-a) (null line-b))
                          (= line-a line-b))
                      (extended< column-a column-b)))))))

(defun flycheck-enhancements--navigate-errors-with-wraparound (forward? errs)
  (let* ((expanded-buffer-file-name (expand-file-name buffer-file-name))
         (all-errors (--separate
                      (string= expanded-buffer-file-name
                               (expand-file-name (flycheck-error-filename it)))
                      (-sort #'flycheck-error< errs)))
         (current-file-errors (first all-errors))
         (other-all (second all-errors))
         (other-errors
          (--filter (eq (flycheck-error-level it) 'error) other-all)))
    (let ((next-error
           (cond
             (other-errors
              ;; Search in other files first - they're dependencies and
              ;; current module would not work without fixing them first.
              (car other-errors))
             (current-file-errors
              ;; Search in current file.
              (let* ((current-line (line-number-at-pos))
                     (current-col (1+ (current-column)))
                     (current-pos (cons current-line current-col))
                     ;; Make sure that current error will go to the
                     ;; end of the candidate list regardless of the
                     ;; direction we're searching.
                     (cmp (if forward? #'extended<= #'extended<))
                     (position<= (lambda (x y)
                                   (let ((line-x (car x))
                                         (line-y (car y))
                                         (col-x (cdr x))
                                         (col-y (cdr y)))
                                     (or (< line-x line-y)
                                         (and (= line-x line-y)
                                              (funcall cmp col-x col-y))))))
                     (current-file-errors-around-current-line
                      (--split-with (funcall position<=
                                             (cons (flycheck-error-line it) (flycheck-error-column it))
                                             current-pos)
                                    current-file-errors))
                     (current-file-errors-before-current-line
                      (first current-file-errors-around-current-line))
                     (current-file-errors-after-current-line
                      (second current-file-errors-around-current-line)))
                (car (funcall
                      (if forward? #'identity #'nreverse)
                      (nconc current-file-errors-after-current-line
                             ;; Make errors wrap around.
                             current-file-errors-before-current-line)))))
             (other-all
              ;; Any warnings or hlint suggestions in dependent buffers.
              (car other-all))
             (t nil))))
      (if next-error
          (if-let ((filename (flycheck-error-filename next-error)))
              (progn
                (if (file-exists-p (flycheck-error-filename next-error))
                    (find-file (flycheck-error-filename next-error))
                  (awhen (compilation/find-buffer (flycheck-error-filename next-error)
                                                  (funcall flycheck-enhancements--get-project-root-for-current-buffer))
                    (switch-to-buffer it)))
                (goto-line-dumb (flycheck-error-line next-error))
                (awhen (flycheck-error-column next-error)
                  ;; Flycheck columns are 1-based .
                  (move-to-character-column (- it 1)))
                (flycheck-display-error-at-point))
            (error "Error does not refer to any file: %s" next-error))
        (message "No more flycheck errors")))))

(defun flycheck-enhancements-previous-error-with-wraparound ()
  (interactive)
  (flycheck-enhancements--navigate-errors-with-wraparound nil flycheck-current-errors))

(defun flycheck-enhancements-next-error-with-wraparound ()
  (interactive)
  (flycheck-enhancements--navigate-errors-with-wraparound t flycheck-current-errors))

;;;###autoload
(defun flycheck-setup-from-eproj (proj default-checker &optional preprocess-checker)
  (when (not noninteractive)
    (let* ((flycheck-backend
            (eproj-query/flycheck-checker proj major-mode default-checker)))
      (setq-local flycheck-disabled-checkers
                  (eproj-query/flycheck-disabled-checkers
                   proj
                   major-mode
                   flycheck-disabled-checkers))
      (if flycheck-backend
          (progn
            (awhen preprocess-checker
              (funcall it flycheck-backend))
            ;; (unless (flycheck-eligible-checker? flycheck-backend)
            ;;   (flycheck-verify-checker flycheck-backend)
            ;;   (error "Unable to select checker '%s' for buffer '%s'"
            ;;          flycheck-backend (current-buffer)))
            (setq-local flycheck-checker flycheck-backend)
            (flycheck-mode +1))
        ;; Disable flycheck if it was explicitly set to nil
        (progn
          (when flycheck-mode
            (flycheck-mode -1)))))))

(provide 'flycheck-setup)

;; Local Variables:
;; End:

;; flycheck-setup.el ends here
