;; flycheck-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 May 2016
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(defvar lsp-document-sync-method)

(require 'common)

(require 'current-column-fixed)
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
       (let-alist-static (flycheck-count-errors flycheck-current-errors) (info warning error)
         (when (or error warning info)
           (concat
            (set-string-face-property 'compilation-error (number->string (or error 0)))
            "/"
            (set-string-face-property 'compilation-warning (number->string (or warning 0)))
            "/"
            (set-string-face-property 'compilation-info (number->string (or info 0)))))))
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

;;;###autoload (autoload 'vim:flycheck-run "flycheck-setup" nil t)
;;;###autoload (autoload 'vim:flycheck-run:interactive "flycheck-setup" nil t)
(vim-defcmd vim:flycheck-run (nonrepeatable)
  "Force re-send buffer’s content to currently running LSP server.
Useful if the server got confused with incremental updating
scheme and it’s view of current buffer is malformed."
  (let ((time (current-time)))
    ;; Set modtime of the underlying file
    (set-file-times (buffer-file-name) time)
    ;; Sync buffer’s recorded modtime so that auto-revert won’t trigger.
    (set-visited-file-modtime) time)

  (when lsp-mode
    (let ((n (buffer-size))
          ;; Force full sync now
          (lsp-document-sync-method lsp--sync-full)
          ;; Don’t make full sync asynchronuously - block since the user specifically
          ;; pressed this button to synchronize
          (lsp-debounce-full-sync-notifications nil))
      (lsp-on-change 0 n n)))
  (flycheck-buffer))

(vim-defcmd vim:flycheck-compile (nonrepeatable)
  (call-interactively #'flycheck-compile))

(vim-defcmd vim:flycheck-clear (nonrepeatable)
  (flycheck-clear
   t ;; interrupt running process
   ))

(vim-defcmd vim:flycheck-list-errors (nonrepeatable)
  (flycheck-list-errors))

;;;###autoload
(cl-defun flycheck-install-ex-commands! (&key install-flycheck load-func reset-func)
  (when install-flycheck
    (vim-local-emap "ff" #'vim:flycheck-compile)
    (dolist (cmd '("check" "ch"))
      (vim-local-emap cmd #'vim:flycheck-run))
    (dolist (cmd '("re" "reset"))
      (vim-local-emap cmd
                      (or reset-func
                          #'vim:flycheck-clear:interactive)))
    (dolist (cmd '("errors" "errs"))
      (vim-local-emap cmd #'vim:flycheck-list-errors)))
  (when load-func
    (dolist (cmd '("load" "lo" "l"))
      (vim-local-emap cmd load-func))))

(defun flycheck-error< (a b)
  (let ((filename-a (awhen (flycheck-error-filename a) (expand-file-name it)))
        (filename-b (awhen (flycheck-error-filename b) (expand-file-name it))))
    (cl-assert (stringp filename-a))
    (cl-assert (stringp filename-b))
    (or (string< filename-a filename-b)
        (and (string= filename-a filename-b)
             (let ((line-a     (flycheck-error-line a))
                   (line-b     (flycheck-error-line b)))
               (cl-assert (or (integerp line-a) (null line-a)))
               (cl-assert (or (integerp line-b) (null line-b)))
               (or (extended< line-a line-b)
                   (and (or (and (null line-a) (null line-b))
                            (= line-a line-b))
                        (let ((column-a   (flycheck-error-column a))
                              (column-b   (flycheck-error-column b)))
                          (cl-assert (or (integerp column-a) (null colunm-a)))
                          (cl-assert (or (integerp column-b) (null colunm-b)))
                          (extended< column-a column-b)))))))))

(defun flycheck-enhancements--navigate-errors-with-wraparound (forward? errs)
  (let* ((expanded-buffer-file-name (aif buffer-file-name
                                        (expand-file-name it)
                                      (buffer-name)))
         (curr-buf (current-buffer))
         (all-errors (--separate
                      (if-let (fname (flycheck-error-filename it))
                          (string= expanded-buffer-file-name
                                   (expand-file-name fname))
                        (if-let (buf (flycheck-error-buffer it))
                            (eq buf curr-buf)
                          nil))
                      (-sort #'flycheck-error< errs)))
         (current-file-errors (car all-errors))
         (other-all (cadr all-errors))
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
                     (current-col (1+ (current-column-fixed)))
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
          (progn
            (if-let (filename (flycheck-error-filename next-error))
                (progn
                  (if (file-exists-p (flycheck-error-filename next-error))
                      (find-file (flycheck-error-filename next-error))
                    (aif (compilation/find-buffer (flycheck-error-filename next-error)
                                                  (funcall flycheck-enhancements--get-project-root-for-current-buffer))
                        (switch-to-buffer it)
                      (error "Failed to find path the error refers to: %s. file does not exist an no matching buffer is opened"
                             (flycheck-error-filename next-error)))))
              ;; No filename - got to the bufer in the error message
              (if-let (buf (flycheck-error-buffer next-error))
                  (switch-to-buffer buf)
                (error "Error does not refer to any file or buffer: %s" next-error)))
            (goto-line-dumb (flycheck-error-line next-error))
            (awhen (flycheck-error-column next-error)
              ;; Flycheck columns are 1-based .
              (move-to-character-column (- it 1)))
            (flycheck-display-error-at-point))
        (let ((message-log-max nil))
          (message "No more flycheck errors"))))))

(defun flycheck-enhancements-previous-error-with-wraparound ()
  (interactive)
  (if flycheck-mode
      (flycheck-enhancements--navigate-errors-with-wraparound nil flycheck-current-errors)
    (error "Flycheck not enabled")))

(defun flycheck-enhancements-next-error-with-wraparound ()
  (interactive)
  (if flycheck-mode
      (flycheck-enhancements--navigate-errors-with-wraparound t flycheck-current-errors)
    (error "Flycheck not enabled")))

;;;###autoload
(defun flycheck-setup-from-eproj (proj default-checker &optional preprocess-checker)
  (flycheck-setup-from-eproj-deferred proj default-checker preprocess-checker #'flycheck-mode))

(defun flycheck-setup-from-eproj-deferred (proj default-checker preprocess-checker consume-outcome)
  (when (not noninteractive)
    (let* ((flycheck-backend
            (eproj-query/checker proj major-mode default-checker)))
      (setq-local flycheck-disabled-checkers
                  (eproj-query/disabled-checkers
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
            (funcall consume-outcome +1))
        ;; Disable flycheck if it was explicitly set to nil
        (progn
          (when flycheck-mode
            (funcall consume-outcome -1)))))))

(provide 'flycheck-setup)

;; Local Variables:
;; End:

;; flycheck-setup.el ends here
