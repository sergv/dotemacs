;; flycheck-setup.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 12 May 2016
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'dash)
  (require 'macro-util))

(defvar lsp-document-sync-method)

(defvar dante-check-force-interpret)

(require 'common)
(require 'dash)

(require 'current-column-fixed)
(provide 'flycheck-setup)


(defvar-local flycheck-enhancements--error-overlays nil
  "List of flycheck error overlays in current buffer. Overlays may be invalid/deleted,
do check that ‘overlay-buffer’ is non-nil before use.")

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
           flycheck-relevant-error-other-file-minimum-level nil)

     ;; Need same ‘flycheck-add-overlay’ but with storing overlays in a variables for
     ;; ease of access.
     (remove-hook 'flycheck-process-error-functions #'flycheck-add-overlay)
     (add-hook 'flycheck-process-error-functions #'flycheck-add-and-record-overlay 'append)
     (add-hook 'flycheck-before-syntax-check-hook #'flycheck-reset-recorded-overlays)))

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

(defun flycheck-force-run ()
  "Force re-send buffer’s content to currently running LSP server.
Useful if the server got confused with incremental updating
scheme and it’s view of current buffer is malformed."
  (interactive)
  (let ((time (current-time))
        (buf (resolve-to-base-buffer (current-buffer))))
    (with-current-buffer buf
      ;; Set modtime of the underlying file
      (set-file-times (buffer-file-name buf) time)
      ;; Sync buffer’s recorded modtime so that auto-revert won’t trigger.
      (set-visited-file-modtime time)))

  (when lsp-mode
    (let ((n (buffer-size))
          ;; Force full sync now
          (lsp-document-sync-method lsp--sync-full)
          ;; Don’t make full sync asynchronuously - block since the user specifically
          ;; pressed this button to synchronize
          (lsp-debounce-full-sync-notifications nil))
      (lsp-on-change 0 n n)))

  (flycheck-buffer))

;;;###autoload (autoload 'vim:flycheck-run "flycheck-setup" nil t)
;;;###autoload (autoload 'vim:flycheck-run:interactive "flycheck-setup" nil t)
(vim-defcmd vim:flycheck-run (nonrepeatable)
  (flycheck-force-run))

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

(defun flycheck-error-and-ov-pair< (a b)
  (cl-assert (consp a))
  (cl-assert (consp b))
  (let* ((ov-a (cdr-sure a))
         (ov-b (cdr-sure b))
         (start-a (overlay-start ov-a))
         (start-b (overlay-start ov-b)))
    (cl-assert (overlayp ov-a))
    (cl-assert (overlayp ov-b))
    (or (< start-a start-b)
        (and (= start-a start-b)
             (let ((end-a (overlay-end ov-a))
                   (end-b (overlay-end ov-b)))
               (or (< start-a start-b)
                   (and (= start-a start-b)
                        (let ((err-a (car-sure a))
                              (err-b (car-sure b)))
                          (cl-assert (flycheck-error-p err-a))
                          (cl-assert (flycheck-error-p err-b))
                          (flycheck-error< err-a err-b)))))))))

;; This function assumes that each flycheck error necessarily has a
;; corresponding overlay (in current buffer, no less). This seems to
;; hold - all reported errors have to be presented to the user at some point.
;; Invisible errors would not be desirable.
(defun flycheck-enhancements--navigate-errors-with-wraparound (forward? error-overlays)
  (let* ((curr-buf (current-buffer))
         (min-pt (point-min))
         (max-pt (point-max))
         (buf (resolve-to-base-buffer curr-buf))
         (is-indirect? (not (eq curr-buf buf)))
         (expanded-buffer-file-name (aif buffer-file-name
                                        (expand-file-name it)
                                      (if is-indirect?
                                          (aif (buffer-file-name buf)
                                              (expand-file-name it)
                                            (buffer-name buf))
                                        (buffer-name))))
         (curr-buf (current-buffer))
         (all-errors (--separate
                      (let ((err (car-sure it)))
                        (if-let (fname (flycheck-error-filename err))
                            (string= expanded-buffer-file-name
                                     (expand-file-name fname))
                          (if-let (buf (flycheck-error-buffer err))
                              (eq buf curr-buf)
                            nil)))
                      (sort (--map (cons (overlay-get it 'flycheck-error) it)
                                   error-overlays)
                            #'flycheck-error-and-ov-pair<)))
         (current-file-errors
          (--filter (let ((ov (cdr-sure it)))
                      (and (<= min-pt (overlay-start ov))
                           (<= (overlay-end ov) max-pt)))
                    (car all-errors)))
         (other-all (cadr all-errors))
         (other-errors
          (--filter (eq (flycheck-error-level (car it)) 'error) other-all))

         (next-error-and-overlay
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
                    (cmp (if forward? #'<= #'<))
                    (pt (point))
                    (current-file-errors-around-current-line
                     (--split-with (funcall cmp
                                            (overlay-start (cdr-sure it))
                                            pt)
                                   current-file-errors))
                    (current-file-errors-before-current-line
                     (cl-first current-file-errors-around-current-line))
                    (current-file-errors-after-current-line
                     (cl-second current-file-errors-around-current-line)))
               (car (funcall
                     (if forward? #'identity #'nreverse)
                     (nconc current-file-errors-after-current-line
                            ;; Make errors wrap around.
                            current-file-errors-before-current-line)))))
            (other-all
             ;; Any warnings or hlint suggestions in dependent buffers.
             (car other-all))
            (t nil)))

         (next-error (car next-error-and-overlay))
         (next-error-overlay (cdr next-error-and-overlay)))

    (if next-error
        (let ((switched?
               (if-let* ((filename (flycheck-error-filename next-error)))
                   ;; Test if we’re already visiting the necessary file.
                   (unless (string= filename (buffer-file-name buf))
                     (if (file-exists-p filename)
                         (progn
                           (find-file filename)
                           t)
                       (aif (compilation/find-buffer filename
                                                     (funcall flycheck-enhancements--get-project-root-for-current-buffer))
                           (progn
                             (switch-to-buffer it)
                             t)
                         (error "Failed to find path the error refers to: ‘%s’. file does not exist an no matching buffer is opened"
                                (flycheck-error-filename next-error)))))
                 ;; No filename - got to the bufer in the error message
                 (if-let* ((err-buf (flycheck-error-buffer next-error)))
                     (when
                         (not
                          ;; When called from indirect buffer the error may be for either
                          ;; the indirect buffer or its base buffer
                          ;; (e.g. copied when the indirect was created).
                          ;;
                          ;; Both cases are acceptable
                          (or (eq err-buf buf)
                              (eq err-buf curr-buf)))
                       (switch-to-buffer err-buf)
                       t)
                   (error "Error does not refer to any file or buffer: ‘%s’" next-error)))))
          (if switched?
              ;; Jumped to different buffer - use position from error struct.
              (progn
                (goto-line-dumb (flycheck-error-line next-error))
                (awhen (flycheck-error-column next-error)
                  ;; Flycheck columns are 1-based .
                  (move-to-character-column (- it 1))))
            ;; Stayed in current buffer - use position from overlays
            (goto-char (overlay-start (cdr next-error-and-overlay))))
          (flycheck-display-error-at-point))
      (let ((message-log-max nil))
        (message "No more flycheck errors")))))

(defun flycheck-enhancements-previous-error-with-wraparound ()
  (interactive)
  (if flycheck-mode
      (flycheck-enhancements--navigate-errors-with-wraparound nil
                                                              (flycheck-enhancements--get-error-overlays))
    (error "Flycheck not enabled")))

(defun flycheck-enhancements-next-error-with-wraparound ()
  (interactive)
  (if flycheck-mode
      (flycheck-enhancements--navigate-errors-with-wraparound t
                                                              (flycheck-enhancements--get-error-overlays))
    (error "Flycheck not enabled")))

;;;###autoload
(defun flycheck-setup-from-eproj (proj default-checker &optional on-checker-selected)
  (flycheck-setup-from-eproj-deferred proj default-checker on-checker-selected #'flycheck-mode))

(defun flycheck-setup-from-eproj-deferred (proj default-checker on-checker-selected consume-outcome)
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
            (awhen on-checker-selected
              (funcall it flycheck-backend))
            ;; (unless (flycheck-eligible-checker? flycheck-backend)
            ;;   (flycheck-verify-checker flycheck-backend)
            ;;   (error "Unable to select checker '%s' for buffer '%s'"
            ;;          flycheck-backend (current-buffer)))
            (setq-local flycheck-checker flycheck-backend)
            (add-hook 'flycheck-after-syntax-check-hook
                      #'flycheck-highlight-errors-with-attrap-fix
                      nil
                      t)
            (funcall consume-outcome +1))
        ;; Disable flycheck if it was explicitly set to nil
        (progn
          (when flycheck-mode
            (funcall consume-outcome -1)))))))

(defface flycheck-error-fix-available
  '((t :inherit flycheck-error))
  "Flycheck face for errors that have attrap fix."
  :group 'flycheck-faces)

(defface flycheck-warning-fix-available
  '((t :inherit flycheck-warning))
  "Flycheck face for warnings that have attrap fix."
  :group 'flycheck-faces)

(defface flycheck-info-fix-available
  '((t :inherit flycheck-info))
  "Flycheck face for info that have attrap fix."
  :group 'flycheck-faces)

(defun flycheck-highlight-errors-with-attrap-fix ()
  (when-let ((checker (flycheck-get-checker-for-buffer))
             (messages
              (--filter-nondet
               (car-sure it)
               (--map (cons (flycheck-error-message (overlay-get it 'flycheck-error))
                            it)
                      (flycheck-enhancements--get-error-overlays)))))
    (dolist (entry (attrap--find-fixes-for-flycheck-messages checker messages))
      (cl-destructuring-bind (_ _ ov) entry
        (cl-assert (overlayp ov))
        (let ((err (overlay-get ov 'flycheck-error)))
          (cl-assert (flycheck-error-p err))
          (overlay-put ov
                       'face
                       (pcase (flycheck-error-level err)
                         (`info    'flycheck-info-fix-available)
                         (`warning 'flycheck-warning-fix-available)
                         (`error   'flycheck-error-fix-available))))))))

(defun flycheck-enhancements--get-error-overlays ()
  ;; Filter out deleted overlays
  (--filter-nondet
   (overlay-buffer it)
   flycheck-enhancements--error-overlays))

(defun flycheck-reset-recorded-overlays ()
  "Reset ‘flycheck-enhancements--error-overlays’ before new syntax check."
  (setf flycheck-enhancements--error-overlays nil))

(defun flycheck-add-and-record-overlay (err)
  "Like ‘flycheck-add-overlay’ but records added overlays in flycheck-enhancements--error-overlays."
  (cl-assert (flycheck-error-p err))
  (let ((ov (flycheck-add-overlay err)))
    (cl-assert (overlayp ov))
    (push ov flycheck-enhancements--error-overlays)
    ov))

(provide 'flycheck-setup)

;; Local Variables:
;; End:

;; flycheck-setup.el ends here
