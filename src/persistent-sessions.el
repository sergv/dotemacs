;; persistent-sessions.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 23 January 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'fortunes)
(require 'revive-minimal)

(setf revive-plus:all-frames t
      revive:save-variables-mode-local-private
      '((c++-mode c-indentation-style c-basic-offset)))

(defsubst make-session-entry (buf-name point variables major-mode other-data)
  (list buf-name point variables major-mode other-data))
(defsubst session-entry/buffer-name (entry)
  "Extract buffer name from ENTRY. For buffers with  files this is absolute file
name, for temporary buffers - just the buffer name."
  (first-safe entry))
(defsubst session-entry/point (entry)
  (first-safe (rest-safe entry)))
(defsubst session-entry/variables (entry)
  (first-safe (rest-safe (rest-safe entry))))
;; Major mode is remembered because there're cases when emacs infers wrong
;; mode for otherwise normal buffer. Also modes may be changed by hand, so
;; it is preserved for every buffer.
(defsubst session-entry/major-mode (entry)
  (first-safe (rest-safe (rest-safe (rest-safe entry)))))
(defsubst session-entry/other-data (entry)
  (first-safe (rest-safe (rest-safe (rest-safe (rest-safe entry))))))


(defparameter *sessions-buffer-variables*
  (list
   (list (lambda (buf)
           (require 'haskell-misc)
           (with-current-buffer buf (memq major-mode +haskell-syntax-modes+)))
         'haskell-compile-command
         'haskell-compile-cabal-build-command
         'haskell-process-args-ghci))
  "List of buffer-local variables to save in session file.
Format: list of (<predicate> <vars>) where predicate is a function of
single buffer argument that should return t if <vars> variables should
be saved or restored for current buffer. Obviously it shouldn't depend
on values of said variables.")

(defun sessions/get-buffer-variables (buffer)
  "Get buffer's local variables that should be saved."
  (with-current-buffer buffer
    (concatMap! (lambda (entry)
                  (let ((pred (car entry))
                        (vars (cdr entry)))
                    (when (funcall pred buffer)
                      (map (lambda (var)
                             (when (and (boundp var)
                                        (local-variable? var))
                               (cons var (symbol-value var))))
                           vars))))
                *sessions-buffer-variables*)))

(defun sessions/restore-buffer-variables (buffer bindings)
  "Restore variables captured in BINDINGS for buffer BUFFER."
  (with-current-buffer buffer
    (mapc (lambda (entry)
            (let ((pred (car entry))
                  (vars (cdr entry)))
              (when (funcall pred buffer)
                (dolist (bind bindings)
                  (let ((var (car bind)))
                    (when (memq var vars)
                      (set var (cdr bind))))))))
          *sessions-buffer-variables*)))

(defparameter *sessions-global-variables* '(log-edit-comment-ring
                                            vim:ex-history
                                            read-expression-history
                                            *search-minibuffer-history*)
  "List of global variables to save in session file.")

(defun sessions/strip-text-properties (val)
  (cond
    ((ring? val)
     (let ((result (make-ring (ring-size val))))
       (dotimes (n (ring-length val))
         (ring-insert result
                      (sessions/strip-text-properties
                       (ring-ref val n))))
       result))
    ((string? val)
     (substring-no-properties val))
    (t
     val)))

(defun sessions/truncate-long-sequences (val)
  "Truncate overly long sequences."
  (let ((max-size 1000))
    (cond
      ((ring? val)
       (if (< max-size (ring-size val))
         (foldr (lambda (r item)
                  (ring-insert r item)
                  r)
                (make-ring max-size)
                (subseq (ring-elements val) 0 (min max-size (ring-length val))))
         val))
      ((and (or (list? val)
                (vector? val))
            (< max-size (length val)))
       (subseq val 0 (min max-size (length val))))
      (t
       val))))

(defun sessions/get-global-variables ()
  "Get global variables that should be saved in form of sequence of (var . value)
entries."
  (remq nil
        (map (lambda (var)
               (when (boundp var)
                 (cons var (sessions/strip-text-properties
                            (sessions/truncate-long-sequences
                             (symbol-value var))))))
             *sessions-global-variables*)))

(defun sessions/restore-global-variables (bindings)
  "Restore global variables from BINDINGS."
  (dolist (bind bindings)
    (set (car bind) (cdr bind))))

(defparameter sessions/ignored-temporary-buffers
  '(dired-mode)
  "Buffer with these modes should never be preserved across sessions.")

(defparameter sessions/special-modes
  `((shell-mode
     (save ,(lambda (buf)
              (save-excursion
                (with-inhibited-read-only
                 (goto-char (point-max))
                 (forward-line -1)
                 (list (list 'contents
                             ;; collect properties as well
                             (buffer-substring (point-min) (point-max)))
                       (list 'current-dir
                             (expand-file-name default-directory))
                       (list 'comint-input-ring
                             comint-input-ring))))))
     (restore ,(lambda (buffer-name saved-data)
                 (awhen (assoc 'contents saved-data)
                   (let ((buf (get-buffer-create buffer-name)))
                     (with-current-buffer buf
                       (with-inhibited-read-only
                        (with-inhibited-modification-hooks
                         (with-inhibited-redisplay
                           (goto-char (point-min))
                           (insert (second it))
                           (insert "\n\n")))))
                     (shell buf)
                     (accept-process-output (get-buffer-process buf)
                                            5 ;; Time to wait in seconds.
                                            )
                     (when-let (current-dir
                                (cadr-safe
                                 (assoc 'current-dir saved-data)))
                       (goto-char (point-max))
                       (insert "cd \"" current-dir "\"")
                       (comint-send-input))
                     (awhen (cadr-safe (assoc 'comint-input-ring saved-data))
                       (setf comint-input-ring it)))))))))


(defun sessions/save-buffers (file)
  "Save all buffers that have physical file assigned into FILE."
  (interactive "Ffile to save session in: ")
  (with-temp-buffer
    (insert ":;exec emacs --load \"$0\"\n\n")
    (insert ";:;exec gdb -ex \"run\" --args emacs --load \"$0\"\n\n")
    (insert (format ";; this session was created on %s\n;; Today's quotes are\n%s\n"
                    (format-time-string "%A, %e %B %Y")
                    (loop
                      for i from 0 to 5
                      concating
                      (concat
                       (join-lines
                        (map (lambda (x) (concat ";; " x))
                             (split-into-lines (fortune/get-next-fortune))))
                       "\n;;\n"))))
    (print '(require 'persistent-sessions) (current-buffer))
    (let* ((buffers (buffer-list))
           (buffer-data
            (map (lambda (buf)
                   (with-current-buffer buf
                     (make-session-entry
                      (abbreviate-file-name (buffer-file-name buf))
                      (point)
                      (sessions/get-buffer-variables buf)
                      major-mode
                      nil)))
                 (filter (comp #'not #'null? #'buffer-file-name)
                         buffers)))
           (temporary-buffer-data
            (map (lambda (buf)
                   (with-current-buffer buf
                     (make-session-entry
                      (buffer-name buf)
                      (point)
                      (sessions/get-buffer-variables buf)
                      major-mode
                      (buffer-substring-no-properties (point-min) (point-max)))))
                 (filter (lambda (buf)
                           (with-current-buffer buf
                             (and (null? (buffer-file-name buf))
                                  (not (memq major-mode
                                             sessions/ignored-temporary-buffers))
                                  (not (assq major-mode
                                             sessions/special-modes))
                                  (not (string-match-pure? "\\(?:^ \\)\\|\\(?:^\\*.*\\*$\\)"
                                                           (buffer-name buf))))))
                         buffers)))
           (special-buffer-data
            (remq nil
                  (map (lambda (buf)
                         (with-current-buffer buf
                           (when-let* (spec-entry
                                       (assq major-mode
                                             sessions/special-modes)
                                       save-func (cadr-safe (assq 'save spec-entry)))
                             (list major-mode
                                   (buffer-name buf)
                                   (funcall save-func buf)))))
                       buffers)))
           (frame-data
            (revive-plus:window-configuration-printable)))
      (insert "(sessions/load-from-data\n")
      (insert "  '(\n")
      (dolist (x (list (list 'buffers buffer-data)
                       (list 'temporary-buffers temporary-buffer-data)
                       (list 'special-buffers special-buffer-data)
                       (list 'frames frame-data)
                       (list 'global-variables (sessions/get-global-variables))))
        (print x (current-buffer))
        (insert "\n"))
      (insert "))\n")
      (insert "\n\n;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; coding: utf-8
;; mode: emacs-lisp
;; End:")

      (write-region (point-min) (point-max) file)
      (make-file-executable file))))

(defun sessions/call-symbol-function (sym)
  "Call function bound to symbol SYM, autoloading it if necessary."
  (let ((func (symbol-function sym)))
    (when (autoloadp func)
      (autoload-do-load func))
    (funcall (symbol-function sym))))

(defun sessions/load-from-data (session-entries)
  "Load session from DATA."
  (let ((setup-buffer
         (lambda (point mode vars)
           (setf mode (or (and (fboundp mode)
                               mode)
                          default-major-mode))
           (goto-char point)
           (unless (eq? major-mode mode)
             (sessions/call-symbol-function mode))
           (sessions/restore-buffer-variables (current-buffer) vars))))
    (awhen (assq 'buffers session-entries)
      (mapc (lambda (entry)
              (if (file-exists? (session-entry/buffer-name entry))
                (with-current-buffer (find-file-noselect
                                      (session-entry/buffer-name entry))
                  (funcall setup-buffer
                           (session-entry/point entry)
                           (session-entry/major-mode entry)
                           (session-entry/variables entry)))
                (message "warning: file %s does not exist!"
                         (session-entry/buffer-name entry))))
            (cadr it)))
    (awhen (assq 'temporary-buffers session-entries)
      (mapc (lambda (entry)
              (let ((buf
                     (get-buffer-create
                      (session-entry/buffer-name entry))))
                (with-current-buffer buf
                  (insert (session-entry/other-data entry))
                  (funcall setup-buffer
                           (session-entry/point entry)
                           (session-entry/major-mode entry)
                           (session-entry/variables entry)))))
            (cadr it)))
    (aif (assq 'special-buffers session-entries)
      (map (lambda (saved-info)
             (let ((mmode (first saved-info))
                   (buffer-name (second saved-info))
                   (special-data (third saved-info)))
               (when-let* (spec-entry
                           (assq mmode sessions/special-modes)
                           restore-func (cadr-safe (assq 'restore spec-entry)))
                 (funcall restore-func buffer-name special-data))))
           (cadr it))
      (message "warning: session-entries without special buffer information"))
    (aif (assq 'frames session-entries)
      (revive-plus:restore-window-configuration (first (rest it)))
      (message "warning: session-entries without frame information"))
    (aif (assq 'global-variables session-entries)
      (sessions/restore-global-variables (first (rest it)))
      (message "warning: session-entries without global variables information"))))

(defvar sessions/load-buffers-hook nil
  "Hook run after restoring session in `sessions/load-buffers'.")

(defun sessions/load-buffers (file)
  "Load session from FILE's contents."
  (interactive "ffile to load session from: ")
  (if (file-exists? file)
    (progn
      (sessions/load-from-data
       (with-temp-buffer
         (insert-file-contents-literally file)
         (read (buffer-substring-no-properties (point-min) (point-max)))
         ;; (read (current-buffer))
         ))
      (run-hooks 'sessions/load-buffers-hook))
    (message "warning: file %s does not exist" file)))

(provide 'persistent-sessions)

;; local Variables:
;; End:

;; persistent-sessions.el ends here
