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
(require 'persistent-sessions-serializers)
(require 'persistent-sessions-error-reporting)

(setf revive-plus:all-frames t
      revive:save-variables-mode-local-private
      '((c++-mode c-indentation-style c-basic-offset)))

;; nil - no 'version field in session data structure;
;;     - encode strings and rings as-is via prin1
;; 2   - add 'version field to session data structure;
;;     - encode strings and ring contents with explicit property list
(defconst +sessions-schema-version+ 2)

(defsubst make-session-entry (buf-name point variables major-mode other-data special-variables)
  "BUF-NAME         - buffer name
POINT             - position within buffer
VARIABLES         - values of local variables
MAJOR-MODE        - buffer's mode
OTHER-DATA        - some data, depending on buffer type
SPECIAL-VARIABLES - local variables that may require special treatment when restoring"
  (list buf-name
        point
        variables
        major-mode
        other-data
        special-variables))
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
(defsubst session-entry/special-variables (entry)
  (first-safe (rest-safe (rest-safe (rest-safe (rest-safe (rest-safe entry)))))))


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
    (-mapcat (lambda (entry)
               (let ((pred (car entry))
                     (vars (cdr entry)))
                 (when (funcall pred buffer)
                   (-map (lambda (var)
                           (when (and (boundp var)
                                      (local-variable? var))
                             (cons var
                                   (sessions/store-value
                                    (symbol-value var)))))
                         vars))))
             *sessions-buffer-variables*)))

(defun sessions/restore-buffer-variables (version buffer bindings)
  "Restore variables captured in BINDINGS for buffer BUFFER."
  (with-current-buffer buffer
    (dolist (entry *sessions-buffer-variables*)
      (let ((pred (car entry))
            (vars (cdr entry)))
        (when (funcall pred buffer)
          (dolist (bind bindings)
            (let ((var (car bind)))
              (when (memq var vars)
                (set var (sessions/versioned/restore-value version (cdr bind)))))))))))


(defparameter *sessions-special-variables*
  `((structured-haskell-mode
     ,(lambda (buffer)
        (with-current-buffer buffer
          (and (boundp 'structured-haskell-mode)
               structured-haskell-mode)))
     ,(lambda (buffer value)
        (with-current-buffer buffer
          (structured-haskell-mode
           (if value +1 -1)))))))

(defun sessions/get-special-buffer-variables (buffer)
  (-map (lambda (entry)
          (pcase entry
            (`(,var-name ,get-value ,_)
             (list var-name
                   (funcall get-value buffer)))
            (_
             (error "Invalid special variable entry: %s" entry))))
        *sessions-special-variables*))

(defun sessions/restore-special-buffer-variables (buffer values)
  (dolist (entry values)
    (pcase entry
      (`(,var-name ,value)
       (let ((set-value (caddr (assq var-name *sessions-special-variables*))))
         (if set-value
           (if (functionp set-value)
             (funcall set-value buffer value)
             (error "Error: found non-function set-value entry in *sessions-special-variables* for key %s"
                    set-value))
           (message "*sessions-special-variables*: warning: cannot find setter for special variable %s"
                    var-name))))
      (_
       (message "*sessions-special-variables*: warning: invalid special variables entry: %s"
                entry)))))


(defparameter *sessions-global-variables* '(log-edit-comment-ring
                                            vim:ex-history
                                            read-expression-history
                                            *search-minibuffer-history*)
  "List of global variables to save in session file.")

(defun sessions/get-global-variables ()
  "Get global variables that should be saved in form of sequence of (var . value)
entries."
  (remq nil
        (-map (lambda (var)
                (when (boundp var)
                  (cons var
                        (sessions/store-value
                         (sessions/truncate-long-sequences
                          (symbol-value var))))))
              *sessions-global-variables*)))

(defun sessions/restore-global-variables (version bindings)
  "Restore global variables from BINDINGS."
  (dolist (bind bindings)
    (set (car bind)
         (sessions/versioned/restore-value version (cdr bind)))))


(defparameter sessions/ignored-temporary-buffer-modes
  '(dired-mode
    magit-diff-mode
    magit-revision-mode
    magit-mode
    magit-popup-mode
    magit-stashes-mode
    magit-stash-mode
    magit-status-mode
    magit-refs-mode
    magit-merge-preview-mode
    magit-process-mode
    magit-rebase-mode
    magit-log-mode
    magit-log-select-mode
    magit-cherry-mode
    magit-reflog-mode)
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
                             (sessions/store-string
                              (buffer-substring (point-min) (point-max))))
                       (list 'current-dir
                             (sessions/store-string
                              (expand-file-name default-directory)))
                       (list 'comint-input-ring
                             (sessions/store-ring comint-input-ring)))))))
     (restore ,(lambda (version buffer-name saved-data)
                 (let ((buf (get-buffer-create buffer-name)))
                   (sessions/report-and-ignore-asserts
                     (aif (assq 'contents saved-data)
                       (with-current-buffer buf
                         (with-inhibited-read-only
                          (with-inhibited-modification-hooks
                           (with-inhibited-redisplay
                             (goto-char (point-min))
                             (insert (sessions/versioned/restore-string version (second it)))
                             (insert "\n\n")))))
                       (message "shell-restore: no 'contents")))
                   (shell buf)
                   (accept-process-output (get-buffer-process buf)
                                          5 ;; Time to wait in seconds.
                                          )
                   (sessions/report-and-ignore-asserts
                     (let ((current-dir
                            (cadr-safe
                             (assq 'current-dir saved-data))))
                       (aif current-dir
                         (progn
                           (goto-char (point-max))
                           (insert "cd \""
                                   (sessions/versioned/restore-string version current-dir)
                                   "\"")
                           (comint-send-input))
                         (message "shell-restore: no 'current-dir"))))
                   (sessions/report-and-ignore-asserts
                     (aif (cadr-safe (assq 'comint-input-ring saved-data))
                       (setf comint-input-ring
                             (sessions/versioned/restore-ring version it))
                       (message "shell-restore: no 'comint-input-ring")))))))))

(defun sessions/is-temporary-buffer? (buf)
  (with-current-buffer buf
    (and (null? (buffer-file-name buf))
         (not (memq major-mode
                    sessions/ignored-temporary-buffer-modes))
         (not (assq major-mode
                    sessions/special-modes))
         (not (string-match-pure? "\\(?:^ \\)\\|\\(?:^\\*.*\\*$\\)"
                                  (buffer-name buf))))))


(defun sessions/save-buffers/make-session ()
  "Make session to save later"
  (let* ((buffers (buffer-list))
         (buffer-data
          (-map (lambda (buf)
                  (with-current-buffer buf
                    (make-session-entry
                     (sessions/store-string (abbreviate-file-name buffer-file-name))
                     (point)
                     (sessions/get-buffer-variables buf)
                     major-mode
                     nil
                     (sessions/get-special-buffer-variables buf))))
                (--filter (not (null? (buffer-file-name it)))
                          buffers)))
         (temporary-buffer-data
          (-map (lambda (buf)
                  (with-current-buffer buf
                    (make-session-entry
                     (sessions/store-string (buffer-name buf))
                     (point)
                     (sessions/get-buffer-variables buf)
                     major-mode
                     (sessions/store-string
                      (buffer-substring (point-min) (point-max)))
                     (sessions/get-special-buffer-variables buf))))
                (-filter #'sessions/is-temporary-buffer? buffers)))
         (special-buffer-data
          (remq nil
                (-map (lambda (buf)
                        (with-current-buffer buf
                          (when-let* (spec-entry
                                      (assq major-mode
                                            sessions/special-modes)
                                      save-func (cadr-safe (assq 'save spec-entry)))
                            (list major-mode
                                  (sessions/store-string
                                   (buffer-name buf))
                                  (funcall save-func buf)))))
                      buffers)))
         (frame-data
          (revive-plus:window-configuration-printable)))
    (list (list 'schema-version +sessions-schema-version+)
          (list 'buffers buffer-data)
          (list 'temporary-buffers temporary-buffer-data)
          (list 'special-buffers special-buffer-data)
          (list 'frames frame-data)
          (list 'global-variables (sessions/get-global-variables)))))

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
                        (-map (lambda (x) (concat ";; " x))
                              (split-into-lines (fortune/get-next-fortune))))
                       "\n;;\n"))))
    (print '(require 'persistent-sessions) (current-buffer))
    (let ((session (sessions/save-buffers/make-session)))
      (insert "(sessions/load-from-data\n")
      (insert "'(\n")
      (dolist (entry session)
        (cond
          ((eq (car-safe entry) 'buffers)
           (insert "(buffers")
           (insert "(")
           (dolist (buf-entry (cadr entry))
             (print buf-entry (current-buffer)))
           (insert "))\n"))
          (t
           (print entry (current-buffer))
           (insert "\n"))))
      (insert "))\n")
      (insert "\n\n;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; coding: utf-8
;; mode: emacs-lisp
;; End:")
      (write-region (point-min) (point-max) file)
      (make-file-executable file))))

(defun sessions/load-from-data (session-entries)
  "Load session from DATA."
  (let* ((version
          (cadr-safe
           (assq 'schema-version session-entries)))
         (setup-buffer
          (lambda (point mode vars special-vars)
            (sessions/report-and-ignore-asserts
              (sessions/assert-with-args (symbolp mode)
                                         "Invalid mode: %s"
                                         mode)
              (setf mode (or (and (fboundp mode)
                                  mode)
                             default-major-mode))
              (unless (eq? major-mode mode)
                (sessions/call-symbol-function mode)))
            (sessions/report-and-ignore-asserts
              (sessions/assert-with-args (numberp point)
                                         "Invalid point: %s"
                                         point)
              (goto-char point))
            (sessions/report-and-ignore-asserts
              (sessions/restore-buffer-variables version (current-buffer) vars))
            (sessions/report-and-ignore-asserts
              (sessions/restore-special-buffer-variables (current-buffer) special-vars)))))
    (sessions/report-and-ignore-asserts
      (sessions/assert-with-args
       (or (null version)
           (numberp version))
       "Invalid version: %s"
       version))
    (aif (assq 'buffers session-entries)
      (mapc (lambda (entry)
              (sessions/report-and-ignore-asserts
                (let ((buf-name
                       (sessions/versioned/restore-string
                        version
                        (session-entry/buffer-name entry))))
                  (sessions/assert-with-args (file-exists? buf-name)
                                             "File %s does not exist!"
                                             buf-name)
                  (with-current-buffer (find-file-noselect buf-name)
                    (funcall setup-buffer
                             (session-entry/point entry)
                             (session-entry/major-mode entry)
                             (session-entry/variables entry)
                             (session-entry/special-variables entry))))))
            (cadr it))
      (message "sessions/load-from-data: no 'buffers field"))
    (aif (assq 'temporary-buffers session-entries)
      (mapc (lambda (entry)
              (sessions/report-and-ignore-asserts
                (unless (memq (session-entry/major-mode entry) sessions/ignored-temporary-buffer-modes)
                  (let ((buf (get-buffer-create
                              (sessions/versioned/restore-string
                               version
                               (session-entry/buffer-name entry)))))
                    (with-current-buffer buf
                      (insert
                       (sessions/versioned/restore-string
                        version
                        (session-entry/other-data entry)))
                      (funcall setup-buffer
                               (session-entry/point entry)
                               (session-entry/major-mode entry)
                               (session-entry/variables entry)
                               (session-entry/special-variables entry)))))))
            (cadr it))
      (message "sessions/load-from-data: no 'temporary-buffers field"))
    (aif (assq 'special-buffers session-entries)
      (-map (lambda (saved-info)
              (let ((mmode (first saved-info))
                    (buffer-name
                     (sessions/versioned/restore-string
                      version
                      (second saved-info)))
                    (special-data (third saved-info)))
                (when-let* (spec-entry   (assq mmode sessions/special-modes)
                                         restore-func (cadr-safe (assq 'restore spec-entry)))
                  (funcall restore-func version buffer-name special-data))))
            (cadr it))
      (message "sessions/load-from-data: no 'special-buffers field"))
    (aif (assq 'frames session-entries)
      (revive-plus:restore-window-configuration (cadr it))
      (message "sessions/load-from-data: no 'frames field"))
    (aif (assq 'global-variables session-entries)
      (sessions/restore-global-variables version (cadr it))
      (message "sessions/load-from-data: no 'global-variables field"))))

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

;;;; Utils

(defun sessions/call-symbol-function (sym)
  "Call function bound to symbol SYM, autoloading it if necessary."
  (let ((func (symbol-function sym)))
    (when (autoloadp func)
      (autoload-do-load func))
    (funcall (symbol-function sym))))

(defun sessions/strip-text-properties (val)
  (cond
    ((ring-p val)
     (let ((result (make-ring (ring-size val))))
       (dotimes (n (ring-length val))
         (ring-insert result
                      (sessions/strip-text-properties
                       (ring-ref val n))))
       result))
    ((stringp val)
     (substring-no-properties val))
    (t
     val)))

(defun sessions/truncate-long-sequences (val)
  "Truncate overly long sequences."
  (let ((max-size 2000))
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

(provide 'persistent-sessions)

;; local Variables:
;; End:

;; persistent-sessions.el ends here
