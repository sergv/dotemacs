;; persistent-sessions.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 23 January 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'more-clojure)
(require 'fortunes)
(require 'revive-setup)

(defalias 'revive-plus:window-configuration-printable #'window-configuration-printable)
(defalias 'revive-plus:restore-window-configuration #'restore-window-configuration)


(defun make-session-entry (file-name point variables major-mode)
  (list file-name point variables major-mode))
(defun session-entry/file-name (entry)
  (first-safe entry))
(defun session-entry/point (entry)
  (first-safe (rest-safe entry)))
(defun session-entry/variables (entry)
  (first-safe (rest-safe (rest-safe entry))))
;; Major mode is remembered because there're cases when emacs infers wrong
;; mode for otherwise normal buffer. Also modes may be changed by hand, so
;; it is preserved for every buffer.
(defun session-entry/major-mode (entry)
  (first-safe (rest-safe (rest-safe (rest-safe entry)))))


(defparameter *sessions-buffer-variables* nil
  "List of buffer-local variables to save in session file.")

(defun sessions/get-buffer-variables (buffer)
  "Get buffer's local variables that should be saved."
  (with-current-buffer buffer
    (map (lambda (var)
           (when (local-variable? var)
             ;; (assert (local-variable? var)
             ;;         t
             ;;         "Non-local variable %s in buffer \"%s\""
             ;;         var
             ;;         buffer)
             (list var (symbol-value var))))
         *sessions-buffer-variables*)))

(defun sessions/restore-buffer-variables (buffer bindings)
  "Restore variables captured in BINDINGS for buffer BUFFER."
  (with-current-buffer buffer
    (dolist (bind bindings)
      (destructuring-bind (variable value) bind
        (set variable value)))))



(defparameter *sessions-global-variables* '(log-edit-comment-ring
                                      vim:ex-history
                                      read-expression-history
                                      eshell-history-ring
                                      *search-minibuffer-history*)
  "List of global variables to save in session file.")

(defun sessions/truncate-long-sequences (val)
  "Truncate overly long sequences."
  (let ((max-size 500))
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
                 (cons var (sessions/truncate-long-sequences (symbol-value var)))))
             *sessions-global-variables*)))

(defun sessions/restore-global-variables (bindings)
  "Restore global variables from BINDINGS."
  (dolist (bind bindings)
    (destructuring-bind (var . value) bind
      (set var value))))

(defparameter sessions/special-modes
  `((eshell-mode
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
                       (list 'eshell-history-ring
                             eshell-history-ring))))))
     (restore ,(lambda (buffer-name saved-data)
                 (message "Restoring eshell buffer %s" buffer-name)
                 (when-let (contents
                            (assoc 'contents saved-data))
                   (save-excursion
                     (require 'eshell)
                     (let ((eshell-buffer-name buffer-name))
                       (eshell)
                       (with-inhibited-read-only
                        (forward-line -1)
                        (delete-region (point-min)
                                       ;; do not capture trailing \n
                                       (line-end-position)))
                       (save-excursion
                         (with-inhibited-read-only
                          (with-inhibited-modification-hooks
                           (with-inhibited-redisplay
                             (goto-char (point-min))
                             (insert (second contents))))))
                       (when-let (current-dir
                                  (cadr-safe
                                   (assoc 'current-dir saved-data)))
                         (goto-char (point-max))
                         (insert "cd \"" current-dir "\"")
                         (eshell-send-input))
                       (awhen (cadr-safe (assoc 'eshell-history-ring saved-data))
                         (setf eshell-history-ring it))))))))
    (shell-mode
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
                             (split-into-lines (fortune *fortunes*))))
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
                      major-mode)))
                 (filter (comp #'not #'null? #'buffer-file-name)
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
            (window-configuration-printable)))
      (print
       (list 'sessions/load-from-data
             (list 'quote
                   (list (list 'buffers
                               buffer-data)
                         (list 'special-buffers
                               special-buffer-data)
                         (list 'frames
                               frame-data)
                         (list 'global-variables
                               (sessions/get-global-variables)))))
       (current-buffer)))
    (insert "\n\n;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; coding: utf-8
;; mode: emacs-lisp
;; End:")

    (write-region (point-min) (point-max) file)
    (make-file-executable file)))

(defun sessions/call-symbol-function (sym)
  "Call function bound to symbol SYM, autoloading it if necessary."
  (let ((func (symbol-function sym)))
    (when (autoloadp func)
      (autoload-do-load func))
    (funcall (symbol-function sym))))

(defun sessions/load-from-data (data)
  "Load session from DATA."
  (let ((session-entries data))
    (awhen (assq 'buffers session-entries)
      (mapc (lambda (entry)
              (if (file-exists? (session-entry/file-name entry))
                (with-current-buffer (find-file-noselect
                                      (session-entry/file-name entry))
                  (goto-char (session-entry/point entry))
                  (aif (session-entry/major-mode entry)
                    (unless (eq? major-mode it)
                      (sessions/call-symbol-function it))
                    (message "warning: session buffer entry without major mode: %s"
                             entry))
                  (sessions/restore-buffer-variables (current-buffer)
                                                     (session-entry/variables entry)))
                (message "warning: file %s does not exist!"
                         (session-entry/file-name entry))))
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
      (restore-window-configuration (first (rest it)))
      (message "warning: session-entries without frame information"))
    (aif (assq 'global-variables session-entries)
      (sessions/restore-global-variables (first (rest it)))
      (message "warning: session-entries without global variables information"))))

(defun sessions/load-buffers (file)
  "Load session from FILE's contents."
  (interactive "ffile to load session from: ")
  (if (file-exists? file)
    (sessions/load-from-data
     (with-temp-buffer
       (insert-file-contents-literally file)
       (read (buffer-substring-no-properties (point-min) (point-max)))))
    (message "warning: file %s does not exist" file)))


(provide 'persistent-sessions)

;; local Variables:
;; End:

;; persistent-sessions.el ends here
