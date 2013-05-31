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


(defvar *sessions-buffer-variables* nil
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



(defvar *sessions-global-variables* '(log-edit-comment-ring
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
      (else
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



(defun sessions/save-buffers (file)
  "Save all buffers that have physical file assigned into FILE."
  (interactive "Ffile to save session in: ")
  (with-temp-buffer
    (insert ":;exec emacs --load \"$0\"\n\n")
    (insert ";:;exec gdb -ex \"run\" --args emacs --load \"$0\"\n\n")
    (insert (format ";; this session was created on %s\n;; Today's quote is\n%s\n"
                    (format-time-string "%A, %e %B %Y")
                    (join-lines
                     (map (lambda (x) (concat ";; " x))
                          (split-into-lines (fortune *fortunes*))))))
    (print '(require 'persistent-sessions) (current-buffer))
    (let ((buffer-data
           (remq nil
                 (map (lambda (buf)
                        (when (buffer-file-name buf)
                          (with-current-buffer buf
                            (make-session-entry
                             (abbreviate-file-name (buffer-file-name buf))
                             (point)
                             (sessions/get-buffer-variables buf)
                             major-mode))))
                      (buffer-list))))
          (frame-data
           (window-configuration-printable)))
      (pp
       (list 'sessions/load-from-data
             (list 'quote
                   (list (list 'buffers
                               buffer-data)
                         (list 'frames
                               frame-data)
                         (list 'global-variables
                               (sessions/get-global-variables)))))
       (current-buffer)))
    (write-region (point-min) (point-max) file)
    (make-file-executable file)))

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
                      (funcall (symbol-function it)))
                    (message "warning: session buffer entry without major mode: %s"
                             entry))
                  (sessions/restore-buffer-variables (current-buffer)
                                                     (session-entry/variables entry)))
                (message "warning: file %s does not exist!"
                         (session-entry/file-name entry))))
            (cadr it)))
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
