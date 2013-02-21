;; persistent-sessions.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 23 January 2013
;; Description:

(eval-when-compile (require 'cl))

(require 'revive-setup)

(defalias 'revive-plus:window-configuration-printable #'window-configuration-printable)
(defalias 'revive-plus:restore-window-configuration #'restore-window-configuration)


(defun make-session-entry (file-name point variables)
  (list file-name point variables))
(defun session-entry/file-name (entry)
  (car entry))
(defun session-entry/point (entry)
  (cadr entry))
(defun session-entry/variables (entry)
  (caddr entry))

(setq-default test nil)
(symbol-value 'test)

(defvar *sessions-buffer-variables* nil
  "List of buffer-local variables to save in session file.")

(defun sessions/get-buffer-variables (buffer)
  "Get buffer's local variables that should be saved."
  (with-current-buffer buffer
    (mapcar (lambda (var)
              (when (local-variable? var)
                ;; (assert (local-variable? var)
                ;;         t
                ;;         "Non-local variable %s in buffer \"%s\""
                ;;         var
                ;;         buffer)
                (list var (symbol-value var))))
            *sessions-buffer-variables*)))

(defun sessions/restore-buffer-variables (buffer bindings)
  "Get buffer's local variables that should be saved."
  (with-current-buffer buffer
    (dolist (bind bindings)
      (destructuring-bind (variable value) bind
        (set variable value)))))



(defun sessions/save-buffers (file)
  "Save all buffers that have physical file assigned into FILE."
  (interactive)
  (with-temp-buffer
    (print ;; pp
     (list (list 'buffers
                 (remq nil
                       (mapcar (lambda (buf)
                                 (when (buffer-file-name buf)
                                   (make-session-entry
                                    (buffer-file-name buf)
                                    (with-current-buffer buf
                                      (point))
                                    (sessions/get-buffer-variables buf))))
                               (buffer-list))))
           (list 'frames
                 (window-configuration-printable)))
     (current-buffer))
    (write-region (point-min) (point-max) file)))

(defun sessions/load-buffers (file)
  (interactive)
  (if (file-exists? file)
    (let ((session-entries
            (with-temp-buffer
              (insert-file-contents-literally file)
              (read (buffer-substring-no-properties (point-min) (point-max))))))
      (aif (assq 'buffers session-entries)
        (mapc (lambda (entry)
                (with-current-buffer (find-file-noselect
                                      (session-entry/file-name entry))
                  (goto-char (session-entry/point entry))
                  (sessions/restore-buffer-variables (current-buffer)
                                                     (session-entry/variables entry))))
              (cadr it)))
      (aif (assq 'frames session-entries)
        (restore-window-configuration (cadr it))))
    (message "warning: file %s does not exist" file)))


(provide 'persistent-sessions)

;; local Variables:
;; End:

;; persistent-sessions.el ends here
