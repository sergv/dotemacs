;; kibit.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 19 April 2013
;; Description:

;; Teach compile the syntax of the kibit output
(require 'compile)

(defconst +kibit-error-regexp+ "At \\([^:]+\\):\\([[:digit:]]+\\):")
(defconst +kibit-check-buffer-name+ "*kibit-check*")

(define-compilation-mode kibit-check-mode "Kibit check"
  "Mode for checking Clojure source with Kibit tool."
  (setq-local compilation-error-regexp-alist
              (list
               (list +kibit-error-regexp+
                     1 ;; file-group
                     2 ;; line-group
                     nil ;; column-group
                     0   ;; type - 0 - info
                     )))

  (set (make-local-variable '*compilation-jump-error-regexp*)
       +kibit-error-regexp+)

  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil))

(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compilation-start "lein kibit"
                     #'kibit-check-mode
                     (lambda (_)
                       +kibit-check-buffer-name+)))

(defun kibit-current-file ()
  "Run kibit on the current file.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compilation-start (format "lein kibit %s"
                             (shell-quote-argument buffer-file-name))
                     #'kibit-check-mode
                     (lambda (_)
                       +kibit-check-buffer-name+)))


(provide 'kibit)

;; Local Variables:
;; End:

;; kibit.el ends here
