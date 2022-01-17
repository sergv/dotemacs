;; shell-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 19 September 2013
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'macro-util)
(require 'abbrev+)

(defun shell-script-info-message-template ()
  "Call echo to show current some messages/values of specified variables."
  (interactive)
  (let ((start
         (lambda ()
           (insert "echo \"")))
        (end
         (lambda (_)
           (insert "\"")))
        (format
         (lambda (user-input)
           (insert user-input " = ${" user-input "}"))))
    (insert-info-format-template
     :start start
     :end end
     :format format)))

(defun-once shell-script-abbrev+-make-abbrevs
  (abbrev+-compile-abbreviations
   (list
    (cons (list "info")
          (make-abbrev+-abbreviation
           :action-type 'function-with-side-effects
           :action-data #'shell-script-info-message-template)))))

(defun shell-script-abbrev+-setup ()
  (setf abbrev+-abbreviations (shell-script-abbrev+-make-abbrevs)
        abbrev+-do-not-expand-predicate #'point-inside-string-or-comment?)
  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'shell-script-abbrev+)

;; Local Variables:
;; End:

;; shell-abbrev+.el ends here
