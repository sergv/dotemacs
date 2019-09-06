;; java-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  1 January 2017
;; Description:

(require 'common)

(defun java-print-info-template ()
  "Insert call to printf statement to print some variables and messages
while interactively prompting for variables/messages."
  (interactive)
  (let* ((start
          (lambda ()
            (insert "Utils.d(\"")))
         (end
          (lambda ()
            (insert ");")))
         (quote-input
          (lambda (x)
            (replace-regexp-in-string "\"" "\\\"" x)))
         (insert-continuation
          (lambda (should-merge-messages?)
            (if should-merge-messages?
                (delete-backward-char 1)
              (insert " + \""))))
         (insert-message
          (lambda (is-initial-insertion? user-input)
            (insert (format "%s\"" (funcall quote-input user-input)))))
         (insert-variable
          (lambda (is-initial-insertion? user-input)
            (insert
             (format "%s%s = \" + %s"
                     (if is-initial-insertion? "" ", ")
                     (funcall quote-input user-input)
                     (if (string-match-p "[ \t]" user-input)
                         (concat "(" user-input ")")
                       user-input))))))
    (insert-info-template
     :start start
     :end end
     :insert-continuation insert-continuation
     :insert-message insert-message
     :insert-variable insert-variable)))

(defun java-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w" "w_" "^ >")
        abbrev+-abbreviations
        (list
         (make-abbrev+-abbreviation
          :trigger "Utils\\.d"
          :action-type 'function-with-side-effects
          :action-data #'java-print-info-template
          :predicate #'point-not-inside-string-or-comment?)))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'java-abbrev+)

;; Local Variables:
;; End:

;; java-abbrev+.el ends here
