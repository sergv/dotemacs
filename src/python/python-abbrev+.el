;; python-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 January 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'common)

(define-print-info-skeleton
    python-debug-message-skeleton
  :doc "Insert call to print statement to print some variables and messages while
interactively prompting for variables/messages."
  :print-begin "print("
  :print-end ")"

  :indent-after-func nil
  :insert-newline-before-var-list nil
  :msg-transform nil

  :format-print-value "{}"
  :format-string-start "\""
  :format-string-end "\""

  :insert-entity-name-procedure
  (lambda (beginning)
    (save-excursion
     (save-match-data
      (goto-char beginning)
      (if (= 0 (current-column))
        ""
        (condition-case nil
            (progn
              (beginning-of-defun)
              (when (looking-at "def[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)")
                (concat
                 (upcase (match-string 1))
                 ": ")))
          (error ""))))))

  :make-variable-list
  (lambda (list)
    (concat ".format("
            (join-lines list ", ")
            ")")))



(defun python-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w" "w_" "^ >")
        abbrev+-abbreviations
        (list
         (list "\\."
               ;; this enables space to be omitted
               (list
                (lambda () (insert "self.")))
               (lambda () (not (point-inside-string-or-comment?))))
         (list "\\<pr\\(?:i\\(?:nt?\\)?\\)?\\>"
               (list
                (lambda ()
                  (yas/expand-snippet "print(\"$1\")$0")))
               (lambda () (not (point-inside-string-or-comment?))))
         (list "\\<pr\\(?:i\\(?:nt?\\)?\\)?f\\>"
               (list
                (lambda ()
                  (yas/expand-snippet "print(\"$1\".format($2))$0")))
               (lambda () (not (point-inside-string-or-comment?))))
         (list "\\<info\\>"
               (list
                #'python-debug-message-skeleton)
               (lambda () (not (point-inside-string-or-comment?))))
         ;; print_function
         (list "\\<pr\\(?:i\\(?:nt\\)?\\)?_f\\(?:u\\(?:n\\(?:c\\(?:t\\(?:i\\(?:on?\\)?\\)?\\)?\\)?\\)?\\)?\\>"
               "from __future__ import print_function"
               (lambda () (not (point-inside-string-or-comment?))))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))


(provide 'python-abbrev+)

;; Local Variables:
;; End:

;; python-abbrev+.el ends here
