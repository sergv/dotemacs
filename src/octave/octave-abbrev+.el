;; octave-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 11 April 2012
;; Description:

(define-print-info-skeleton
    octave-debug-message-skeleton
  :doc "Insert call to printf statement to print some variables and messages
while interactively prompting for variables/messages."
  :print-begin "printf("
  :print-end ");"

  :indent-after-func nil
  :insert-newline-before-var-list nil
  :msg-transform #'upcase

  :format-print-value "%s"
  :format-string-start "\""
  :format-string-end "\\n\""

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
                (when (looking-at
                       (rxx ((name (regex "[a-zA-Z_][a-zA-Z0-9_]*"))
                             (arg-name name))
                         "function"
                         (??
                          (* whitespace)
                          arg-name
                          (* ","
                             (* whitespace)
                             arg-name)
                          (* whitespace)
                          "=")
                         (* whitespace)
                         (group
                          name)
                         (* whitespace)
                         "("))
                  (concat
                   (upcase (match-string 1))
                   ": ")))
            (error ""))))))

  :make-variable-list (lambda (list)
                        (if (< 0 (length list))
                          (concat ", "
                                  (mapconcat (lambda (x)
                                               (concat "num2str(" x ")"))
                                             list
                                             ", "))
                          "")))

(defun octave-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w" "w_" "^ >")
        abbrev+-abbreviations
        (list
         (list "\\<prf?\\>"
               (list
                (lambda ()
                  (yas-expand-snippet "printf(\"${1}\\n\");")))
               (lambda () (not (point-inside-string-or-comment?))))
         (list "\\<info\\>"
               (list
                #'octave-debug-message-skeleton)
               (lambda () (not (point-inside-string-or-comment?))))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))


(provide 'octave-abbrev+)

;; Local Variables:
;; End:

;; octave-abbrev+.el ends here
