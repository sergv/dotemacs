;; c-abbrev+.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 15 September 2012
;; Description:

(require 'common)

(defun c-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w" "w_" "^ >")
        abbrev+-abbreviations
        (list
         (list "\\<pr\\(?:i\\(?:nt?\\)?\\)?f?\\>"
               (list
                (lambda ()
                  (yas/expand-snippet "printf(\"$1\\n\")$0")))
               (lambda () (not (point-inside-string-or-comment?))))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'c-abbrev+)

;; Local Variables:
;; lexical-binding: t
;; End:

;; c-abbrev+.el ends here
