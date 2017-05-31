;; c-abbrev+.el --- -*- lexical-binding: t; -*-

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
         (make-abbrev+-abbreviation
          :trigger "\\<pr\\(?:i\\(?:nt?\\)?\\)?f?\\>"
          :action-type 'yas-snippet
          :action-data "printf(\"$1\\n\"$2);$0"
          :predicate #'point-not-inside-string-or-comment?)))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'c-abbrev+)

;; Local Variables:
;; End:

;; c-abbrev+.el ends here
