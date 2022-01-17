;; c-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 15 September 2012
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)

(defun-once c-abbrev+-make-abbrevs
  (abbrev+-compile-abbreviations
   (list
    (cons (list "pr"
                "pri"
                "prf"
                "prif"
                "print"
                "printf")
          (make-abbrev+-abbreviation
           :action-type 'yas-snippet
           :action-data "printf(\"$1\\n\"$2);$0")))))

(defun c-abbrev+-setup ()
  (setf abbrev+-abbreviations (c-abbrev+-make-abbrevs)
        abbrev+-do-not-expand-predicate #'point-inside-string-or-comment?)

  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'c-abbrev+)

;; Local Variables:
;; End:

;; c-abbrev+.el ends here
