;; isabelle-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 31 January 2025
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'abbrev+)

(defun-once isabelle-abbrev+-make-abbrevs
  (abbrev+-compile-abbreviations
   (--map
    (cons (list (car it))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data (cdr it)))
    '(("->"   . "\\<rightarrow>")
      ("=>"   . "\\<Rightarrow>")
      ("-->"  . "\\<longrightarrow>")
      ("==>"  . "\\<Longrightarrow>")
      ("<->"  . "\\<leftrightarrow>")
      ("<=>"  . "\\<Leftrightarrow>")
      ("<-->" . "\\<longleftrightarrow>")
      ("<==>" . "\\<Longleftrightarrow>")
      ("!"    . "\\<forall>")
      ("?"    . "\\<exists>")
      ("~"    . "\\<not>")
      ("&"    . "\\<and>")
      ("|"    . "\\<or>")
      ("=="   . "\\<equiv>")))))

(defun isabelle-abbrev+-setup ()
  (setf abbrev+-abbreviations (isabelle-abbrev+-make-abbrevs)
        ;; Many, if not most, things happen within strings in Isabelle so
        ;; enable abbrevs there.
        abbrev+-do-not-expand-predicate #'point-inside-comment?)
  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'isabelle-abbrev+)

;; Local Variables:
;; End:

;; isabelle-abbrev+.el ends here
