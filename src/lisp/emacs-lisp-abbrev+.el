;; emacs-lisp-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 19 December 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'macro-util))

(require 'general-lisp-setup)
(require 'abbrev+)
(require 'common)

(defun emacs-lisp-print-info-template ()
  (interactive)
  (let ((start
         (lambda ()
           (insert "(message \"")))
        (end
         (lambda (var-list)
           (insert "\"\n")
           (insert (join-lines (--map (concat "(pp-to-string " it ")")
                                      var-list)))
           (insert ")")))
        (format
         (lambda (user-input) (insert user-input " = %s"))))
    (insert-info-format-template
     :start start
     :end end
     :format format
     :reindent-at-end #'prog-indent-sexp)))

(defun-once emacs-lisp-abbrev+-make-abbrevs
  (vector
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("beginning" 3)
                  ("of"        1)
                  ("line"      1))))
    :action-type 'literal-string
    :action-data "(beginning-of-line")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("buffer"    3)
                  ("substring" 3))))
    :action-type 'literal-string
    :action-data "(buffer-substring")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("buffer"     3)
                  ("substring"  3)
                  ("no"         1)
                  ("properties" 0))))
    :action-type 'literal-string
    :action-data "(buffer-substring-no-properties")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("def"      1)
                  ("keys"     1)
                  ("for"      1)
                  ("map"      1))))
    :action-type 'literal-string
    :action-data "(def-keys-for-map")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("multiple" 1)
                  ("value"    1)
                  ("bind"     1))))
    :action-type 'literal-string
    :action-data "(multiple-value-bind")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("destructuring" 3)
                  ("bind"          1))))
    :action-type 'literal-string
    :action-data "(destructuring-bind")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("lambda" 3))))
    :action-type 'literal-string
    :action-data "(lambda")
   (make-abbrev+-abbreviation
    :trigger "(\\(?:mess?\\|msg\\)\\_>"
    :action-type 'literal-string
    :action-data "(message")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("condition" 2)
                  ("case"      3))))
    :action-type 'literal-string
    :action-data "(condition-case")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("point" 1)
                  ("min"   2))))
    :action-type 'literal-string
    :action-data "(point-min")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("point" 1)
                  ("max"   2))))
    :action-type 'literal-string
    :action-data "(point-max")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("unwind"  2)
                  ("protect" 2))))
    :action-type 'literal-string
    :action-data "(unwind-protect")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("beginning"  2)
                  ("of"         1)
                  ("line"       1))))
    :action-type 'literal-string
    :action-data "(beginning-of-line")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("end"  2)
                  ("of"   1)
                  ("line" 1))))
    :action-type 'literal-string
    :action-data "(end-of-line")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("line"      1)
                  ("beginning" 1)
                  ("position"  3))))
    :action-type 'literal-string
    :action-data "(line-beginning-position")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("line"     1)
                  ("end"      1)
                  ("position" 3))))
    :action-type 'literal-string
    :action-data "(line-end-position")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("with"     1)
                  ("current"  1)
                  ("buffer"   1))))
    :action-type 'literal-string
    :action-data "(with-current-buffer")
   (make-abbrev+-abbreviation
    :trigger (eval-when-compile
               (make-abbrev+-re-for-lisp-func-name
                '(("with"   1)
                  ("temp"   1)
                  ("buffer" 1))))
    :action-type 'literal-string
    :action-data "(with-temp-buffer")

   (make-abbrev+-abbreviation
    :trigger "\\_<info\\_>"
    :action-type 'function-with-side-effects
    :action-data #'emacs-lisp-print-info-template
    :predicate (lambda () (and (not (lisp-point-inside-string-or-comment?))
                               (not (lisp-prev-pos-is-beginning-of-list? (point))))))))

(defun emacs-lisp-abbrev+-setup ()
  (setf abbrev+-skip-syntax ["w_"
                             "w_("
                             ;;"^ >"
                             ]
        abbrev+-abbreviations (emacs-lisp-abbrev+-make-abbrevs))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))


(provide 'emacs-lisp-abbrev+)

;; Local Variables:
;; End:

;; emacs-lisp-abbrev+.el ends here
