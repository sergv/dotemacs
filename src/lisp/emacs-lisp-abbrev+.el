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

(defun emacs-lisp-abbrev+-only-when-called (xs)
  "Ensure that lisp functions will get expanded only when they’re
being invoked."
  (--map (concat "(" it) xs))

(defconst emacs-lisp-abbrev+--delims
  ;; "" is for accounting for present and missing ‘-’ separator of name parts.
  '("-" ""))

(defun-once emacs-lisp-abbrev+-make-abbrevs
  (abbrev+-compile-abbreviations
   (list
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("beginning"  1 3)
              ("of"         1)
              ("line"       1))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(beginning-of-line"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("buffer"    1 3)
              ("substring" 1 3))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(buffer-substring"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("buffer"     1 3)
              ("substring"  1 3)
              ("no"         1)
              ("properties" 0 1))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(buffer-substring-no-properties"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("def"  1 2)
              ("keys" 1 2)
              ("for"  1 2)
              ("map"  1 2))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(def-keys-for-map"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("multiple" 1 4)
              ("value"    1 3)
              ("bind"     1 1))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(multiple-value-bind"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("destructuring" 3 5)
              ("bind"          1 1))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(destructuring-bind"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (list "l" "la" "lam"))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(lambda"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (list "msg" "mess"))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(message"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (list "cond-case"
                 "condcase"))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(condition-case"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (list "pmi"
                 "pmin"
                 "p-mi"
                 "ptmix"
                 "pt-mix"))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(point-min"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (list "pma"
                 "pmax"
                 "p-ma"
                 "ptmax"
                 "pt-max"))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(point-max"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("unwind"  2 3)
              ("protect" 2 4))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(unwind-protect"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (list "eol"
                 "e-o-l"))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(end-of-line"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("line"      1)
              ("beginning" 1 3)
              ("pos"))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(line-beginning-position"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("line"     1)
              ("end"      1)
              ("position" 3 5))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(line-end-position"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("with"     1)
              ("current"  1 4)
              ("buffer"   1 3))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(with-current-buffer"))
    (cons (emacs-lisp-abbrev+-only-when-called
           (make-abbrev+-triggers-for-func-name
            emacs-lisp-abbrev+--delims
            '(("with"   1)
              ("temp"   1)
              ("buffer" 1 3))))
          (make-abbrev+-abbreviation
           :action-type 'literal-string
           :action-data "(with-temp-buffer"))

    (cons (list "info")
          (make-abbrev+-abbreviation
           :action-type 'function-with-side-effects
           :action-data #'emacs-lisp-print-info-template
           :predicate (lambda ()
                        (not (lisp-prev-pos-is-beginning-of-list? (point)))))))))

(defun emacs-lisp-abbrev+-setup ()
  (setf abbrev+-abbreviations (emacs-lisp-abbrev+-make-abbrevs)
        abbrev+-do-not-expand-predicate #'lisp-point-inside-string-or-comment?)
  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'emacs-lisp-abbrev+)

;; Local Variables:
;; End:

;; emacs-lisp-abbrev+.el ends here
