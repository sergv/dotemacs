;; clojure-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  3 February 2013
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'abbrev+)

(defun clojure-util/quote-format-arguments (x)
  (replace-regexp-in-string (rx "%"
                                (group
                                 (regexp "[^bBhHsScCdoxXeEfgGaAtT%n]\\|$" )))
                            "%%\\1"
                            x))

(defun clojure-print-info-template ()
  "Insert call to print and format to show some variable values and messages."
  (interactive)
  (let ((start
         (lambda ()
           (insert "(print (format \"")))
        (end
         (lambda (var-list)
           (insert "\\n\"\n")
           (insert (join-lines (-map #'clojure-util/quote-format-arguments
                                     var-list)))
           (insert "))")))
        (format
         (lambda (user-input) (insert user-input " = %s"))))
    (insert-info-format-template
     :start start
     :end end
     :format format
     :reindent-at-end #'prog-indent-sexp)))

(defun clojure-android-log-template ()
  "Insert call to android log to show some variable values and messages."
  (interactive)
  (let ((start
         (lambda ()
           (insert "(log \"")))
        (end
         (lambda (var-list)
           (insert "\"\n")
           (insert (join-lines (-map #'clojure-util/quote-format-arguments
                                     var-list)))
           (insert ")")))
        (format
         (lambda (user-input) (insert user-input " = %s"))))
    (insert-info-format-template
     :start start
     :end end
     :format format
     :reindent-at-end #'prog-indent-sexp)))

(defun-once clojure-abbrev+-make-abbrevs
  (abbrev+-compile-abbreviations
   (list
    (cons (list "info")
          (make-abbrev+-abbreviation
           :action-type 'function-with-side-effects
           :action-data #'clojure-print-info-template
           :predicate (lambda ()
                        (not (lisp-prev-pos-is-beginning-of-list? (point))))))
    (cons (list "log")
          (make-abbrev+-abbreviation
           :action-type 'function-with-side-effects
           :action-data #'clojure-android-log-template
           :predicate (lambda ()
                        (not (lisp-prev-pos-is-beginning-of-list? (point)))))))))

(defun clojure-abbrev+-setup ()
  (setf abbrev+-abbreviations (clojure-abbrev+-make-abbrevs)
        abbrev+-do-not-expand-predicate #'lisp-point-inside-string-or-comment?)
  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'clojure-abbrev+)

;; Local Variables:
;; End:

;; clojure-abbrev+.el ends here
