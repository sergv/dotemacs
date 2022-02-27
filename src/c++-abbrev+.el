;; c++-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  8 October 2012
;; Description:

(eval-when-compile
  (require 'macro-util))

(require 'common)

(defun c++-print-info-template ()
  "Insert call to printf statement to print some variables and messages
while interactively prompting for variables/messages."
  (interactive)
  (let* ((start (lambda () (insert "std::cout")))
         (end (lambda () (insert " << std::endl;")))
         (message (lambda (is-initial? input)
                    (insert " << \""
                            (if is-initial? "" ", ")
                            (replace-regexp-in-string "\\([\"\\]\\)" "\\\\1" input) "\"")))
         (variable (lambda (is-initial? input)
                     (insert " << \""
                             (if is-initial? "" ", ")
                             input
                             " = \" << "
                             input))))
    (insert-info-template
     :start start
     :end end
     :insert-continuation #'ignore
     :insert-message message
     :insert-variable variable)))

(defun-once c++-abbrev+-make-abbrevs
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
           :action-data "printf(\"$1\\n\"$2);$0"))
    (cons (list "info")
          (make-abbrev+-abbreviation
           :action-type 'function-with-side-effects
           :action-data #'c++-print-info-template))
    (cons (list "cout"
                "std:cout"
                "std::cout")
          (make-abbrev+-abbreviation
           :action-type 'yas-snippet
           :action-data "std::cout << $1 << std::endl;$2"))
    (cons (list "endl"
                "std:endl"
                "std::endl")
          (make-abbrev+-abbreviation
           :action-type 'literal-string-no-space-at-end
           :action-data "std::cout << std::endl;"))
    (cons (list "cerr"
                "std:cerr"
                "std::cerr")
          (make-abbrev+-abbreviation
           :action-type 'yas-snippet
           :action-data "std::cerr << $1 << std::endl;$2"))
    (cons (make-abbrev+-triggers-for-func-name
           '("_" "")
           '(("static" 1)
             ("cast")))
          (make-abbrev+-abbreviation
           :action-type 'yas-snippet
           :action-data "static_cast<${1:target}>($2)$3")))))

(defun c++-abbrev+-setup ()
  (setf abbrev+-abbreviations (c++-abbrev+-make-abbrevs)
        abbrev+-do-not-expand-predicate #'point-inside-string-or-comment?)
  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'c++-abbrev+)

;; Local Variables:
;; End:

;; c++-abbrev+.el ends here
