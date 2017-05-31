;; c++-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  8 October 2012
;; Description:

(require 'common)

(defun c++-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w" "w_" "^ >")
        abbrev+-abbreviations
        (list
         (make-abbrev+-abbreviation
          :trigger "\\<pr\\(?:i\\(?:nt?\\)?\\)?f?\\>"
          :action-type 'yas-snippet
          :action-data "printf(\"$1\\n\"$2);$0"
          :predicate #'point-not-inside-string-or-comment?)
         (make-abbrev+-abbreviation
          :trigger "\\<info\\>"
          :action-type 'yas-snippet
          :action-data "std::cout << \"$1: \" << $1 << std::endl;$2"
          :predicate #'point-not-inside-string-or-comment?)
         (make-abbrev+-abbreviation
          :trigger "\\(?:std::?\\)?\\<cout\\>"
          :action-type 'yas-snippet
          :action-data "std::cout << $1 << std::endl;$2"
          :predicate #'point-not-inside-string-or-comment?)
         (make-abbrev+-abbreviation
          :trigger "\\(?:std::?\\)?\\<endl\\>"
          :action-type 'literal-string-no-space-at-end
          :action-data "std::cout << std::endl;"
          :predicate #'point-not-inside-string-or-comment?)
         (make-abbrev+-abbreviation
          :trigger "\\(?:std::?\\)?\\<cerr\\>"
          :action-type 'yas-snippet
          :action-data "std::cerr << $1 << std::endl;$2"
          :predicate #'point-not-inside-string-or-comment?)
         (make-abbrev+-abbreviation
          :trigger (rx bow
                       "s"
                       (? "t"
                          (? "a"
                             (? "t"
                                (? "i"
                                   (? "c")))))
                       (? "_")
                       "cast"
                       eow)
          :action-type 'yas-snippet
          :action-data "static_cast<${1:target}>($2)$3"
          :predicate #'point-not-inside-string-or-comment?)))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'c++-abbrev+)

;; Local Variables:
;; End:

;; c++-abbrev+.el ends here
