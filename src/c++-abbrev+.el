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
         (list "\\<pr\\(?:i\\(?:nt?\\)?\\)?f?\\>"
               (list
                (lambda ()
                  (yas-expand-snippet "printf(\"$1\\n\"$2);$0")))
               #'point-not-inside-string-or-comment?)
         (list "\\<info\\>"
               (list
                (lambda ()
                  (yas-expand-snippet
                   "std::cout << \"$1: \" << $1 << std::endl;$2")))
               #'point-not-inside-string-or-comment?)
         (list "\\(?:std::?\\)?\\<cout\\>"
               (list
                (lambda ()
                  (yas-expand-snippet
                   "std::cout << $1 << std::endl;$2")))
               #'point-not-inside-string-or-comment?)
         (list "\\(?:std::?\\)?\\<endl\\>"
               ;; note: use lambda to avoid trailing space
               (list (lambda () (insert "std::cout << std::endl;")))
               #'point-not-inside-string-or-comment?)
         (list "\\(?:std::?\\)?\\<cerr\\>"
               (list
                (lambda ()
                  (yas-expand-snippet
                   "std::cerr << $1 << std::endl;$2")))
               #'point-not-inside-string-or-comment?)
         (list (rx bow
                   "s"
                   (? "t"
                      (? "a"
                         (? "t"
                            (? "i"
                               (? "c")))))
                   (? "_")
                   "cast"
                   eow)
               (list
                (lambda ()
                  (yas-expand-snippet "static_cast<$1>($2)$3")))
               #'point-not-inside-string-or-comment?)))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'c++-abbrev+)

;; Local Variables:
;; End:

;; c++-abbrev+.el ends here
