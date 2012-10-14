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
                  (yas/expand-snippet "printf(\"$1\\n\"$2);$0")))
               (lambda () (not (point-inside-string-or-comment?))))
         (list "\\<info\\>"
               (list
                (lambda ()
                  (yas/expand-snippet
                   "std::cout << \"$1: \" << $1 << std::endl;$2")))
               (lambda () (not (point-inside-string-or-comment?))))
         (list "\\<cout\\>"
               (list
                (lambda ()
                  (yas/expand-snippet
                   "std::cout << $1 << std::endl;$2")))
               (lambda () (not (point-inside-string-or-comment?))))
         (list "\\<cerr\\>"
               (list
                (lambda ()
                  (yas/expand-snippet
                   "std::cerr << $1 << std::endl;$2")))
               (lambda () (not (point-inside-string-or-comment?))))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'c++-abbrev+)

;; Local Variables:
;; End:

;; c++-abbrev+.el ends here
