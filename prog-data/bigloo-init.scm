;; bigloo-init.scm ---

;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 23 July 2012

(set-prompter! (lambda (level)
                 (if (= 1 level)
                     (display "> ")
                     (begin
                      (display level)
                      (display "> ")))))



