;; ghc-check-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 13 April 2015
;; Description:

(require 'haskell-compile)

(define-derived-mode ghc-check-mode prog-mode "GHC Check"
  "Mode to enable haskell compilation error fontification independent
of `compilation-mode'."
  (let* ((regexp-alist
          ;; haskell-compilation-error-regexp-alist
          (list haskell-compilation-error-filename-regexp)
          )
         (maybe-pair-group
          (lambda (group face lax-match)
            (if (cons? group)
              `((,(car group) ,face nil lax-match)
                (,(cdr group) ,face nil lax-match))
              `((,group ,face nil lax-match)))))
         (fontification-regexps
          (map (lambda (entry)
                 (destructuring-bind (regexp file line column type) entry
                   `(,regexp
                     (,file
                      ,(cond
                         ((or (equal? type nil)
                              (equal? type 2))
                          'compilation-error-face)
                         ((equal? type 1)
                          'compilation-warning-face)
                         ((equal? type 0)
                          'compilation-info-face)))
                     ,@(funcall maybe-pair-group line 'compilation-line-face nil)
                     ,@(funcall maybe-pair-group column 'compilation-column-face t))))
               regexp-alist)))
    (setq-local *compilation-jump-error-regexp*
                (caar regexp-alist))
    (set (make-local-variable 'compilation-error-regexp-alist)
         regexp-alist)
    (set (make-local-variable 'font-lock-defaults)
         `(,fontification-regexps
           nil ;; perform syntactic fontification
           nil ;; do not ignore case
           nil ;; no special syntax provided
           ))
    (font-lock-mode +1)))

(provide 'ghc-check-mode)

;; Local Variables:
;; End:

;; ghc-check-mode.el ends here
