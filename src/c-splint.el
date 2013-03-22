;; c-splint.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 25 December 2011
;; Keywords:
;; Requirements:
;; Status:

(defun splint-run (&optional arg)
  "Run splint."
  (interactive (list current-prefix-arg))
  (let ((fname (file-name-nondirectory buffer-file-name)))
    (compilation-start (join-lines (list "splint"
                                         fname)
                                   " ")
                       #'splint-mode)))


(define-compilation-mode haskell-compilation-mode "Splint"
  "Mode for splint checks."

  (setq-local compilation-error-regexp-alist
              (list
               (list haskell-compile-warning-regexp 1 2 3 1)
               (list haskell-compile-error-regexp   1 2 3 2)))

  (setq-local *compilation-jump-error-regexp*
              haskell-compile-error-regexp)

  (setq-local compilation-first-column 1) ;GHC counts from 1.
  (setq-local compilation-disable-input t)
  (setq-local compilation-scroll-output nil))


(provide 'c-splint)

;; Local Variables:
;; End:

;; c-splint.el ends here
