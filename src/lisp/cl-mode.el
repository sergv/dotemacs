;; cl-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 13 February 2012
;; Keywords:
;; Requirements:
;; Status:

;; Nearly everything is taken from emacs standard lisp-mode.el

;; (add-to-list 'load-path "/home/sergey/emacs/test/")

(require 'ansi-lisp-highlight)
(require 'sexpy-highlight)
(require 'cl)


(define-derived-mode cl-mode prog-mode "Lisp"
  (lisp-mode-variables t t)

  (setq-local find-tag-default-function 'lisp-find-tag-default)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq imenu-case-fold-search t)

  (sexpy-set-up-fontification)

  ;; this worked last time

  (setq font-lock-defaults
        '(cl-font-lock-keywords
          nil
          ;; be case-insensetive
          t
          (("+-*/.<>=!?$%_&~^:@" . "w"))
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function))))

(provide 'cl-mode)

;; Local Variables:
;; End:

;; cl-mode.el ends here
