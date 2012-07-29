;;; cl-mode.el ---

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

  (set (make-local-variable 'find-tag-default-function) 'lisp-find-tag-default)
  (set (make-local-variable 'comment-start-skip)
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
;; lexical-binding: t
;; End:

;;; cl-mode.el ends here
