;;; scheme-abbrev+.el ---

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday,  3 February 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'general-lisp-setup)
(require 'abbrev+)


(define-lisp-debug-print-skeleton
    scheme-debug-message-skeleton
  :doc "Call to format to print some variables and messages."
  :print-begin "(format #t "
  :use-upcase nil

  :format-print-value "~a"
  :format-string-start "\""
  :format-string-end "\\n\""

  :make-variable-list (lambda (list)
                        (mapconcat #'identity list "\n"))

  ;; Scheme differs from CL and Elisp in function definitions - function name is
  ;; enclosed in parens when using define
  :insert-entity-name-procedure
  (lambda (beginning)
    (save-excursion
     (condition-case nil
         (progn
           (goto-char beginning)
           (save-excursion
            ;; this throws error if no enclosing list found
            (backward-up-list))
           (beginning-of-defun)
           (forward-symbol 1)
           (skip-syntax-forward " (>")
           (let ((symbol (symbol-at-point)))
             (if symbol
               (concat
                ;; do not use upcase in Scheme
                (symbol-name symbol)
                ": ")
               "")))
       ;; no enclosing list was found, so use no name here
       (error "")))))


(defun scheme-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w_" "w_(" ;;"^ >"
                              )
        abbrev+-abbreviations
        (list
         (list (make-abbrev+-re-for-lisp-func-name
                '(("lambda" 3)))
               "(lambda")
         (list "\\_<de?bu?g\\_>"
               (list
                #'scheme-debug-message-skeleton)
               (lambda () (and (not (lisp-point-inside-string-or-comment?))
                               (not (lisp-pos-is-beginning-of-list? (point))))))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))


(provide 'scheme-abbrev+)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; scheme-abbrev+.el ends here
