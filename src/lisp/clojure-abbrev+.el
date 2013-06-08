;; clojure-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  3 February 2013
;; Description:

(require 'abbrev+)

(defun clojure-util/quote-format-arguments (x)
  (replace-regexp-in-string (rx "%"
                                (group
                                 (regexp "[^bBhHsScCdoxXeEfgGaAtT%n]\\|$" )))
                            "%%\\1"
                            x))

(define-lisp-print-info-skeleton
    clojure-print-info-skeleton
  :doc "Insert call to print and format to show some variable values and messages."
  :print-begin "(print (format "

  :format-print-value "%s"
  :format-string-start "\""
  :format-string-end "\\n\""
  :print-end "))"
  :msg-transform #'clojure-util/quote-format-arguments

  :make-variable-list join-lines)

(define-lisp-print-info-skeleton
    clojure-android-log-skeleton
  :doc "Insert call to android log to show some variable values and messages."
  :print-begin "(log "

  :format-print-value "%s"
  :format-string-start "\""
  :format-string-end "\\n\""
  :print-end ")"
  :msg-transform #'clojure-util/quote-format-arguments

  :make-variable-list join-lines)

(defun clojure-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w_" "w_(" ;;"^ >"
                              )
        abbrev+-abbreviations
        (list
         (list "\\_<info\\_>"
               (list #'clojure-print-info-skeleton)
               (lambda ()
                 (and (not (lisp-point-inside-string-or-comment?))
                      (not (lisp-prev-pos-is-beginning-of-list? (point))))))
         (list "\\_<log\\_>"
               (list #'clojure-android-log-skeleton)
               (lambda ()
                 (and (not (lisp-point-inside-string-or-comment?))
                      (not (lisp-prev-pos-is-beginning-of-list? (point))))))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'clojure-abbrev+)

;; Local Variables:
;; End:

;; clojure-abbrev+.el ends here
