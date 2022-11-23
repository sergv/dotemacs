;; vim-align.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 10 November 2022
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'vim-common)
(require 'vim-macs)
(require 'vim-ex)

;; (vim--define-arg-handler 'substitute
;;   :activate 'vim-ex--pattern-argument-activate
;;   :deactivate 'vim-ex--pattern-argument-deactivate
;;   :update 'vim-ex--pattern-argument-update)
;; argument:align

(defconst vim--align-known-flags '(?n))

(defun vim--parse-align-pattern-flags (str)
  (let ((i 0)
        (end (length str)))
    (while (and (< i end)
                (not (memq (aref str i) vim-common-command-delimiters)))
      (cl-incf i))
    (let ((delimiter (aref str i)))
      (cl-incf i)
      (let ((pattern-start i)
            (continue t))
        (while (and (< i end)
                    continue)
          (let ((c (aref str i)))
            (cond
              ((eq c ?\\)
               (setf i (+ i 2)))
              ((eq c delimiter)
               (setf continue nil))
              (t
               (cl-incf i)))))
        (let ((pattern-end i))
          (cl-incf i)
          (let ((flags-start i))
            (while (and (< i end)
                        (and (memq (aref str i) vim--align-known-flags)))
              (cl-incf i))
            (let ((flags-end i))
              (values (expand-escape-sequences!
                       (substring-no-properties str pattern-start (min end pattern-end)))
                      (and (< flags-start end)
                           (string->list (substring-no-properties str flags-start flags-end)))))))))))

(vim-defcmd vim:cmd-align (motion argument:text nonrepeatable noninteractive)
  "The VIM align command: [range]s/pattern/flags

Allowed flags are:
  n - align only first occurrence on every line
  i - ignore case
  I - don't ignore case"
  (save-match-data
    (vim:cmd-nohighlight)
    (cl-multiple-value-bind (pattern flags)
        (vim--parse-align-pattern-flags argument)
      (let* ((pattern-with-group
              (if-let ((re-groups (parse-regexp-groups pattern)))
                  (cons pattern (apply #'min re-groups))
                (cons (concat "\\(?1:\\s-*\\)\\(?:" pattern "\\)") 1)))
             (regexp (car pattern-with-group))
             (group-number (cdr pattern-with-group))
             (case-fold-search
              (cond
                ((memq ?i flags) t)
                ((memq ?I flags) nil)
                (t               nil))))
        (align-regexp (vim-motion-begin-pos motion)
                      (vim-motion-end-pos motion)
                      regexp
                      group-number
                      nil ;; spacing
                      (not (memq ?n flags)))))))

(provide 'vim-align)

;; Local Variables:
;; End:

;; vim-align.el ends here
