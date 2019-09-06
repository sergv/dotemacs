;; python-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Friday, 27 January 2012
;; Keywords:
;; Requirements:
;; Status:

(require 'macro-util)
(require 'common)

(defun python-print-info-template ()
  "Insert call to print statement to print some variables and messages while
interactively prompting for variables/messages."
  (interactive)
  (let* ((entity-name (python--function-name-at-position (point)))
         (quote-message
          (lambda (x) (replace-regexp-in-string (rx "\"") "\\\"" x)))
         (start
          (lambda ()
            (insert "print(\""
                    (or entity-name ""))))
         (end
          (lambda (var-list)
            (insert "\"")
            (when (< 0 (length var-list))
              (insert
               ".format("
               (join-lines var-list ", ")
               ")"))
            (insert ")")))
         (format
          (lambda (user-input) (insert (funcall quote-message user-input) " = {}"))))
    (insert-info-format-template
     :start start
     :end end
     :format format
     :reindent-at-end #'prog-indent-sexp
     :quote-message quote-message)))

(defun python--function-name-at-position (position)
  "Find out python function name that contains POSITION. Return empty
string on error"
  (save-excursion
    (save-match-data
      (goto-char position)
      (if (= 0 (current-column))
        ""
        (condition-case nil
            (progn
              (beginning-of-defun)
              (when (looking-at "def[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)")
                (concat
                 (upcase (match-string 1))
                 ": ")))
          (error ""))))))

(defun python-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w" "w_" "^ >")
        abbrev+-abbreviations
        (list
         ;; Use space 'function-with-side-effects to avoid space at the end.
         (make-abbrev+-abbreviation
          :trigger "\\."
          :action-type 'literal-string-no-space-at-end
          :action-data "self."
          :predicate #'point-not-inside-string-or-comment?)
         (make-abbrev+-abbreviation
          :trigger "pr\\(?:i\\(?:nt?\\)?\\)?"
          :action-type 'yas-snippet
          :action-data "print(\"$1\")$0"
          :predicate #'point-not-inside-string-or-comment?)
         (make-abbrev+-abbreviation
          :trigger "pr\\(?:i\\(?:nt?\\)?\\)?f"
          :action-type 'yas-snippet
          :action-data "print(\"$1\".format($2))$0"
          :predicate #'point-not-inside-string-or-comment?)
         (make-abbrev+-abbreviation
          :trigger "info"
          :action-type 'function-with-side-effects
          :action-data #'python-print-info-template
          :predicate #'point-not-inside-string-or-comment?)
         ;; print_function
         (make-abbrev+-abbreviation
          :trigger "pr\\(?:i\\(?:nt\\)?\\)?_f\\(?:u\\(?:n\\(?:c\\(?:t\\(?:i\\(?:on?\\)?\\)?\\)?\\)?\\)?\\)?"
          :action-type 'literal-string-no-space-at-end
          :action-data "from __future__ import print_function"
          :predicate #'point-not-inside-string-or-comment?)))
  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'python-abbrev+)

;; Local Variables:
;; End:

;; python-abbrev+.el ends here
