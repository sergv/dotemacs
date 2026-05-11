;; nix-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 11 May 2026
;; Description:


(eval-when-compile
  (require 'macro-util))

(require 'abbrev+)
(require 'common)
(require 'semnav)

(defun nix--quote-string-for-template-insertion (str)
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defun nix-print-info-template ()
  "Insert call to printf statement to print some variables and messages
while interactively prompting for variables/messages."
  (interactive)
  (let* ((start
          (lambda ()
            (insert "builtins.trace (\"")))
         (end
          (lambda (_acc)
            (insert ") ")))
         (quote-input
          #'nix--quote-string-for-template-insertion)
         (insert-continuation
          (lambda (acc should-merge-messages?)
            (if should-merge-messages?
                (delete-char -1)
              (insert " ++ \""))
            acc))
         (insert-message
          (lambda (acc _is-initial-insertion? user-input)
            (cl-assert (stringp user-input))
            (insert (format "%s\"" (funcall quote-input user-input)))
            (cons 'message user-input)))
         (insert-variable
          (lambda (acc is-initial-insertion? user-input)
            (cl-assert (stringp user-input))
            (insert
             (format "%s%s = \" ++ builtins.toString %s"
                     (cond
                       (is-initial-insertion?
                        "")
                       ((and acc
                             (eq (car acc) 'message)
                             (let ((str (cdr acc)))
                               (cl-assert (stringp str))
                               (eq ?: (aref str (1- (length str))))))
                        " ")
                       (t
                        ", "))
                     (funcall quote-input user-input)
                     (if (string-match-p "[ \t]" user-input)
                         (concat "(" user-input ")")
                       user-input)))
            (cons 'variable user-input))))
    (insert-info-template
     :start start
     :end end
     :insert-continuation insert-continuation
     :insert-message insert-message
     :insert-variable insert-variable)))

(defun-once nix-abbrev+-make-abbrevs
  (abbrev+-compile-abbreviations
   (list
    (cons (list "info"
                "trace")
          (make-abbrev+-abbreviation
           :action-type 'function-with-side-effects
           :action-data #'nix-print-info-template)))))

(defun nix-abbrev+-setup ()
  (setf abbrev+-abbreviations (nix-abbrev+-make-abbrevs)
        abbrev+-do-not-expand-predicate #'point-inside-string-or-comment?)

  (def-keys-for-map vim-insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'nix-abbrev+)

;; Local Variables:
;; End:

;; nix-abbrev+.el ends here
