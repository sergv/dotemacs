;; octave-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Wednesday, 11 April 2012
;; Description:

(require 'macro-util)
(require 'common)

(defun octave-print-info-template ()
  "Insert call to printf statement to print some variables and messages
while interactively prompting for variables/messages."
  (interactive)
  (let* ((entity-name (octave--function-name-at-point (point)))
         (start
          (lambda ()
            (insert "printf(\"" entity-name)))
         (end
          (lambda (var-list)
            (insert "\\n\"")
            (when (< 0 (length var-list))
              (insert
               (concat ", "
                       (mapconcat (lambda (x)
                                    (concat "num2str(" x ")"))
                                  var-list
                                  ", "))))
            (insert ");")))
         (format
          (lambda (user-input) (insert (upcase user-input) " = %s"))))
    (insert-info-format-template
     :start start
     :end end
     :format format
     :reindent-at-end #'prog-indent-sexp)))

(defun octave--function-name-at-point (position)
  "Get name of function that contains POSITION."
  (save-excursion
    (save-match-data
      (goto-char position)
      (if (= 0 (current-column))
          ""
        (condition-case nil
            (progn
              (beginning-of-defun)
              (when (looking-at
                     (rxx ((name (regex "[a-zA-Z_][a-zA-Z0-9_]*"))
                           (arg-name name))
                       "function"
                       (??
                        (* whitespace)
                        arg-name
                        (* ","
                           (* whitespace)
                           arg-name)
                        (* whitespace)
                        "=")
                       (* whitespace)
                       (group
                        name)
                       (* whitespace)
                       "("))
                (concat
                 (upcase (match-string 1))
                 ": ")))
          (error ""))))))

(defun octave-abbrev+-setup ()
  (setf abbrev+-skip-syntax '("w" "w_" "^ >")
        abbrev+-abbreviations
        (list
         (list "\\<prf?\\>"
               (list
                (lambda ()
                  (yas-expand-snippet "printf(\"${1}\\n\");")))
               #'point-not-inside-string-or-comment?)
         (list "\\<info\\>"
               (list
                #'octave-print-info-template)
               #'point-not-inside-string-or-comment?)))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))


(provide 'octave-abbrev+)

;; Local Variables:
;; End:

;; octave-abbrev+.el ends here
