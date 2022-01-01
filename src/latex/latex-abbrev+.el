;; latex-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 19 December 2011
;; Keywords:
;; Requirements:
;; Status:

(eval-when-compile
  (require 'cl-lib)
  (require 'macro-util))

;; set up abbrev+ for latex

(defun latex-get-labels-in-buffer ()
  "Return list of all labels used in buffer. Labels are extracted
from \\label{...} and \\ref{...} constructs."
  (interactive)
  (let ((label-re (rx (or "\\label{" "\\ref{")
                      (group (*? (regexp "[^ \t\n{}]")))
                      "}")))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (remove-duplicates-hashing
         (cl-loop
           while (re-search-forward label-re nil t)
           collect (match-string-no-properties 1))
         #'equal)))))

(defun latex-insert-reference-template ()
  "Insert \\ref{} construct and put prompt between angle brackets."
  (let ((label
         (ivy-completing-read "Label: "
                              (latex-get-labels-in-buffer)
                              nil
                              ;; I deliberately do not require match here to
                              ;; enable not-yet-entered labels
                              nil)))
    (insert "\\ref{" label "}")))

(defun latex-setup-abbrev+ ()
  (setf abbrev+-skip-syntax ["^ >"]
        abbrev+-abbreviations
        (vector
         (make-abbrev+-abbreviation
          :trigger (rx "\\" (or "i" "п" "и"))
          :action-type 'literal-string
          :action-data "\\item")
         (make-abbrev+-abbreviation
          :trigger (rx "\\" (or (seq "r" (? "e" (? "f")))
                                (seq "щ" (? "в" (? "н")))
                                (seq "р" (? "е" (? "ф")))))
          :action-type 'function-with-side-effects
          :action-data #'latex-insert-reference-template)))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'latex-abbrev+)

;; Local Variables:
;; End:

;; latex-abbrev+.el ends here
