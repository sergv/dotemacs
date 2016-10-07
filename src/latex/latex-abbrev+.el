;; latex-abbrev+.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 19 December 2011
;; Keywords:
;; Requirements:
;; Status:

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
         (loop
           while (re-search-forward label-re nil t)
           collect (match-string-no-properties 1))
         #'equal)))))

(defun latex-insert-reference-template ()
  "Insert \\ref{} construct and put prompt between angle brackets."
  (let ((label
         (ido-completing-read "Label: "
                              (latex-get-labels-in-buffer)
                              nil
                              ;; I deliberately do not require match here to
                              ;; enable not-yet-entered labels
                              nil)))
    (insert "\\ref{" label "}")))

(defun latex-setup-abbrev+ ()
  (setf abbrev+-skip-syntax '("^ >")
        abbrev+-abbreviations
        (list
         (list (rx "\\" "i") "\\item")
         (list (rx "\\" "п") "\\item")
         (list (rx "\\" "и") "\\item")
         (list (rx "\\" "r" (? "e" (? "f")))
               (list
                #'latex-insert-reference-template))
         (list (rx "\\" "щ" (? "в" (? "н")))
               (list
                #'latex-insert-reference-template))
         (list (rx "\\" "р" (? "е" (? "ф")))
               (list
                #'latex-insert-reference-template))))

  (def-keys-for-map vim:insert-mode-local-keymap
    ("SPC" abbrev+-insert-space-or-expand-abbrev)))

(provide 'latex-abbrev+)

;; Local Variables:
;; End:

;; latex-abbrev+.el ends here
