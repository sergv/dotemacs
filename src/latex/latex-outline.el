;; latex-outline.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 30 August 2011
;; Keywords:
;; Requirements:
;; Status:

;; This package is frozen for undefinite time becouse
;; it's too much work to do and result will not differ
;; in any significant way from vim support of latex.
;; Therefore I STRONGLY encourage You to abandon this
;; file and forget about it until You lose too much time
;; tinkering with it.

(defmacro latex:save-ex-save-re (&rest body)
  (declare (indent defun))
  `(save-excursion
     (save-match-data
       ,@body)))


(defconst latex-preamble-start "\\\\documentclass\\[.*\\]{.*}")
(defconst latex-document-start "^ *\\\\begin *{document}[ %]*$")

(defmacro latex-initialize (&rest body)
  (declare (indent defun))
  `(progn
     (unless latex:document-start
       (latex-set-up-document-start-marker))
     (unless latex:document-start
       (error
        "latex-initialize: fatal error: cannot find document start \\begin{document}, aborting."))
     ,@body))


(defun latex-outline-level ()
  "Determile folding level."
  (interactive)
  (latex-initialize
   (save-excursion
     (end-of-line)
     (if (< (point) (marker-position latex:document-start))
         1
       (let* ((line (current-line))
              (type-of-section (or (case (latex-type-of-section-at-point)
                                     (part 0)
                                     (chapter 1)
                                     (section 2)
                                     (subsection 3)
                                     (subsubsection 4)
                                     (paragraph 5)
                                     (subparagraph 6))
                                   ;; we're in preamble or
                                   ;; in space just after document start
                                   0))
              (sect-start (or (latex-begin-of-section-at-point)
                              (marker-position latex:document-start)))
              (whitespace-offset (loop
                                   for i = 0 then (1+ i)
                                   while (char= ?\s (aref line i))
                                   count t)))
         (+ whitespace-offset
            ;; if line is some type of section
            (if (latex:type-of-section line)
                0
              1)
            type-of-section
            (let ((y (length (latex:get-unmatched-environments-in-region
                              sect-start
                              (point)))))
              (max 0 (1- y)))))))))

(defconst latex-begin-environment "\\\\begin{\\([^{}]+\\)}")
(defconst latex-end-environment "\\\\end{\\([^{}]+\\)}")

(defun latex:get-unmatched-environments-in-region (begin end)
  "Return list of environment names that have been opened with \\begin{name}
in region between BEGIN and END but were not closed with \\end{name}.
List will be in order of appearance of environments in region."
  (interactive "r")
  (latex:save-ex-save-re
   (let* ((table (make-hash-table :test 'equal
                                  :size 23))
          (number 1)
          result)

     (goto-char begin)
     (while (re-search-forward latex-begin-environment end t)
       (let* ((env-name (match-string-no-properties 1))
              (count (1+ (cdr (gethash env-name table (cons 0 0)))))
              (entry (cons number count)))
         (puthash env-name
                  entry
                  table)
         (incf number)))

     (goto-char begin)
     (while (re-search-forward latex-end-environment end t)
       (let* ((env-name (match-string-no-properties 1))
              (val (gethash env-name table (cons 0 0)))
              (count (1- (cdr val)))
              (entry (cons (car val) count)))
         (puthash env-name
                  entry
                  table)))

     (maphash (lambda (env-name v)
                ;; (unless (or (string= env-name "document")
                ;;             (>= (cdr v) 0))
                ;;   (error "have negative count of ends: %s on %s" v env-name))
                (when (> (cdr v) 0)
                  (dotimes (_ (cdr v))
                    (push (cons env-name (car v)) result))))
              table)
     (-map #'car
           (sort result
                 (lambda (a b)
                   (< (cdr a) (cdr b))))))))


(defun latex-type-of-section-at-point ()
  "Return type of section in which point is currently
located. Possible values are 'section, 'subsection and 'subsubsection and nil
if point is in preamble or not in section."
  ;; may need to use latex-initialize here
  (latex:save-ex-save-re
   ;; need to move past the opening { of section header here
   (line-end-position)
   (when (re-search-backward latex-sectioning-regexp
                             (marker-position latex:document-start)
                             t)
     (message "MATCH STRING 0: %S" (match-string-no-properties 0))
     (latex:type-of-section (match-string-no-properties 0)))))

(defun latex-begin-of-section-at-point ()
  "Return position of beginning of sectioning entity that captures
position at point."
  ;; may need to use latex-initialize here
  (latex:save-ex-save-re
   ;; need to move past the opening { of section header here
   (line-end-position)
   (re-search-backward latex-sectioning-regexp
                       (marker-position latex:document-start)
                       t)))

(defconst latex-sectioning-regexp
  (rx "\\"
      (group
       (or "part"
           "chapter"
           "section"
           "subsection"
           "subsubsection"
           "paragraph"
           "subparagraph"))
      (? "*")
      "{"
      (+? anything)
      ;; "}" ;; <- this causes bugs when section header ends up on more than one line
      ))

(defun latex:type-of-section (str)
  "Return type of section that STR denotes,
possible values are 'part, 'chapter, 'section, 'subsection, 'subsubsection,
'paragraph and 'subparagraph or nil if str doesn't denotes any type
of section."
  (save-match-data
    (when (string-match latex-sectioning-regexp str)
      (intern (match-string 1 str)))))


(defvar latex:document-start nil
  "Marker that marks \\begin{document} in latex file.")
(make-variable-buffer-local 'latex:document-start)
(set-default 'latex:document-start nil)


(defun latex-set-up-document-start-marker ()
  "Set up position of \\begin{document} string in latex file
for use in utility functions."
  (interactive)
  (latex:save-ex-save-re
   (goto-char (point-min))
   (let ((start (re-search-forward latex-document-start nil t)))
     (when start
       (setq latex:document-start (copy-marker start))))))


(defconst latex-part-regexp          "\\\\part\\*?{\\(?:.\\|\n\\)*?}")
(defconst latex-chapter-regexp       "\\\\chapter\\*?{\\(?:.\\|\n\\)*?}")
(defconst latex-section-regexp       "\\\\section\\*?{\\(?:.\\|\n\\)*?}")
(defconst latex-subsection-regexp    "\\\\subsection\\*?{\\(?:.\\|\n\\)*?}")
(defconst latex-subsubsection-regexp "\\\\subsubsection\\*?{\\(?:.\\|\n\\)*?}")
(defconst latex-paragraph-regexp     "\\\\paragraph\\*?{\\(?:.\\|\n\\)*?}")
(defconst latex-subparagraph-regexp  "\\\\subparagraph\\*?{\\(?:.\\|\n\\)*?}")

(defun latex-hide-all ()
  "Hide all sections in latex file."
  (interactive)
  (latex:save-ex-save-re
   (let ((type-re (reduce (lambda (re x)
                            (if re
                                re
                              (when x
                                (goto-char (marker-position
                                            latex:document-start))
                                (when (re-search-forward x nil t)
                                  x))))
                          (list latex-part-regexp
                                latex-chapter-regexp
                                latex-section-regexp
                                latex-subsection-regexp
                                latex-subsubsection-regexp
                                latex-paragraph-regexp
                                latex-subparagraph-regexp)
                          :initial-value nil)))

     (goto-char (marker-position latex:document-start))
     (while (search-forward-regexp type-re nil t)
       (outline-hide-subtree)))))


(defconst latex-outline-regexp
  (rx (sequence bol
                (* whitespace)
                "\\"
                (or (sequence (or "part"
                                  "chapter"
                                  "section"
                                  "subsection"
                                  "subsubsection"
                                  "paragraph"
                                  "subparagraph"
                                  "begin"
                                  "end")
                              (? "*"))
                    (sequence "documentclass"
                              (? "["
                                 (* nonl)
                                 "]")
                              "{"
                              (*? nonl)
                              "}")))))

(defhydra-derive hydra-latex-vim-normal-z-ext hydra-vim-normal-z-ext (:exit t :foreign-keys nil :hint nil)
  "
_C_: hide all
_O_: show all
_c_: hide subtree
_o_: show subtree"
  ("C" latex-hide-all)
  ("c" hide-subtree)
  ("O" show-all)
  ("o" show-subtree))

(defun latex-setup-folding ()
  (interactive)
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot buffer-display-table
                          'selective-display
                          (string-to-vector " ..."))

  ;; outline uses this regexp to find headers.
  (setq outline-regexp latex-outline-regexp)
  ;; (set (make-local-variable 'outline-heading-end-regexp)
  ;; "")

  ;; enable our level computation
  (setq outline-level #'latex-outline-level)
  ;; turn on outline mode
  (outline-minor-mode t)
  ;; initially hide all but the headers
  ;;(hide-body)

  (def-keys-for-map vim:normal-mode-local-keymap
    ("'" outline-up-heading)
    ("z" hydra-latex-vim-normal-z-ext/body)))

(provide 'latex-outline)

;; Local Variables:
;; End:

;; latex-outline.el ends here
