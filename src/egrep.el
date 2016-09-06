;; egrep.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 18 February 2016
;; Description:

(require 'f)

(require 'common)
(require 'find-files)
(require 'macro-util)

(autoload 'grep-read-files "grep")
(autoload 'grep-read-regexp "grep")

(defstruct (egrep-match
            (:conc-name egrep-match/))
  file          ;; Absolute file name.
  line-number   ;; Integer.
  column-number ;; Integer.
  ;; line          ;; String with match highlighted.
  select-entry  ;; String with filename, line number and match highlighted
  )

(defun egrep-search (regexp exts-globs ignored-exts-globs dir ignore-case)
  "Search for REGEXP in files under directory DIR that match FILE-GLOBS and don't
match IGNORED-FILE-GLOBS."
  (let* ((files (find-rec*
                 :root dir
                 :extensions-globs exts-globs
                 :ignored-extensions-globs ignored-exts-globs
                 :ignored-directories *ignored-directories*
                 :ignored-directory-prefixes *ignored-directory-prefixes*))
         (matches
          (list->vector
           (-mapcat
            (lambda (filename)
              (for-buffer-with-file filename
                (goto-char (point-min))
                (let ((local-matches nil)
                      (case-fold-search ignore-case))
                  (while (re-search-forward regexp nil t)
                    (let* ((line-start-pos (line-beginning-position))
                           (line (current-line-with-properties))
                           (start-column
                            (- (match-beginning 0) line-start-pos))
                           (end-column
                            (- (match-end 0) line-start-pos))
                           (line-number (count-lines (point-min) (point))))
                      (put-text-property
                       start-column
                       end-column
                       'face
                       'lazy-highlight
                       line)
                      (push (make-egrep-match
                             :file filename
                             :line-number line-number
                             :column-number start-column
                             :select-entry
                             (let ((short-file-name (file-relative-name filename
                                                                        dir))
                                   (line-number-string (number->string line-number)))
                               (concat (propertize short-file-name 'face 'compilation-info)
                                       ":"
                                       (propertize line-number-string 'face 'compilation-line-number)
                                       ":"
                                       line
                                       "\n")))
                            local-matches))
                    ;; Jump to end of line in order to show at most one match per
                    ;; line.
                    (end-of-line))
                  (nreverse local-matches))))
            files))))
    (when (= (length matches) 0)
      (error "No matches for regexp \"%s\" across files %s"
             regexp
             (mapconcat #'identity exts-globs ", ")))
    (select-start-selection
     matches
     :buffer-name "*grep*"
     :on-selection
     (lambda (idx selection-type)
       (let ((match (elt matches idx)))
         (let ((buf (aif (find-buffer-visiting (egrep-match/file match))
                      it
                      (find-file-noselect (egrep-match/file match)))))
           (funcall
            (pcase selection-type
              (`same-window  #'switch-to-buffer)
              (`other-window #'switch-to-buffer-other-window))
            buf)
           (goto-line (egrep-match/line-number match))
           (move-to-column (egrep-match/column-number match)))))
     :item-show-function
     #'egrep-match/select-entry
     :separator-function
     (constantly nil)
     :preamble-function
     (lambda ()
       (format "Browse matches for `%s' in files matching %s\n\n"
               regexp
               (mapconcat #'identity exts-globs " "))))))

(defun egrep--read-files (regexp)
  "Query user for list of glob patterns to search in and return list of
string patterns."
  (split-string (grep-read-files regexp)
                "[ \t\n\r\f\v]+"
                t))

(defun egrep (regexp exts-globs dir &optional ignore-case)
  (interactive
   (let ((regexp (grep-read-regexp)))
     (list
      regexp
      (egrep--read-files regexp)
      (read-directory-name "Base directory: "
                           nil default-directory t)
      (and current-prefix-arg
           (<= 4 (first current-prefix-arg))))))
  (assert (listp exts-globs))
  (egrep-search regexp exts-globs grep-find-ignored-files dir ignore-case))

(defun egrep-region (str exts-globs dir &optional ignore-case)
  (interactive
   (let* ((ignore-case?
           (and current-prefix-arg
                (<= 4 (first current-prefix-arg))))
          (regexp
           (read-regexp (format
                         "%s for"
                         (if ignore-case?
                           "Case-insensetive search"
                           "Search"))
                        (get-region-string-no-properties)
                        'grep-regexp-history)))
     (list
      regexp
      (egrep--read-files regexp)
      (read-directory-name "Base directory: "
                           nil default-directory t)
      ignore-case?)))
  (egrep str exts-globs dir ignore-case))

(provide 'egrep)

;; Local Variables:
;; End:

;; egrep.el ends here
