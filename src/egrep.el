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
  file         ;; Absolute file name.
  start-pos    ;; Integer. Buffer position of match start.
  end-pos      ;; Integer. Buffer position of match end.
  select-entry ;; String with filename, line number and match highlighted
  )

(defun egrep--make-match-entry (file-name match-start match-end)
  "Make text entry for current match that can be shown to the user.

MATCH-START and MATCH-END are match bounds in the current buffer"
  (save-excursion
    (goto-char match-start)
    (let* ((line-start-pos
            (line-beginning-position))
           (line-number (line-number-at-pos))
           (line-end-pos
            (save-excursion
              (goto-char match-end)
              (line-end-position)))
           (matched-text (buffer-substring line-start-pos
                                           line-end-pos)))
      (put-text-property
       (- match-start line-start-pos)
       (- match-end line-start-pos)
       'face
       'lazy-highlight
       matched-text)
      (let* ((header
              (concat file-name
                      ":"
                      (propertize (number->string line-number)
                                  'face 'compilation-line-number)
                      ":"))
             (header-space (make-string (length header) ?\s))
             (lines (split-into-lines matched-text nil)))
        (cl-assert lines)
        (concat header
                (if lines
                    (concat
                     (car lines)
                     (if (cdr lines)
                         (concat "\n"
                                 (mapconcat (lambda (line)
                                              (concat header-space line))
                                            (cdr lines)
                                            "\n"))
                       ""))
                  "!error: no lines in the block!")
                "\n")))))

(defun egrep--find-matches (regexp exts-globs ignored-exts-globs dir ignore-case)
  (let* ((files (find-rec*
                 :root dir
                 :extensions-globs exts-globs
                 :ignored-extensions-globs ignored-exts-globs
                 :ignored-directories +ignored-directories+
                 :ignored-directory-prefixes +ignored-directory-prefixes+))
         (files-length (length files))
         (progress-reporter
          (make-standard-progress-reporter files-length "files"))
         (matches
          (loop
            for filename in files
            nconc
            (for-buffer-with-file filename
              (funcall progress-reporter 1)
              (redisplay t)
              (goto-char (point-min))
              (let* ((result-ptr (cons nil nil))
                     (local-matches result-ptr)
                     (case-fold-search ignore-case)
                     (short-file-name
                      (propertize (file-relative-name filename dir)
                                  'face 'compilation-info)))
                (while (re-search-forward regexp nil t)
                  (let* ((match-start (match-beginning 0))
                         (match-end (match-end 0))
                         (entry
                          (egrep--make-match-entry short-file-name
                                                   match-start
                                                   match-end)))
                    (setf (cdr local-matches)
                          (cons (make-egrep-match
                                 :file filename
                                 :start-pos match-start
                                 :end-pos match-end
                                 :select-entry entry)
                                nil))
                    (setf local-matches (cdr local-matches))
                    ;; Jump to end of line in order to show at most one match per
                    ;; line.
                    (end-of-line)))
                (cdr result-ptr))))))
    (when (null matches)
      (error "No matches for regexp \"%s\" across files %s"
             regexp
             (mapconcat #'identity exts-globs ", ")))
    (message "Finished looking in files")
    (redisplay t)
    matches))

(defun egrep-search (regexp exts-globs ignored-exts-globs dir ignore-case)
  "Search for REGEXP in files under directory DIR that match FILE-GLOBS and don't
match IGNORED-FILE-GLOBS."
  (let ((matches
         (list->vector
          (egrep--find-matches regexp exts-globs ignored-exts-globs dir ignore-case)))
        (kmap (make-sparse-keymap)))
    (def-keys-for-map kmap
      ("H" (lambda ()
             (interactive)
             (let ((new-matches
                    (list->vector
                     (egrep--find-matches regexp exts-globs ignored-exts-globs dir ignore-case))))
               (select-mode-update-items new-matches 0)))))
    (select-mode-start-selection
     matches
     :buffer-name "*grep*"
     :after-init (lambda ()
                   (select-mode-extend-keymap-with kmap))
     :on-selection
     (lambda (idx match selection-type)
       ;; NB Don't call `select-mode-exit' here since we may return to *grep* buffer
       ;; to try out another match.
       (let ((buf (aif (find-buffer-visiting (egrep-match/file match))
                      it
                    (find-file-noselect (egrep-match/file match)))))
         (funcall
          (pcase selection-type
            (`same-window  #'switch-to-buffer)
            (`other-window #'switch-to-buffer-other-window))
          buf)
         (goto-char (egrep-match/start-pos match))))
     :item-show-function
     #'egrep-match/select-entry
     :separator nil
     :preamble
     (format "Browse matches for ‘%s’ in files matching %s starting at directory %s\n\n"
             regexp
             (mapconcat #'identity exts-globs " ")
             dir)
     :working-directory dir)))

(defun egrep--read-files (regexp)
  "Query user for list of glob patterns to search in and return list of
string patterns."
  (split-string (grep-read-files regexp)
                "[ \t\n\r\f\v]+"
                t))

;;;###autoload
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
  (cl-assert (listp exts-globs))
  (egrep-search regexp exts-globs grep-find-ignored-files dir ignore-case))

;;;###autoload
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
