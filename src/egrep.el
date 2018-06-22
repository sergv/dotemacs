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


(defvar egrep-backend
  (cond
    ((fboundp #'haskell-native-grep-rec)
     'native)
    (t
     'elisp))
  "Which immplementation to use to provide grepping capability within Emacs.")

(defun make-egrep-match (file start-pos formatted-entry)
  (cons file (cons start-pos formatted-entry)))

(defsubst egrep-match-file (x)
  (declare (pure t) (side-effect-free t))
  (car x))

(defsubst egrep-match-start-pos (x)
  (declare (pure t) (side-effect-free t))
  (cadr x))

(defsubst egrep-match-formatted-entry (x)
  (declare (pure t) (side-effect-free t))
  (cddr x))


(defun egrep--format-select-entry (file-name match-start match-end)
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
             (lines (split-into-lines matched-text t)))
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

(defun egrep--find-matches (regexp exts-globs ignored-files-globs root ignore-case)
  (pcase egrep-backend
    (`native
     (egrep--find-matches--native regexp exts-globs ignored-files-globs root ignore-case))
    (`elisp
     (egrep--find-matches--elisp regexp exts-globs ignored-files-globs root ignore-case))))

(defun egrep--find-matches--native (regexp exts-globs ignored-files-globs root ignore-case)
  (let* ((globs-to-find exts-globs)
         (ignored-files ignored-files-globs)
         (ignored-dirs
          (nconc
           (--map (concat "*/" (strip-trailing-slash it) "*") +ignored-directories+)
           (--map (concat "*/" it "*") +ignored-directory-prefixes+)))
         (matches
          (haskell-native-grep-rec
           (vector root)
           regexp
           (coerce globs-to-find 'vector)
           (coerce ignored-files 'vector)
           (coerce ignored-dirs 'vector)
           ignore-case)))
    (cl-assert (vectorp matches))
    (when (or (null matches)
              (= (length matches) 0))
      (error "No matches for regexp \"%s\" across files %s"
             regexp
             (mapconcat #'identity exts-globs ", ")))
    matches))

(defun egrep--find-matches--elisp (regexp exts-globs ignored-files-globs root ignore-case)
  (let* ((files (find-rec*
                 :root root
                 :globs-to-find exts-globs
                 :ignored-files-globs ignored-files-globs
                 :ignored-directories +ignored-directories+
                 :ignored-directory-prefixes +ignored-directory-prefixes+))
         (files-length (length files))
         (should-report-progress? (<= 100 files-length))
         (progress-reporter
          (when should-report-progress?
            (make-standard-progress-reporter files-length "files")))
         (matches
          (loop
            for filename in (sort files #'string<)
            nconc
            (for-buffer-with-file filename
              (when progress-reporter
                (funcall progress-reporter 1))
              (redisplay t)
              (goto-char (point-min))
              (let* ((result-ptr (cons nil nil))
                     (local-matches result-ptr)
                     (case-fold-search ignore-case)
                     (short-file-name
                      (propertize (file-relative-name filename root)
                                  'face 'compilation-info)))
                (while (re-search-forward regexp nil t)
                  (let* ((match-start (match-beginning 0))
                         (match-end (match-end 0))
                         (formatted
                          (egrep--format-select-entry short-file-name
                                                      match-start
                                                      match-end)))
                    (setf (cdr local-matches)
                          (cons (make-egrep-match
                                 filename
                                 match-start
                                 formatted)
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
    (when should-report-progress?
      (message "Finished looking in files")
      (redisplay t))
    (list->vector matches)))

(defun egrep-search (regexp exts-globs ignored-files-globs dir ignore-case)
  "Search for REGEXP in files under directory DIR that match FILE-GLOBS and don't
match IGNORED-FILE-GLOBS."
  (let* ((get-matches
          (lambda ()
            (egrep--find-matches regexp exts-globs ignored-files-globs dir ignore-case)))
         (matches
          (funcall get-matches))
         (kmap (make-sparse-keymap)))
    (def-keys-for-map kmap
      ("H" (lambda ()
             (interactive)
             (select-mode-update-items (funcall get-matches) 0))))
    (select-mode-start-selection
     matches
     :buffer-name "*grep*"
     :after-init (lambda ()
                   (select-mode-extend-keymap-with kmap))
     :on-selection
     (lambda (idx match selection-type)
       ;; NB Don't call `select-mode-exit' here since we may return to *grep* buffer
       ;; to try out another match.
       (let ((buf (aif (find-buffer-visiting (egrep-match-file match))
                      it
                    (find-file-noselect (egrep-match-file match)))))
         (funcall
          (pcase selection-type
            (`same-window  #'switch-to-buffer)
            (`other-window #'switch-to-buffer-other-window))
          buf)
         (goto-char (egrep-match-start-pos match))))
     :item-show-function
     #'egrep-match-formatted-entry
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

(defvar egrep-tag-default nil)
(defvar egrep-regexp-history nil)

(defun egrep--read-regexp ()
  (read-regexp (pcase egrep-backend
                 (`native
                  "Extended re")
                 (`elisp
                  "Emacs re"))
               'grep-tag-default
               'egrep-regexp-history))

;;;###autoload
(defun egrep (regexp exts-globs dir &optional ignore-case)
  (interactive
   (let ((regexp (egrep--read-regexp)))
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
