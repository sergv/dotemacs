;; egrep.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday, 18 February 2016
;; Description:

(eval-when-compile
  (require 'cl)
  (require 'macro-util))

(require 'f)

(require 'common)
(require 'find-files)
(require 'foreign-setup)
(require 'macro-util)

(autoload 'grep-read-files "grep")
(autoload 'grep-read-regexp "grep")


(defvar egrep-backend nil
  "Which immplementation to use to provide grepping capability within Emacs.")

;; Configure explicitly so that dumped emacs will pick it up properly.
(setf egrep-backend
      (cond
        (use-foreign-libraries?
         'native)
        (t
         'elisp)))

(defun make-egrep-match (file short-file-name line column matched-prefix matched-text matched-suffix)
  (cons file (cons short-file-name (cons line (cons column (cons matched-prefix (cons matched-text matched-suffix)))))))

(defsubst egrep-match-file (x)
  (declare (pure t) (side-effect-free t))
  (car x))

(defsubst egrep-match-short-file-name (x)
  (declare (pure t) (side-effect-free t))
  (cadr x))

(defsubst egrep-match-line (x)
  (declare (pure t) (side-effect-free t))
  (caddr x))

(defsubst egrep-match-column (x)
  (declare (pure t) (side-effect-free t))
  (cadddr x))

(defsubst egrep-match-matched-prefix (x)
  (declare (pure t) (side-effect-free t))
  (car (cddddr x)))

(defsubst egrep-match-matched-text (x)
  (declare (pure t) (side-effect-free t))
  (cadr (cddddr x)))

(defsubst egrep-match-matched-suffix (x)
  (declare (pure t) (side-effect-free t))
  (cddr (cddddr x)))

(defun egrep-match< (a b)
  (let ((file-a (egrep-match-short-file-name a))
        (file-b (egrep-match-short-file-name b)))
    (or (string< file-a file-b)
        (and (string= file-a file-b)
             (let ((line-a (egrep-match-line a))
                   (line-b (egrep-match-line b)))
               (or (< line-a line-b)
                   (and (eq line-a line-b)
                        (let ((column-a (egrep-match-column a))
                              (column-b (egrep-match-column b)))
                          (< column-a column-b)))))))))

(defun egrep--format-match-entry (match-entry)
  "Make text entry for current match that can be shown to the user.

MATCH-START and MATCH-END are match bounds in the current buffer"
  (let ((header
         (concat (propertize (egrep-match-short-file-name match-entry)
                             'face 'eproj-symbnav-file-name)
                 ":"
                 (propertize (number->string (egrep-match-line match-entry))
                             'face 'compilation-line-number)
                 ":"
                 (propertize (number->string (egrep-match-column match-entry))
                             'face 'compilation-column-number)
                 ":"))
        (matched-text
         (concat
          (egrep-match-matched-prefix match-entry)
          (propertize (egrep-match-matched-text match-entry)
                      'face
                      'lazy-highlight)
          (egrep-match-matched-suffix match-entry))))
    (let ((header-space (make-string (length header) ?\s))
          (lines (split-into-lines matched-text t)))
      (cl-assert lines)
      (if lines
          (concat (propertize (concat (car lines) "\n")
                              'line-prefix header)
                  (if (cdr lines)
                      (mapconcat (lambda (line)
                                   (propertize (concat line "\n")
                                               'line-prefix header-space))
                                 (cdr lines)
                                 "")
                    ""))
        "!error: no lines in the block!"))))

(defun egrep--find-matches (regexp exts-globs ignored-files-globs root ignore-case)
  (pcase egrep-backend
    (`native
     (egrep--find-matches--native regexp exts-globs ignored-files-globs root ignore-case))
    (`elisp
     (egrep--find-matches--elisp regexp exts-globs ignored-files-globs root ignore-case))))

(defun egrep--find-matches--native (regexp globs-to-find ignored-files-globs root ignore-case)
  (save-some-buffers)
  (let ((matches
         (haskell-native-grep
          (list root)
          regexp
          globs-to-find
          ignored-files-globs
          +ignored-directories+
          +ignored-directory-prefixes+
          nil
          ignore-case)))
    (cl-assert (listp matches))
    (when (or (null matches)
              (= (length matches) 0))
      (error "No matches for regexp \"%s\" across files %s"
             regexp
             (mapconcat #'identity globs-to-find ", ")))
    matches))

(defun egrep--find-matches--elisp (regexp exts-globs ignored-files-globs root ignore-case)
  (save-match-data
    (let* ((files (find-rec*
                   :root root
                   :globs-to-find exts-globs
                   :ignored-files-globs ignored-files-globs
                   :ignored-directories +ignored-directories+
                   :ignored-directory-prefixes +ignored-directory-prefixes+))
           (files-length (length files))
           (should-report-progress? (and (<= 100 files-length) (not noninteractive)))
           (progress-reporter
            (when should-report-progress?
              (make-standard-progress-reporter files-length "files")))
           (matches
            (let ((case-fold-search ignore-case))
              (cl-loop
               for filename in (sort files #'string<)
               nconc
               (for-buffer-with-file filename
                 (save-excursion
                   (when progress-reporter
                     (funcall progress-reporter 1))
                   (redisplay t)
                   (goto-char (point-min))
                   (let* ((res (cons nil nil))
                          (tmp res))
                     (while (re-search-forward regexp nil t)
                       (let* ((match-start (match-beginning 0))
                              (match-end (match-end 0))
                              (line (line-number-at-pos match-start))
                              (column (- match-start (line-beginning-position))))
                         (save-excursion
                           (goto-char match-start)
                           (let ((match-prefix
                                  (buffer-substring (line-beginning-position)
                                                    match-start))
                                 (match-text
                                  (buffer-substring match-start match-end))
                                 (match-suffix
                                  (progn
                                    (goto-char match-end)
                                    (buffer-substring match-end (line-end-position)))))
                             (setf tmp
                                   (setcdr-sure tmp
                                                (cons (make-egrep-match filename
                                                                        (file-relative-name filename root)
                                                                        line
                                                                        column
                                                                        match-prefix
                                                                        match-text
                                                                        match-suffix)
                                                      nil)))))
                         ;; Jump to end of line in order to show at most one match per
                         ;; line.
                         (end-of-line)))
                     (cdr res))))))))
      (when (null matches)
        (error "No matches for regexp \"%s\" across files %s"
               regexp
               (mapconcat #'identity exts-globs ", ")))
      (when should-report-progress?
        (message "Finished looking in files")
        (redisplay t))
      matches)))

(defun egrep-commit-changed-entries ()
  (interactive)
  (let ((changed-entries nil))
    (select-mode-on-selectable-items
     (lambda (match-entry buffer-str)
       (let ((orig-str
              (concat
               (substring-no-properties (egrep-match-matched-prefix match-entry))
               (substring-no-properties (egrep-match-matched-text match-entry))
               (substring-no-properties (egrep-match-matched-suffix match-entry))))
             (stripped-buffer-str (substring (substring-no-properties buffer-str)
                                             0
                                             -1)))
         (when (not (string= orig-str stripped-buffer-str))
           (push (list match-entry orig-str stripped-buffer-str) changed-entries)))))
    (let ((changed-entries-hash-table
           (make-hash-table :test #'equal))
          (ordered-changed-entries
           (make-hash-table :test #'equal)))
      (dolist (entry changed-entries)
        (let* ((match-entry (car entry))
               (file-name (egrep-match-file match-entry)))
          (puthash file-name
                   (cons entry
                         (gethash file-name changed-entries-hash-table nil))
                   changed-entries-hash-table)))
      ;; Sort entries: changed-entries-hash-table -> ordered-changed-entries.
      (maphash (lambda (file-name entries)
                 (puthash file-name
                          (sort entries
                                (lambda (x y)
                                  (let ((x-match (car x))
                                        (y-match (car y)))
                                    ;; Use descending by line numbers order so
                                    ;; that line numbers will not be
                                    ;; invalidated when changes are applied.
                                    (> (egrep-match-line x-match)
                                       (egrep-match-line y-match)))))
                          ordered-changed-entries))
               changed-entries-hash-table)
      ;; Sanity check that changes can be applied - old content didn’t change after we gathered it.
      (maphash
       (lambda (file-name entries)
         (for-buffer-with-file file-name
           (dolist (entry entries)
             (let ((match-entry (car entry))
                   (orig-str (cadr entry)))
               (cl-assert (stringp orig-str))
               (goto-line-dumb (egrep-match-line match-entry))
               (beginning-of-line)
               (let ((current-str
                      (buffer-substring-no-properties (point)
                                                      (+ (point)
                                                         (length orig-str)))))
                 (unless (string= orig-str current-str)
                   (error "Cannot apply changes in file %s at line %s: grep looked at too old version.\nExpected contents:\n%s\nCurrent contents:\n%s\n"
                          (egrep-match-short-file-name match-entry)
                          (egrep-match-line match-entry)
                          orig-str
                          current-str)))))))
       ordered-changed-entries)
      ;; Apply the changes.
      (maphash
       (lambda (file-name entries)
         (with-temp-buffer
           (insert-file-contents file-name
                                 t ;; make current buffer visit inserted file
                                 )
           (dolist (entry entries)
             (let ((match-entry (car entry))
                   (orig-str (cadr entry))
                   (new-contents (caddr entry)))
               (cl-assert (stringp orig-str))
               (cl-assert (stringp new-contents))
               (goto-line-dumb (egrep-match-line match-entry))
               (beginning-of-line)
               (delete-region (point)
                              (+ (point)
                                 (length orig-str)))
               (insert new-contents)))
           (write-region (point-min) (point-max) file-name)
           (set-buffer-modified-p nil)))
       ordered-changed-entries)

      (select-mode-exit))))

(defun egrep-search (regexp exts-globs ignored-files-globs dir ignore-case)
  "Search for REGEXP in files under directory DIR that match
FILE-GLOBS and don't match IGNORED-FILE-GLOBS."
  (let* ((get-matches
          (lambda ()
            (let ((matches
                   (egrep--find-matches regexp exts-globs ignored-files-globs dir ignore-case)))
              (sort (list->vector
                     (remove-duplicates-by-hashing-projections
                      (lambda (match)
                        (cons (egrep-match-line match) (egrep-match-file match)))
                      #'equal
                      matches))
                    #'egrep-match<))))
         (matches
          (funcall get-matches))
         (kmap (make-sparse-keymap)))
    (def-keys-for-map kmap
      ("H"   (lambda ()
               (interactive)
               (let* ((selected-item (aref
                                      (select-mode--state-items select-mode--current-state)
                                      (select-mode--state-selected-item select-mode--current-state)))
                      (new-items (funcall get-matches))
                      (new-idx (v--find-idx (equal it selected-item) new-items)))
                 (select-mode-update-items new-items (or new-idx 0)))))
      ("C-S" #'egrep-commit-changed-entries))
    (select-mode-start-selection
     matches
     :buffer-name "*grep*"
     :enable-undo t
     :after-init (lambda ()
                   (remove-text-properties (point-min) (point-max) '(read-only t))
                   (use-local-map (make-sparse-keymap))
                   (undo-tree-mode +1)
                   (setf vim-normal-mode-local-keymap kmap
                         vim-insert-mode-local-keymap (make-sparse-keymap))
                   (def-keys-for-map vim-normal-mode-local-keymap
                     ("<up>"     select-mode-select-previous-item)
                     ("<down>"   select-mode-select-next-item)
                     ("<return>" select-mode-do-select-same-window)
                     ("SPC"      select-mode-do-select-other-window)
                     ("h"        select-mode-select-next-item)
                     ("t"        select-mode-select-previous-item)
                     ("<escape>" select-mode-exit))
                   (def-keys-for-map (vim-normal-mode-local-keymap
                                      vim-insert-mode-local-keymap)
                     ("C-g" select-mode-exit))
                   ;; Required to make additions made to `vim-normal-mode-local-keymap'
                   ;; effective immediately.
                   (vim-activate-normal-mode))
     :on-selection
     (lambda (_idx match selection-type)
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
         (goto-line-dumb (egrep-match-line match))
         (beginning-of-line)
         (forward-char (egrep-match-column match))))
     :item-show-function
     #'egrep--format-match-entry
     :preamble
     (propertize
      "\n"
      'display
      (format "Browse matches for ‘%s’ in files matching %s starting at directory %s\n\n"
              regexp
              (mapconcat #'identity exts-globs " ")
              dir)
      'read-only t)
     :working-directory dir
     :read-only nil)))

(defun egrep--read-files (regexp)
  "Query user for list of glob patterns to search in and return list of
string patterns."
  (split-string (grep-read-files regexp)
                "[ \t\n\r\f\v]+"
                t))

(defvar egrep-tag-default nil)
(defvar egrep-regexp-history nil)

(autoload 'grep-tag-default "grep" nil nil)

(defun egrep--tag-default-quoted ()
  (awhen (grep-tag-default)
    (regexp-quote it)))

(defun egrep--read-regexp ()
  (read-regexp (pcase egrep-backend
                 (`native
                  "Extended re")
                 (`elisp
                  "Emacs re"))
               #'egrep--tag-default-quoted
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
  (egrep-search (expand-escape-sequences regexp)
                exts-globs
                grep-find-ignored-files
                dir
                ignore-case))

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
