;; eproj-haskell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 February 2015
;; Description:

(require 'common-heavy)
(require 'eproj)
(require 'eproj-ctags)
(require 'haskell-watch)

(defparameter *fast-tags-exec* (executable-find "fast-tags"))

;;;###autoload
(defun eproj/create-haskell-tags (proj project-files-thunk parse-tags-proc)
  (cl-assert (eproj-project-p proj))
  ;; (when eproj-verbose-tag-loading
  ;;   (notify "Creating haskell tags for project %s" (eproj-project/root proj)))
  (unless *fast-tags-exec*
    (error "Cannot load haskell project, fast-tags executable not found (and no tag-file specified)"))
  (with-temp-buffer
    (with-disabled-undo
     (with-inhibited-modification-hooks
      (let ((out-buffer (current-buffer))
            (ext-re (eproj-language/extension-re
                     (gethash 'haskell-mode eproj/languages-table))))
        (with-temp-buffer
          (with-disabled-undo
           (with-inhibited-modification-hooks
            (dolist (file (eproj-thunk-get-value project-files-thunk))
              (when (string-match-p ext-re file)
                (insert file "\n")))
            (unless (= 0
                       (call-process-region (point-min)
                                            (point-max)
                                            *fast-tags-exec*
                                            nil
                                            ;; Discard error output from fast-tags
                                            (list out-buffer nil)
                                            nil
                                            "-o-"
                                            "--nomerge"
                                            "-"))
              (error "fast-tags invokation failed: %s"
                     (with-current-buffer out-buffer
                       (buffer-substring-no-properties (point-min) (point-max)))))
            (erase-buffer))))
        (funcall parse-tags-proc out-buffer))))))

;;;###autoload
(defun eproj/get-fast-tags-tags-from-buffer (buffer)
  "Constructs hash-table of (tag . eproj-tag) bindings extracted from buffer BUFFER.
BUFFER is expected to contain simplified output of ctags - fast-tags command.

Function does not attempt to parse <key>=<value> pairs after ;\",
and expects single character there instead (this isn't be checked at
runtime but rather will be silently relied on)."
  (with-current-buffer buffer
    (save-match-data
      (goto-char (point-min))
      (let ((tags-tables
             (mk-nested-hash-tables
              (list
               (list #'(lambda (tag)
                         (cdr (assq 'type (eproj-tag/properties tag))))
                     #'equal)
               (list #'(lambda (tag)
                         (list (eproj-tag/symbol tag)
                               (eproj-tag/file tag)))
                     #'equal))))
            (gc-cons-threshold (min (* 100 1024 1024)
                                    (max gc-cons-threshold
                                         ;; Every 1000 lines takes up 1 mb or so.
                                         (/ (* (count-lines (point-min) (point-max)) 1024 1024)
                                            1000))))
            (progress-reporter (when eproj-verbose-tag-loading
                                 (let ((total-tags-count (count-lines (point-min) (point-max))))
                                   (make-standard-progress-reporter total-tags-count "tags")))))
        (garbage-collect)
        (while (not (eobp))
          (beginning-of-line)
          (when (and (not (looking-at-p "^!_TAG_")) ;; skip metadata
                     (looking-at eproj-ctags--line-re))
            (let ((symbol (eproj-ctags--cache-string
                           (match-string-no-properties 1)))
                  (file (eproj-ctags--cache-string
                         (match-string-no-properties 2)))
                  (line (string->number (match-string-no-properties 3))))
              (goto-char (match-end 0))
              ;; now we're past ;"
              (let* ((fields-str (buffer-substring-no-properties
                                  (point)
                                  (line-end-position)))
                     (fields
                      (list (cons 'type
                                  (eproj-ctags--cache-string
                                   (trim-whitespace fields-str)))))
                     (new-tag (make-eproj-tag
                               symbol
                               file
                               line
                               fields)))
                (nested-hash-tables/add!
                 new-tag
                 tags-tables)
                ;; (puthash symbol
                ;;          (cons new-tag (gethash symbol tags-table nil))
                ;;          tags-table)
                )))
          (forward-line 1)
          (when eproj-verbose-tag-loading
            (funcall progress-reporter 1)))
        (let* ((result-table (make-hash-table :test #'equal :size 997))
               (add-tag-to-result
                (lambda (tag)
                  (puthash (eproj-tag/symbol tag)
                           (cons tag
                                 (gethash (eproj-tag/symbol tag)
                                          result-table
                                          nil))
                           result-table)))
               (data (nested-hash-tables/data tags-tables))
               (constructors-tags
                (gethash "C" data nil))
               (type-tags
                (gethash "t" data nil)))
          (remhash "C" data)
          ;; Add all non-constructor tags.
          (maphash (lambda (_ tags-table)
                     (maphash (lambda (k tag)
                                (funcall add-tag-to-result tag))
                              tags-table))
                   data)
          ;; Add constructor tags while filter out thouse that are already covered
          ;; by type tags.
          (when (and constructors-tags
                     type-tags)
            (maphash (lambda (name-and-file tag)
                       (unless (gethash name-and-file type-tags)
                         (funcall add-tag-to-result tag)))
                     constructors-tags))
          result-table)))))


(defun eproj/haskell-tag-kind (tag)
  (pcase (cdr-safe (assq 'type (eproj-tag/properties tag)))
    ("m" "Module")
    ("f" "Function")
    ("c" "Class")
    ("t" "Type")
    ("C" "Constructor")
    ("o" "Operator")
    ("p" "Pattern")
    ("F" "Type family")
    (_
     (error "Invalid haskell tag property %s"
            (eproj-tag/properties tag)))))

;;;###autoload
(defun eproj/haskell-tag->string (proj tag)
  (cl-assert (eproj-tag-p tag))
  (let* ((type (cdr-safe (assq 'type (eproj-tag/properties tag))))
         (is-module?
          (pcase type
            ("m" t)
            (_   nil))))
    (concat (eproj-tag/symbol tag)
            " ["
            (eproj/haskell-tag-kind tag)
            "]\n"
            (eproj--resolve-to-abs-path (eproj-tag/file tag)
                                        (eproj-project/root proj))
            ":"
            (number->string (eproj-tag/line tag))
            "\n"
            (awhen (eproj/haskell-extract-tag-signature proj tag)
              (concat it "\n")))))

(defun eproj/haskell-extract-tag-signature (proj tag)
  "Fetch line where TAG is defined."
  (cl-assert (eproj-tag-p tag) nil "Eproj tag is required.")
  (let* ((type (cdr-safe (assq 'type (eproj-tag/properties tag))))
         (is-module?
          (pcase type
            ("m" t)
            (_   nil))))
    (unless is-module?
      (for-buffer-with-file
          (eproj--resolve-to-abs-path (eproj-tag/file tag)
                                      (eproj-project/root proj))
        (save-excursion
          (goto-line1 (eproj-tag/line tag))
          (eproj/haskel-extract-block)
          ;; alternative implementation with regexps
          ;; (save-match-data
          ;;   (goto-line1 (eproj-tag/line tag))
          ;;   (if (looking-at "^\\([^ \t\n\r\f\v].* ::\\(?: .*\n\\|\n\\)\\(?:^[ \t]+.+\n\\)*\\)")
          ;;     (match-string-no-properties 1)
          ;;     (current-line)))
          )))))

(defun eproj/haskel-extract-block ()
  "Extract indented Haskell block that starts on the current line."
  (beginning-of-line)
  (let ((start (point)))
    (cl-symbol-macrolet
        ((advance
          (progn
            (forward-line 1)
            (beginning-of-line)
            (skip-chars-forward " \t"))))
      (skip-chars-forward " \t")
      (let ((col (current-column)))
        ;; actualy this is a loop with postcondition
        advance
        (while (< col (current-column))
          advance)
        (let ((previous-line-end (line-end-position 0)))
          (buffer-substring-no-properties start previous-line-end))))))

;;;###autoload
(defun eproj/haskell-get-extra-navigation-files (proj)
  (when-let (watch-project
             (haskell-watch-get-project (eproj-project/root proj)))
    (let ((res nil))
      (maphash (lambda (key value)
                 (push key res))
               (haskell-watched-project/watched-files watch-project))
      res)))

(provide 'eproj-haskell)

;; Local Variables:
;; End:

;; eproj-haskell.el ends here
