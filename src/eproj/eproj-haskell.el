;; eproj-haskell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 February 2015
;; Description:

(require 'eproj)

(defparameter *fast-tags-exec* (executable-find "fast-tags"))

(defun eproj/load-haskell-project (proj make-project-files)
  "Load haskell project PROJ according to definitions in .eproj-info file.

NB MAKE-PROJECT-FILES should be a function of 0 arguments that returns a list of
files.

Note: old tags file is removed before calling update command."
  (assert (eproj-project-p proj))
  (if-let (tag-file-entry (assoc 'tag-file (eproj-project/aux-info proj)))
    (progn
      (unless (= (length tag-file-entry) 2)
        (error "invalid 'tag-file entry in %s project: %s"
               (eproj-project/root proj)
               tag-file-entry))
      (let ((tag-file (cadr tag-file-entry)))
        (assert (stringp tag-file))
        (let ((tag-file-path (eproj-resolve-abs-or-rel-name tag-file
                                                            (eproj-project/root proj))))
          (when (or (null tag-file-path)
                    (not (file-exists-p tag-file-path)))
            (error "Cannot find tag file %s at %s" tag-file tag-file-path))
          (for-buffer-with-file tag-file-path
            (eproj/ctags-get-tags-from-buffer (current-buffer) t)))))
    (progn
      (unless *fast-tags-exec*
        (error "Cannot load haskell project, fast-tags executable not found and no tag-file specified"))
      (with-temp-buffer
        (with-disabled-undo
          (with-inhibited-modification-hooks
            (let ((out-buffer (current-buffer))
                  (ext-re (eproj-language/extension-re
                           (gethash 'haskell-mode eproj/languages-table))))
              (with-temp-buffer
                (with-disabled-undo
                  (with-inhibited-modification-hooks
                    (let ((files
                           (filter
                            (lambda (file)
                              (string-match-pure? ext-re file))
                            (funcall make-project-files))))
                      (when files
                        (insert (car files))
                        (dolist (file (cdr files))
                          (insert "\0" file))))
                    (when (not (= 0
                                  (call-process-region (point-min)
                                                       (point-max)
                                                       *fast-tags-exec*
                                                       nil
                                                       out-buffer
                                                       nil
                                                       "-0"
                                                       "-o-"
                                                       "--nomerge")))
                      (error "fast-tags invokation failed: %s"
                             (with-current-buffer out-buffer
                               (buffer-substring-no-properties (point-min) (point-max)))))
                    (erase-buffer))))
              (eproj/ctags-get-tags-from-buffer out-buffer t)))))
      ;; (message "Warning: no tag file for haskell project %s"
      ;;          (eproj-project/root proj))
      )))

(defun eproj/haskell-tag->string (proj tag)
  (assert (eproj-tag-p tag))
  (let* ((type (cdr-safe (assoc 'type (eproj-tag/properties tag))))
         (is-module?
          (pcase type
            ("m" t)
            (_   nil))))
    (concat (eproj-tag/symbol tag)
            " ["
            (pcase type
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
                      (eproj-tag/properties tag))))
            "]\n"
            (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
                                           (eproj-project/root proj))
            ":"
            (number->string (eproj-tag/line tag))
            "\n"
            (if is-module?
              ""
              (concat
               (eproj/haskell-extract-tag-signature proj tag)
               "\n")))))

(defun eproj/haskell-extract-tag-signature (proj tag)
  "Fetch line where TAG is defined."
  (assert (eproj-tag-p tag) nil "Eproj tag is required.")
  (for-buffer-with-file
      (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
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
      )))

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

(provide 'eproj-haskell)

;; Local Variables:
;; End:

;; eproj-haskell.el ends here
