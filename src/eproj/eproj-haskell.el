;; eproj-haskell.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 28 February 2015
;; Description:

(require 'common-heavy)
(require 'eproj)
(require 'eproj-ctags)
(require 'eproj-tag-index)
(require 'haskell-watch)

;;;###autoload
(defun eproj/create-haskell-tags (proj project-files-thunk parse-tags-proc)
  (with-temp-buffer
    (with-disabled-undo
     (with-inhibited-modification-hooks
      (let ((out-buffer (current-buffer))
            (ext-re (eproj-language/extension-re
                     (gethash 'haskell-mode eproj/languages-table)))
            (fast-tags-exe
             (cached-executable-find "fast-tags")))
        (unless fast-tags-exe
          (error "Fast tags executable not found"))
        (unless (file-executable-p fast-tags-exe)
          (error "Fast tags executable does not exist: %s"
                 fast-tags-exe))
        (with-temp-buffer
          (with-disabled-undo
           (with-inhibited-modification-hooks
            (dolist (file (eproj-thunk-get-value project-files-thunk))
              (when (string-match-p ext-re file)
                (insert file "\n")))
            (unless (= 0
                       (call-process-region
                        (point-min)
                        (point-max)
                        fast-tags-exe
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
        (funcall parse-tags-proc (eproj-project/root proj) out-buffer))))))

;;;###autoload
(defun eproj/get-fast-tags-tags-from-buffer (proj-root buffer)
  "Constructs hash-table of (tag . eproj-tag) bindings extracted from buffer BUFFER.
BUFFER is expected to contain simplified output of ctags - fast-tags command.

Function does not attempt to parse <key>=<value> pairs after ;\",
and expects single character there instead (this isn't be checked at
runtime but rather will be silently relied on)."
  (with-current-buffer buffer
    (save-match-data
      (goto-char (point-min))
      (let ((tags-index (empty-eproj-tag-index))
            (gc-cons-threshold (min (* 100 1024 1024)
                                    (max gc-cons-threshold
                                         ;; Every 1000 lines takes up 1 mb or so.
                                         (/ (* (count-lines (point-min) (point-max)) 1024 1024)
                                            1000))))
            (progress-reporter (when eproj-verbose-tag-loading
                                 (let ((total-tags-count (count-lines (point-min) (point-max))))
                                   (make-standard-progress-reporter total-tags-count "tags")))))
        (garbage-collect)
        (while (looking-at-p "^!_TAG_")
          (forward-line 1))
        (while (not (eobp))
          (beginning-of-line)
          (when (looking-at eproj-ctags--line-re)
            (let ((symbol (eproj-ctags--cache-string
                           (match-string-no-properties 1)))
                  (file (eproj-ctags--cache-string
                         (eproj-normalise-file-name-expand-cached
                          (match-string-no-properties 2)
                          proj-root)))
                  (line (string->number (match-string-no-properties 3))))
              (goto-char (match-end 0))
              ;; now we're past ;"
              (skip-chars-forward "\t")
              (let ((type (char-after (point))))
                (eproj-tag-index-add! symbol
                                      file
                                      line
                                      type
                                      nil
                                      tags-index))))
          (forward-line 1)
          (when eproj-verbose-tag-loading
            (funcall progress-reporter 1)))
        tags-index))))

;;;###autoload
(defun eproj/haskell-tag-kind (tag)
  (cl-assert (eproj-tag-p tag) nil "Invalid tag: %s" tag)
  (aif (eproj-tag/type tag)
      (pcase it
        (?m "Module")
        (?f "Function")
        (?c "Class")
        (?t "Type")
        (?C "Constructor")
        (?o "Operator")
        (?p "Pattern")
        (?F "Type family")
        (invalid
         (error "Invalid Haskell tag type %c" invalid)))
    "Unknown"))

;;;###autoload
(defun eproj/haskell-tag->string (proj tag-name tag)
  (cl-assert (eproj-tag-p tag))
  (concat tag-name
          " ["
          (eproj/haskell-tag-kind tag)
          "]\n"
          (eproj--resolve-to-abs-path (eproj-tag/file tag)
                                      (eproj-project/root proj))
          ":"
          (number->string (eproj-tag/line tag))
          (awhen (eproj-tag/column tag)
            (concat ":" (number->string it)))
          "\n"
          (awhen (eproj/haskell-extract-tag-signature proj tag)
            (concat it "\n"))))

;;;###autoload
(defun eproj/haskell-extract-tag-signature (proj tag)
  "Fetch line where TAG is defined."
  (cl-assert (eproj-tag-p tag) nil "Eproj tag is required.")
  (let ((is-module?
         (pcase (eproj-tag/type tag)
           (?m t)
           (_  nil))))
    (unless is-module?
      (for-buffer-with-file
          (eproj--resolve-to-abs-path (eproj-tag/file tag)
                                      (eproj-project/root proj))
        (save-excursion
          (goto-line-dumb (eproj-tag/line tag))
          (eproj/haskel-extract-block)
          ;; alternative implementation with regexps
          ;; (save-match-data
          ;;   (goto-line-dumb (eproj-tag/line tag))
          ;;   (if (looking-at "^\\([^ \t\n\r\f\v].* ::\\(?: .*\n\\|\n\\)\\(?:^[ \t]+.+\n\\)*\\)")
          ;;     (match-string-no-properties 1)
          ;;     (current-line)))
          )))))

;;;###autoload
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
             (ignore-errors
               (haskell-watch-get-project (eproj-project/root proj))))
    (let ((res nil)
          (cabal-project
           (concat (eproj-project/root proj) "/cabal.project")))
      (maphash (lambda (key _value)
                 (push key res))
               (haskell-watched-project/watched-files watch-project))
      (when (file-exists-p cabal-project)
        (push cabal-project res))
      res)))

(provide 'eproj-haskell)

;; Local Variables:
;; End:

;; eproj-haskell.el ends here
