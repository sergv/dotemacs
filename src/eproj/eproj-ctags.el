;; eproj-ctags.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 16 February 2016
;; Description:

;;;; ctags facility

(require 'common)
(require 'eproj-tag-index)

(eval-when-compile (require 'subr-x))

(defparameter eproj-ctags--exec
  (or (let ((ctags-exec
             (platform-dependent-executable (concat +execs-path+ "/exuberant-ctags"))))
        (when (and ctags-exec
                   (file-exists-p ctags-exec))
          ctags-exec))
      (cached-executable-find "ctags-exuberant")
      (cached-executable-find "exuberant-ctags")))

(defparameter *ctags-language-flags*
  '((c-mode
     "--language-force=c"
     "--c-kinds=-defgmpstuv"
     "--c-kinds=+defgmstuv"
     "--fields=+SzkK"
     "--extras=+q")
    (c++-mode
     "--language-force=c++"
     "--c++-kinds=+cdefgmnpstuv"
     "--fields=+iaSzkK"
     "--extras=+q")
    (python-mode
     "--language-force=python"
     "--python-kinds=+cfmvi"
     "--fields=+SzkK")
    (java-mode
     "--language-force=java"
     "--java-kinds=+cefgimp"
     "--fields=+iaSzkK")))

(defconst eproj-ctags--line-re
  (rx bol
      ;; tag name, *can* contain spaces
      ;; (cf C++'s "operator =" tag produced by ctags.)
      (group (+ (not (any ?\t ?\n))))
      "\t"
      ;; filename, *can* contain spaces
      (group (+ (not (any ?\t ?\n))))
      "\t"
      (or (group (+ digit))
          (or (seq "/^"
                   (+ (or (not (any ?\n ?/))
                          "\\\\/"))
                   "$/")
              (seq "?^"
                   (+ (or (not (any ?\n ?/))
                          "\\/"))
                   "$?")))
      (or (seq (* (any ?\s ?\t))
               ";\"")
          eol)))

(defconst eproj-ctags--aux-fields-re
  (eval-when-compile
    (concat "\\=\\("
            (eval-when-compile
              (regexp-opt
               '("kind"
                 "access"
                 "class"
                 "file"
                 "signature"
                 "namespace"
                 "struct"
                 "enum"
                 "union"
                 "inherits"
                 "typeref"
                 "function"
                 "interface"
                 "annotation")))
            "\\):\\(.*\\)")))

;;;###autoload
(defun eproj/run-ctags-on-files (lang-mode root-dir files out-buffer)
  (unless eproj-ctags--exec
    (error "ctags executable not found"))
  (with-current-buffer out-buffer
    (goto-char (point-max))
    (unless (looking-at-p "^$")
      (insert "\n"))
    (let ((ext-re (eproj-language/extension-re
                   (gethash lang-mode eproj/languages-table))))
      (with-temp-buffer
        (with-disabled-undo
         (with-inhibited-modification-hooks
          (cd root-dir)
          (dolist (file files)
            (when (string-match-p ext-re file)
              (insert file "\n")))
          (let* ((args
                  (append
                   (list "-o"
                         "-"
                         "-L"
                         "-"
                         "--excmd=number"
                         "--sort=no")
                   (aif (cdr-safe (assq lang-mode *ctags-language-flags*))
                       it
                     (error "unknown ctags language: %s" lang-mode))))
                 (exit-status
                  (apply #'call-process-region
                         (point-min)
                         (point-max)
                         eproj-ctags--exec
                         nil
                         out-buffer
                         nil
                         args)))
            (when (or (not (numberp exit-status))
                      (not (= 0 exit-status)))
              (error "Call to ctags failed.\nMode: %s\nExtension regexp: %s\nExit status: %s\nOutput: %s\nCommand: %s"
                     lang-mode
                     ext-re
                     exit-status
                     (with-current-buffer out-buffer
                       (buffer-substring-no-properties (point-min) (point-max)))
                     (cons eproj-ctags--exec args))))))))))

(defparameter eproj/ctags-string-cache
  (make-hash-table :test #'equal :size 997 :weakness t))

(defsubst eproj-ctags--cache-string (x)
  (assert (stringp x))
  (if-let (cached-x (gethash x eproj/ctags-string-cache))
      cached-x
    (puthash x x eproj/ctags-string-cache)))

;; tags parsing
;;;###autoload
(defun eproj/ctags-get-tags-from-buffer (proj-root buffer)
  "Constructs hash-table of (tag . eproj-tag) bindings extracted from buffer BUFFER.
BUFFER is expected to contain output of ctags command."
  (with-current-buffer buffer
    (save-match-data
      (goto-char (point-min))
      (let ((tags-index (empty-eproj-tag-index))
            (field-cache (make-hash-table :test #'equal))
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
              (let* ((line-end-pos (line-end-position))
                     (fields nil))
                (while (< (point) line-end-pos)
                  (skip-chars-forward "\t")
                  (let ((start (point)))
                    (skip-chars-forward "^\t\n")
                    (let ((end (point)))
                      (save-excursion
                        (goto-char start)
                        (if (re-search-forward eproj-ctags--aux-fields-re end t)
                            (let ((identifier (match-string-no-properties 1))
                                  (value (match-string-no-properties 2)))
                              ;; when value is nonempty
                              (when (not (string= "" value))
                                (let ((new-field (cons (string->symbol identifier)
                                                       (eproj-ctags--cache-string value))))
                                  (push (aif (gethash new-field field-cache)
                                            it
                                          (puthash new-field new-field field-cache))
                                        fields))))
                          (error "Invalid ctags entry: %s" (buffer-substring-no-properties start end)))))))
                (forward-char)
                (eproj-tag-index-add! symbol
                                      file
                                      line
                                      fields
                                      tags-index)))
            (when eproj-verbose-tag-loading
              (funcall progress-reporter 1))))
        tags-index))))

(provide 'eproj-ctags)

;; Local Variables:
;; End:

;; eproj-ctags.el ends here
