;; eproj-ctags.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 16 February 2016
;; Description:

;;;; ctags facility

(eval-when-compile (require 'subr-x))

(defparameter *ctags-exec*
  (or (executable-find "exuberant-ctags")
      (executable-find "ctags-exuberant")))

(defparameter *ctags-language-flags*
  '((c-mode
     "--language-force=c"
     "--c-kinds=-cdefgmnpstuv"
     "--c-kinds=+defgmstuv"
     "--fields=+SzkK"
     "--extra=+q")
    (c++-mode
     "--language-force=c++"
     "--c++-kinds=+cdefgmnpstuv"
     "--fields=+iaSzkK"
     "--extra=+q")
    (python-mode
     "--language-force=python"
     "--python-kinds=+cfmvi"
     "--fields=+SzkK")
    (java-mode
     "--language-force=java"
     "--java-kinds=+cefgimp"
     "--fields=+iaSzkK")))

(defconst +ctags-line-re+
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

(defconst +ctags-aux-fields+
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
    "interface"))

(setf +ctags-aux-fields-re+
      (eval-when-compile
        ;; (concat "^\\("
        ;;         (macroexpand
        ;;          `(rx (or ,@+ctags-aux-fields+)))
        ;;         "\\):\\(.*\\)$")
        (concat "\\=\\("
                (macroexpand
                 `(rx (or ,@+ctags-aux-fields+)))
                "\\):\\(.*\\)")))

(defun eproj/run-ctags-on-files (lang-mode root-dir files out-buffer)
  (unless *ctags-exec*
    (error "ctags executable not found"))
  (with-current-buffer out-buffer
    (goto-char (point-max))
    (unless (looking-at-pure? "^$")
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
          (when (not (= 0
                        (apply #'call-process-region
                               (point-min)
                               (point-max)
                               *ctags-exec*
                               nil
                               out-buffer
                               nil
                               "-f"
                               "-"
                               "-L"
                               "-"
                               "--excmd=number"
                               (aif (rest-safe (assq lang-mode *ctags-language-flags*))
                                    it
                                    (error "unknown ctags language: %s" lang-mode)))))
            (error "ctags invokation failed: %s"
                   (with-current-buffer out-buffer
                     (buffer-substring-no-properties (point-min) (point-max)))))))))))

(defparameter eproj/ctags-string-cache
  (make-hash-table :test #'equal :size 997 :weakness t))

(defsubst eproj/ctags-cache-string (x)
  (assert (stringp x))
  (if-let (cached-x (gethash x eproj/ctags-string-cache))
      cached-x
    (puthash x x eproj/ctags-string-cache)))

;; tags parsing
(defun eproj/ctags-get-tags-from-buffer (buffer)
  "Constructs hash-table of (tag . eproj-tag) bindings extracted from buffer BUFFER.
BUFFER is expected to contain output of ctags command."
  (with-current-buffer buffer
    (save-match-data
      (goto-char (point-min))
      (let ((tags-table (make-hash-table :test #'equal :size 997))
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
        (while (not (eobp))
          (when (and (not (looking-at-pure? "^!_TAG_")) ;; skip metadata
                     (looking-at +ctags-line-re+))
            (let ((symbol (eproj/ctags-cache-string
                           (match-string-no-properties 1)))
                  (file (eproj/ctags-cache-string
                         (match-string-no-properties 2)))
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
                        (if (re-search-forward +ctags-aux-fields-re+ end t)
                            (let ((identifier (match-string-no-properties 1))
                                  (value (match-string-no-properties 2)))
                              ;; when value is nonempty
                              (when (not (string= "" value))
                                (let ((new-field (cons (string->symbol identifier)
                                                       (eproj/ctags-cache-string value))))
                                  (push (aif (gethash new-field field-cache)
                                             it
                                             (puthash new-field new-field field-cache))
                                        fields))))
                          (error "invalid entry: %s" (buffer-substring-no-properties start end)))))))
                (forward-char)
                (puthash symbol
                         (cons (make-eproj-tag
                                symbol
                                file
                                line
                                fields)
                               (gethash symbol tags-table nil))
                         tags-table)))

            ;; (forward-line 1)
            (when eproj-verbose-tag-loading
              (funcall progress-reporter 1))))
        tags-table))))

(provide 'eproj-ctags)

;; Local Variables:
;; End:

;; eproj-ctags.el ends here
