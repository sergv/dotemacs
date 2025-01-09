;; eproj-ctags.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 16 February 2016
;; Description:

;;;; ctags facility

(eval-when-compile
  (require 'macro-util)
  (require 'set-up-platform))

(require 'common)
(require 'eproj-tag-index)
(require 'set-up-paths)

(eval-when-compile (require 'subr-x))

(defvar eproj-ctags--exec
  (or (let ((universal-ctags-exec
             (platform-dependent-executable (concat +execs-path+ "/universal-ctags")))
            (ctags-universal-exec
             (platform-dependent-executable (concat +execs-path+ "/ctags-universal")))
            (ctags-exec
             (platform-dependent-executable (concat +execs-path+ "/ctags"))))
        (or
         (when (and universal-ctags-exec
                    (file-exists-p universal-ctags-exec))
           universal-ctags-exec)
         (when (and ctags-universal-exec
                    (file-exists-p ctags-universal-exec))
           ctags-universal-exec)
         (when (and ctags-exec
                    (file-exists-p ctags-exec))
           ctags-exec)))
      (cached-executable-find "universal-ctags")
      (cached-executable-find "ctags-universal")
      (cached-executable-find "ctags")))

(defvar *ctags-language-flags*
  '((c-mode
     "--language-force=c"
     "--c-kinds=-defgmpstuv"
     "--c-kinds=+defgmstuv"
     "--fields=+k"
     "--extras=+q")
    (c++-mode
     "--language-force=c++"
     "--c++-kinds=+cdefgmnpstuv"
     "--fields=+kia"
     "--extras=+q")
    (python-mode
     "--language-force=python"
     "--python-kinds=+cfmvi"
     "--fields=+k")
    (java-mode
     "--language-force=java"
     "--java-kinds=+cefgimp"
     "--fields=+kia")
    (kotlin-mode
     "--language-force=kotlin"
     "--kotlin-kinds=+picomTCv"
     "--fields=+kia")
    (rust-mode
     "--language-force=rust"
     "--fields=+k")))

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

;;;###autoload
(defun eproj/run-ctags-on-files (lang-mode root-dir files out-buffer)
  (unless eproj-ctags--exec
    (error "ctags executable not found"))
  (unless (file-executable-p eproj-ctags--exec)
    (error "ctags executable does not exist: %s" eproj-ctags--exec))
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
          (with-temporary-file stderr "eproj-ctags-errors" nil nil
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
                           nil ;; delete
                           (list out-buffer stderr)
                           nil ;; display
                           args))
                   (stderr-contents (with-temp-buffer
                                      (insert-file-contents stderr nil nil nil t)
                                      (buffer-substring-no-properties (point-min) (point-max)))))
              (when (or (not (numberp exit-status))
                        (not (= 0 exit-status)))
                (error "Call to ctags failed.\nMode: %s\nExtension regexp: %s\nExit status: %s\nOutput: %s\nStderr: %s\nCommand: %s"
                       lang-mode
                       ext-re
                       exit-status
                       (with-current-buffer out-buffer
                         (buffer-substring-no-properties (point-min) (point-max)))
                       stderr-contents

                       (cons eproj-ctags--exec args)))
              (unless (= 0 (length stderr-contents))
                (error "ctags reports on stderr:\n%s" stderr-contents))))))))))

(defsubst eproj-ctags--share (x cache)
  (if-let (cached-x (gethash x cache))
      cached-x
    (puthash x x cache)))

(defsubst eproj-ctags--make-sharing-cache ()
  (make-hash-table :test #'equal :size 997))

;; tags parsing
;;;###autoload
(defun eproj/ctags-get-tags-from-buffer (proj-root buffer tags-source)
  "Constructs hash-table of (tag . eproj-tag) bindings extracted from buffer BUFFER.
BUFFER is expected to contain output of ctags command."
  (declare (ignore tags-source))
  (with-current-buffer buffer
    (save-match-data
      (goto-char (point-min))
      (let ((tags-index (empty-eproj-tag-index))
            (field-cache (make-hash-table :test #'equal))
            (gc-cons-threshold (cap-floor
                                   (* 100 1024 1024)
                                   gc-cons-threshold
                                 ;; Every 1000 lines takes up 1 mb or so.
                                 (/ (* (count-lines-fixed (point-min) (point-max)) 1024 1024)
                                    1000)))
            (progress-reporter (when eproj-verbose-tag-loading
                                 (let ((total-tags-count (count-lines-fixed (point-min) (point-max))))
                                   (make-standard-progress-reporter total-tags-count "tags"))))
            (file-name-cache (eproj-normalise-file-name-expand-cached/make-cache))
            (sharing-cache (eproj-ctags--make-sharing-cache)))
        (garbage-collect)
        (while (looking-at-p "^!_TAG_")
          (forward-line 1))
        (while (not (eobp))
          (when (looking-at eproj-ctags--line-re)
            (let ((symbol (match-string-no-properties 1))
                  (file (eproj-ctags--share
                         (eproj-normalise-file-name-expand-cached/with-explicit-cache
                          file-name-cache
                          (match-string-no-properties 2)
                          proj-root)
                         sharing-cache))
                  (line (string->number (match-string-no-properties 3))))
              (goto-char (match-end 0))
              ;; now we're past ;"
              (skip-chars-forward "\t")
              (let* ((type (char-after (point)))
                     (line-end-pos (line-end-position))
                     (fields nil))
                (forward-char)
                (while (< (point) line-end-pos)
                  (skip-chars-forward "\t")
                  (let ((start (point)))
                    (skip-chars-forward "^:\t\n")
                    (let ((key (buffer-substring-no-properties start (point))))
                      (forward-char)
                      (let ((start (point)))
                        (skip-chars-forward "^\t\n")
                        (let ((value (buffer-substring-no-properties start (point))))
                          ;; When value is nonempty
                          (unless (string-equal "" value)
                            (let ((new-field (cons (string->symbol key)
                                                   (eproj-ctags--share value sharing-cache))))
                              (push (aif (gethash new-field field-cache)
                                        it
                                      (puthash new-field new-field field-cache))
                                    fields))))))))
                (forward-char)
                (eproj-tag-index-add! symbol
                                      file
                                      line
                                      type
                                      (eproj-ctags--share fields sharing-cache)
                                      tags-index)))
            (when eproj-verbose-tag-loading
              (funcall progress-reporter 1))))
        tags-index))))

;;;; Rust tags

(defun eproj/rust-tag-kind (tag)
  (cl-assert (eproj-tag-p tag) nil "Invalid tag: %s" tag)
  (aif (eproj-tag/type tag)
      (pcase it
        (?n "module")
        (?s "structure")
        (?i "trait interface")
        (?c "implementation")
        (?f "function")
        (?g "enum")
        (?t "type alias")
        (?v "global variable")
        (?M "macro definition")
        (?m "struct field")
        (?e "enum variant")
        (?P "method")
        (invalid
         (error "Invalid Rust tag type %c" invalid)))
    "Unknown"))

;;;###autoload
(defun eproj/rust-tag->string (proj tag-name tag)
  (cl-assert (eproj-tag-p tag))
  (concat tag-name
          (awhen (eproj/rust-tag-kind tag)
            (concat " [" it "]"))
          "\n"
          (eproj/format-tag-path-and-line proj tag)
          "\n"
          (eproj/extract-tag-line proj tag)
          "\n"))

;;;; C/C++ tags

(defun eproj/c-tag-kind (tag)
  (cl-assert (eproj-tag-p tag) nil "Invalid tag: %s" tag)
  (aif (eproj-tag/type tag)
      (pcase it
        (?d "macro definition")
        (?e "enumerated value")
        (?f "function definition")
        (?g "enumeration")
        (?h "included header")
        (?l "local variable")
        (?m "member")
        (?p "function prototype")
        (?s "structure")
        (?t "typedef")
        (?u "union")
        (?v "variable")
        (?L "goto label")
        (?c "class")
        (?n "namespace")
        (?A "namespace aliase")
        (?Z "template parameter")
        (invalid
         (error "Invalid C tag type %c" invalid)))
    "Unknown"))

;;;###autoload
(defun eproj/c-tag->string (proj tag-name tag)
  (cl-assert (eproj-tag-p tag))
  (concat tag-name
          (awhen (eproj/c-tag-kind tag)
            (concat " [" it "]"))
          "\n"
          (eproj/format-tag-path-and-line proj tag)
          "\n"
          (eproj/extract-tag-line proj tag)
          "\n"))

;;;; Java tags

(defun eproj/java-tag-kind (tag)
  (cl-assert (eproj-tag-p tag) nil "Invalid tag: %s" tag)
  (aif (eproj-tag/type tag)
      (concat
       (pcase it
         (?a "annotation")
         (?c "class")
         (?e "enum constant")
         (?f "field")
         (?g "enum type")
         (?i "interface")
         (?m "method")
         (?p "package")
         (invalid
          (error "Invalid Java tag type %c" invalid)))
       (awhen (eproj-tag/get-prop 'access tag)
         (concat "/" it)))
    "Unknown"))

;;;###autoload
(defun eproj/java-tag->string (proj tag-name tag)
  (cl-assert (eproj-tag-p tag))
  (concat tag-name
          " ["
          (eproj/java-tag-kind tag)
          "]\n"
          (awhen (eproj-tag/get-prop 'class tag)
            (concat it
                    "."
                    tag-name
                    "\n"))
          (eproj/format-tag-path-and-line proj tag)
          "\n"
          (when (eproj-tag/line tag)
            (concat (eproj/extract-tag-line proj tag)
                    "\n"))))

;;;; Generic tags

(defun eproj/generic-tag-kind (tag)
  (concat (awhen (eproj-tag/type tag)
            (format "%c " it))
          (format "%s" (eproj-tag/properties tag))))

;;;###autoload
(defun eproj/generic-tag->string (proj tag-name tag)
  (cl-assert (eproj-tag-p tag))
  (concat tag-name
          "\n"
          (eproj/format-tag-path-and-line proj tag)
          "\n"
          (eproj/generic-tag-kind tag)
          "\n"))

;;;; Tag presentation utilities

;;;###autoload
(defun eproj/extract-tag-line (proj tag)
  "Fetch line where TAG is defined."
  (cl-assert (eproj-tag-p tag) nil "Eproj tag is required.")
  (for-buffer-with-file
      (eproj-resolve-to-abs-path (eproj-tag/file tag) proj)
    (with-inhibited-field-text-motion
      (save-excursion
        (goto-line-dumb (eproj-tag/line tag))
        (current-line-with-properties)))))

(defun eproj/format-tag-path-and-line (proj tag)
  (let* ((tag-file (eproj-tag/file tag))
         (root (eproj-project/root proj))
         (filename
          (if (string-prefix-p root
                               tag-file
                               (fold-platform-os-type nil t) ;; case insensitivity
                               )
              ;; Add 1 to remove leading slash.
              (substring-no-properties tag-file (1+ (length root)))
            (abbreviate-file-name tag-file
             (eproj-resolve-to-abs-path tag-file proj)))))
    (concat
     (propertize filename 'face 'eproj-symbnav-file-name)
     ":"
     (propertize (number->string (eproj-tag/line tag))
                 'face 'eproj-symbnav-line-number)
     (awhen (eproj-tag/column tag)
       (propertize (concat ":" (number->string it))
                   'face 'eproj-symbnav-column-number)))))

(provide 'eproj-ctags)

;; Local Variables:
;; End:

;; eproj-ctags.el ends here
