;; eproj.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 30 December 2012
;; Description:
;;
;; Format of .eproj-info
;; ([(languages <langs>)] - it's not a good practice to omit this; if it's empty
;;                          then no attempt will be made to infer languages used
;;  [(project-type <proj-type>)]
;;  [(related <abs-or-rel-dir>*])
;;  [(aux-files
;;    [(tree <tree-root> <pattern>*)])]
;;  [(ignored-files <regexp>+)] - ignored filenames, <regexp>
;;                                should match absolute file names
;;  [(file-list <abs-or-rel-file>)] - filename listing all files in lisp format,
;;                                    e.g. ("foo" "c:/bar.txt" "../quux.log")
;;
;;  ;; these are mostly for haskell
;;  [(tag-file <abs-or-rel-file>)]
;;
;; [...] - optional directive
;; {...} - grouping directive
;; <abs-or-rel-dir> - absolute or relative path to directory
;; <abs-or-rel-file> - absolute or relative path to file
;; <langs> - list of major-mode symbols
;; <proj-type> - symbol naming type of project, will be verified
;;               agains inferred project type
;; <tree-root> - absolute path to existing directory
;; <pattern> - regular expression
;; <command> - emacs string representation of shell command to update tag
;;             file corresponding to tag-file entry
;; <arg> - emacs strings, arguments to the command

(eval-when-compile (require 'cl-lib))

(require 'custom)
(require 'common)
(require 'custom-predicates)
(require 'select-mode)
(require 'more-haskell)
(require 'haskell-autoload)

;;; eproj-tag

;; use this to debug type errors
;; (defstruct (eproj-tag
;;             (:conc-name eproj-tag/))
;;   symbol ;; == name - string
;;   file   ;; string
;;   line   ;; number
;;   properties)

;; (fmakunbound 'make-eproj-tag)

(defsubst make-eproj-tag (symbol file line props)
  (cons symbol (cons file (cons line props))))

(defsubst eproj-tag-p (tag-struct)
  (and (consp tag-struct)
       (stringp (car tag-struct))
       (consp (cdr tag-struct))
       (stringp (cadr tag-struct))
       (consp (cddr tag-struct))
       (integerp (caddr tag-struct))))

(defsubst eproj-tag/symbol (tag-struct)
  (car tag-struct))

(defsubst eproj-tag/file (tag-struct)
  (cadr tag-struct))

(defsubst eproj-tag/line (tag-struct)
  (caddr tag-struct))

(defsubst eproj-tag/properties (tag-struct)
  (cdddr tag-struct))

;;; eproj languages

(defstruct (eproj-language
            (:conc-name eproj-language/))
  mode
  extension-re
  load-procedure
  ;; function taking two arguments, eproj/project structure and
  ;; list of files to load from. Returns hashtable of
  ;; (<identifier> . <eproj-tags>) bindings for specified
  ;; files, <eproj-tags> is a list of tags
  tag->string-procedure ;; function of one argument returning string
  synonym-modes ;; list of symbols, these modes will resolve to this language
                ;; during tag search
  applies-to-files-procedure ;; Function taking list of absolute file names
                             ;; and returning t if at least one file is
                             ;; in this language. The function may be nil.
  normalize-identifier-before-navigation-procedure ;; Possibly strip unneeded
                                                   ;; information before
                                                   ;; performing navigation
  )

;;;; ctags facility

(defparameter *ctags-exec*
  (platform-dependent-executable (concat +execs-path+ "/ctags")))

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
      ;; tag name
      (group (+ (not (any ?\t ?\s ?\n))))
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

(defconst +ctags-aux-fields-re+
  (eval-when-compile
    (macroexpand
     `(rx (or ,@+ctags-aux-fields+)))))

(defun eproj/run-ctags-on-files (lang-mode root-dir files out-buffer)
  (if (not (null? *ctags-exec*))
    (with-current-buffer out-buffer
      (goto-char (point-max))
      (unless (looking-at-pure? "^$")
        (insert "\n"))
      (let ((ext-re (eproj-language/extension-re
                     (gethash lang-mode eproj/languages-table))
                    ;; (cadr (assq lang-mode *ctags-language-extensions*))
                    ))
        (with-temp-buffer
          (cd root-dir)
          (dolist (file files)
            (when (string-match-pure? ext-re file)
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
                     (buffer-substring-no-properties (point-min) (point-max))))))))
    (message "ctags executable not found")))

(defparameter eproj/ctags-string-cache
  (make-hash-table :test #'equal :size 997 :weakness t))

(defsubst eproj/ctags-cache-string (x)
  (assert (string? x))
  (if-let (cached-x (gethash x eproj/ctags-string-cache))
    cached-x
    (puthash x x eproj/ctags-string-cache)))

(defun eproj/ctags-get-tags-from-buffer (buffer proj &optional simple-format?)
  "Constructs hash-table of (tag . eproj-tag) bindings extracted from buffer BUFFER.
BUFFER is expected to contain output of ctags command.

If SIMPLE-FORMAT? is t then do not attempt to parse <key>=<value> pairs
after ;\", and expect single character there instead (this won't be checked at
runtime but rather will be silently relied on)."
  (with-current-buffer buffer
    (save-match-data
      (goto-char (point-min))
      (let ((tags-table (make-hash-table :test #'equal))
            (fields-re (concat "^\\(" +ctags-aux-fields-re+ "\\):\\(.*\\)$")))
        (while (not (eob?))
          (when (and (not (looking-at-pure? "^!_TAG_")) ;; skip metadata
                     (looking-at +ctags-line-re+))
            (let ((symbol (eproj/ctags-cache-string
                           (match-string-no-properties 1)))
                  (file (eproj/ctags-cache-string
                         (match-string-no-properties 2)))
                  (line (string->number (match-string-no-properties 3))))
              (goto-char (match-end 0))
              ;; now we're past ;"
              (let* ((fields-str (buffer-substring-no-properties
                                  (point)
                                  (line-end-position)))
                     (fields
                      (if simple-format?
                        (list (cons 'type
                                    (eproj/ctags-cache-string
                                     (trim-whitespace fields-str))))
                        (delq nil
                              (map (lambda (entry)
                                     (if (string-match? fields-re entry)
                                       (let ((identifier (match-string-no-properties 1 entry))
                                             (value (match-string-no-properties 2 entry)))
                                         ;; when value is nonempty
                                         (when (< 0 (length value))
                                           (cons (string->symbol identifier)
                                                 (eproj/ctags-cache-string value))))
                                       (error "invalid entry: %s" entry)))
                                   (split-string fields-str "\t" t)))))
                     (new-tag (make-eproj-tag
                               symbol
                               file
                               line
                               fields)
                              ;; (make-ctags-tag
                              ;;  :symbol     symbol
                              ;;  :file-idx   (ctags-file->id file)
                              ;;  :line       line
                              ;;  :kind       (cdr (assoc* 'kind fields))
                              ;;  :aux-fields (filter (lambda (x)
                              ;;                        (not (eq? 'kind (car x))))
                              ;;                      fields))
                              ))
                ;; (unless (ctags-file->id file)
                ;;   (let ((file-idx (+ (ctags/latest-defined-index *ctags-file-sequence*)
                ;;                      1)))
                ;;     (setf *ctags-file-sequence* (ctags/grow-vector *ctags-file-sequence*
                ;;                                                    file-idx
                ;;                                                    file))
                ;;     (puthash file file-idx *ctags-file-idxs*)))
                (puthash symbol
                         (cons new-tag
                               (gethash symbol tags-table nil))
                         tags-table))))
          (forward-line 1))
        tags-table))))

;;;; fast-tags facility

(defparameter *fast-tags-exec* (executable-find "fast-tags"))

;;;; language definitions

(defun eproj/generic-tag->string (proj tag)
  (assert (eproj-tag-p tag))
  (concat "Generic tag "
          (eproj-tag/symbol tag)
          "\n"
          (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
                                         (eproj-project/root proj))
          ":"
          (number->string (eproj-tag/line tag))
          "\n"
          (format "%s" (eproj-tag/properties tag))
          "\n")
  ;; (lambda (entry)
  ;;   (let ((delim (cadr (assq orig-major-mode
  ;;                            *ctags-symbols-name-delimiter-alist*))))
  ;;     (format "%s %s%s%s\n%s:%s\n%s:%s\n"
  ;;             (ctags-tag-kind entry)
  ;;             (aif (find-if (lambda (entry)
  ;;                             (memq (car entry)
  ;;                                   '(class
  ;;                                     struct
  ;;                                     union
  ;;                                     enum)))
  ;;                           (ctags-tag-aux-fields entry))
  ;;               (concat (cdr it) delim)
  ;;               "")
  ;;             (ctags-tag-symbol entry)
  ;;             (aif (assoc 'signature (ctags-tag-aux-fields entry))
  ;;               (cdr it)
  ;;               "")
  ;;             (file-name-nondirectory (ctags-tag-file entry))
  ;;             (ctags-tag-line entry)
  ;;             (ctags-tag-file entry)
  ;;             (ctags-tag-line entry))))
  )

(defun eproj/c-tag->string (proj tag)
  (assert (eproj-tag-p tag))
  (concat (eproj-tag/symbol tag)
          " "
          (awhen (assq 'kind (eproj-tag/properties tag))
            (concat
             "["
             (cdr it)
             "]"))
          "\n"
          (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
                                         (eproj-project/root proj))
          ":"
          (number->string (eproj-tag/line tag))
          "\n"
          (eproj/extract-tag-line proj tag)
          "\n"))

(defun eproj/haskell-tag->string (proj tag)
  (assert (eproj-tag-p tag))
  (concat (eproj-tag/symbol tag)
          " ["
          (pcase (cdr-safe (assoc 'type (eproj-tag/properties tag)))
            ("m" "Module")
            ("f" "Function")
            ("c" "Class")
            ("t" "Type")
            ("C" "Constructor")
            ("o" "Operator")
            ("p" "Pattern")
            (_
             (error "Invalid haskell tag property %s"
                    (eproj-tag/properties tag))))
          "]\n"
          (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
                                         (eproj-project/root proj))
          ":"
          (number->string (eproj-tag/line tag))
          "\n"
          (eproj/haskell-extract-tag-signature proj tag)
          "\n"))

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

(defun eproj/load-ctags-project (lang-mode proj files)
  (let ((root (eproj-project/root proj)))
    (with-temp-buffer
      (eproj/run-ctags-on-files lang-mode
                                root
                                files
                                (current-buffer))
      (prog1 (eproj/ctags-get-tags-from-buffer (current-buffer) proj)
        (erase-buffer)))))

(defun eproj/load-haskell-project (proj files)
  "Load haskell project PROJ according to definitions in .eproj-info file.

Note: old tags file is removed before calling update command."
  (assert (eproj-project-p proj))
  (if-let (tag-file (cadr-safe
                     (assoc 'tag-file (eproj-project/aux-info proj))))
    (begin
      (assert (string? tag-file))
      (let ((tag-file-path (eproj-resolve-abs-or-rel-name tag-file
                                                          (eproj-project/root proj))))
        (when (or (null? tag-file-path)
                  (not (file-exists? tag-file-path)))
          (error "Cannot find tag file %s at %s" tag-file tag-file-path))
        (for-buffer-with-file tag-file-path
          (eproj/ctags-get-tags-from-buffer (current-buffer) proj t))))
    (begin
      (when (null? *fast-tags-exec*)
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
                (dolist (file files)
                  (when (string-match-pure? ext-re file)
                    (insert file "\0")))
                (when (not (= 0
                              (call-process-region (point-min)
                                                   (point-max)
                                                   *fast-tags-exec*
                                                   nil
                                                   out-buffer
                                                   nil
                                                   "-0"
                                                   "-o-"
                                                   "--ignore-encoding-errors"
                                                   "--nomerge")))
                  (error "fast-tags invokation failed: %s"
                         (with-current-buffer out-buffer
                           (buffer-substring-no-properties (point-min) (point-max)))))))
              (erase-buffer)
              (eproj/ctags-get-tags-from-buffer out-buffer proj t))))))
      ;; (message "Warning: no tag file for haskell project %s"
      ;;          (eproj-project/root proj))
      )))

(defun eproj/clojure-load-procedure (proj files)
  (assert (eproj-project-p proj))
  (when (memq 'java-mode (eproj-project/languages proj))
    (eproj/load-ctags-project 'java-mode proj files))
  )


(defparameter eproj/languages
  (list (letrec ((lang (make-eproj-language
                        :mode 'haskell-mode
                        :extension-re (concat "\\."
                                              (regexp-opt *haskell-extensions*)
                                              "$")
                        :load-procedure
                        (lambda (proj files)
                          (eproj/load-haskell-project proj files))
                        :tag->string-procedure #'eproj/haskell-tag->string
                        :applies-to-files-procedure
                        (lambda (files)
                          (any? (comp
                                 (partial #'string-match-pure?
                                          (eproj-language/extension-re lang)))
                                files))
                        :synonym-modes '(literate-haskell-mode
                                         haskell-c-mode
                                         c2hs-mode)
                        :normalize-identifier-before-navigation-procedure
                        #'haskell-remove-module-qualification)))
          lang)
        (letrec ((lang (make-eproj-language
                        :mode 'c-mode
                        :extension-re (rx "."
                                          (or "c" "h")
                                          eol)
                        :load-procedure
                        (lambda (proj files)
                          (eproj/load-ctags-project 'c-mode proj files))
                        :tag->string-procedure #'eproj/c-tag->string
                        :applies-to-files-procedure
                        (lambda (files)
                          (any? (comp
                                 (partial #'string-match-pure?
                                          (eproj-language/extension-re lang)))

                                files))
                        :synonym-modes nil
                        :normalize-identifier-before-navigation-procedure
                        #'identity)))
          lang)
        (letrec ((lang (make-eproj-language
                        :mode 'c++-mode
                        :extension-re (rx "."
                                          (or "c"
                                              "cc"
                                              "cxx"
                                              "cpp"
                                              "c++"
                                              "h"
                                              "hh"
                                              "hxx"
                                              "hpp"
                                              "h++"
                                              "inl"
                                              "inc"
                                              "incl")
                                          eol)
                        :load-procedure
                        (lambda (proj files)
                          (eproj/load-ctags-project 'c++-mode proj files))
                        :tag->string-procedure #'eproj/c-tag->string
                        :applies-to-files-procedure
                        (lambda (files)
                          (let ((c-ext (eproj-language/extension-re
                                        (gethash 'c-mode eproj/languages-table))))
                            (any? (lambda (path)
                                    (and (string-match-pure?
                                          (eproj-language/extension-re lang)
                                          path)
                                         (not (string-match-pure? c-ext path))))
                                  files)))
                        :synonym-modes nil
                        :normalize-identifier-before-navigation-procedure
                        #'identity)))
          lang)
        (letrec ((lang (make-eproj-language
                        :mode 'python-mode
                        :extension-re (rx "."
                                          (or "py" "pyx" "pxd" "pxi")
                                          eol)
                        :load-procedure
                        (lambda (proj files)
                          (eproj/load-ctags-project 'python-mode proj files))
                        :tag->string-procedure #'eproj/generic-tag->string
                        :applies-to-files-procedure
                        (lambda (files)
                          (any? (comp
                                 (partial #'string-match-pure?
                                          (eproj-language/extension-re lang)))

                                files))
                        :synonym-modes nil
                        :normalize-identifier-before-navigation-procedure
                        #'identity)))
          lang)
        (letrec ((lang (make-eproj-language
                        :mode 'clojure-mode
                        :extension-re (rx "."
                                          (or "clj"
                                              "java")
                                          eol)
                        :load-procedure #'eproj/clojure-load-procedure
                        :tag->string-procedure #'eproj/generic-tag->string
                        :applies-to-files-procedure
                        (lambda (files)
                          (let ((java-ext (eproj-language/extension-re
                                           (gethash 'java-mode eproj/languages-table))))
                            (any? (lambda (path)
                                    (and (string-match-pure?
                                          (eproj-language/extension-re lang)
                                          path)
                                         (not (string-match-pure?
                                               java-ext
                                               path))))
                                  files)))
                        :synonym-modes nil
                        :normalize-identifier-before-navigation-procedure
                        #'identity)))
          lang)
        (letrec ((lang (make-eproj-language
                        :mode 'java-mode
                        :extension-re (rx "."
                                          (or "java")
                                          eol)
                        :load-procedure
                        (lambda (proj files)
                          (eproj/load-ctags-project 'java-mode proj files))
                        :tag->string-procedure #'eproj/generic-tag->string
                        :applies-to-files-procedure
                        (lambda (files)
                          (any? (comp
                                 (partial #'string-match-pure?
                                          (eproj-language/extension-re lang)))
                                files))
                        :synonym-modes nil
                        :normalize-identifier-before-navigation-procedure
                        #'identity)))
          lang)))

(defparameter eproj/languages-table
  (let ((table (make-hash-table :test #'eq)))
    (dolist (lang eproj/languages)
      (puthash (eproj-language/mode lang) lang table))
    table))

(defparameter eproj/synonym-modes-table
  (let ((table (make-hash-table :test #'eq)))
    (dolist (lang eproj/languages)
      (dolist (synonym (eproj-language/synonym-modes lang))
        (puthash synonym (eproj-language/mode lang) table)))
    table))

;;; eproj-project

;;;; projects themselves

(defstruct (eproj-project
            (:conc-name eproj-project/))
  type ;; references eproj-project-type structure
  root
  aux-info ;; alist of (<symbol> . <symbol-dependent-info>) entries)
  tags ;; list of (language-major-mode . <tags-table>);
  ;; <tags-table> - hashtable of (symbol-str . eproj-tag) bindings
  related-projects ;; list of other project roots
  aux-files-source ;; list of other files or function that yields such list
  languages        ;; list of symbols - major-modes for related languages
  )

(defun eproj-project/aux-files (proj)
  (aif (eproj-project/aux-files-source proj)
    (cond ((functionp it)
           (funcall it))
          ((list? it)
           it)
          (t
           nil))
    nil))

(defun eproj-project/root= (proj-a proj-b)
  (string= (eproj-project/root proj-a)
           (eproj-project/root proj-b)))

(defparameter *eproj-projects*
  (make-hash-table :test #'equal)
  "Hash table mapping project roots to projects.")

(defun eproj-reset-projects ()
  "Clear project database `*eproj-projects*'."
  (interactive)
  (setf *eproj-projects* (make-hash-table :test #'equal))
  ;; do not forget to reset cache
  (eproj/reset-buffer-local-cache)
  (garbage-collect))

(defun eproj-update-projects ()
  "Update projects in database `*eproj-projects*'."
  (interactive)
  (maphash (lambda (root proj)
             (eproj-reload-project! proj))
           *eproj-projects*))

(defun eproj-update-buffer-project ()
  "Update project for current buffer or create new project if it does not exists."
  (interactive)
  (eproj-reload-project! (eproj-get-project-for-buf (current-buffer)))
  (message "done"))

;; careful: quite complex procedure
(defun eproj-update-buffer-tags ()
  "Update tags only for current buffer in project that contains it."
  (interactive)
  (let* ((proj (eproj-get-project-for-buf (current-buffer)))
         (buf (current-buffer))
         (fname (expand-file-name (buffer-file-name buf)))
         (non-fname-tag-func
          (lambda (tag)
            (not (string= fname
                          (expand-file-name
                           (eproj-tag/file tag))))))
         (mode (eproj-symbnav/resolve-synonym-modes
                (with-current-buffer buf
                  major-mode))))
    (unless (memq mode
                  (eproj-project/languages proj))
      (error "Project %s does not manage %s files"
             (eproj-project/root proj)
             mode))
    (if-let (old-tags (cdr-safe (assoc mode (eproj-project/tags proj))))
      (eproj-with-language-load-proc mode load-proc
        (let ((new-tags (funcall load-proc proj (list fname))))
          ;; filter all tags values to remove any tags
          ;; related to current buffer
          (maphash (lambda (symbol-str tags)
                     (puthash symbol-str
                              (filter non-fname-tag-func
                                      tags)
                              old-tags))
                   old-tags)
          (hash-table-merge-with!
           (lambda (symbol-str tags-old tags-new)
             (append tags-old
                     tags-new))
           old-tags
           new-tags)))
      (error "Project %s does not have tags for %s"
             (eproj-project/root proj)
             mode))))

(defun eproj-get-aux-info-for-buffer-project (key)
  "Query aux info for current buffer's project for KEY."
  (rest-safe (assoc key
                    (eproj-project/aux-info
                     (eproj-get-project-for-buf (current-buffer))))))

(defmacro eproj-with-language-load-proc (lang-mode-var
                                         load-proc-var
                                         &rest
                                         body)
  "Execute BODY with LOAD-PROC-VAR bound to load procedure for mode in LANG-MODE-VAR."
  (declare (indent 2))
  (let ((lang-var (gensym "lang")))
    `(progn
       (assert (symbol? ,lang-mode-var)
               nil
               "invalid language mode = %s" ,lang-mode-var)
       (if-let (,lang-var (gethash ,lang-mode-var eproj/languages-table))
         (if-let (,load-proc-var (eproj-language/load-procedure ,lang-var))
           (begin
             ,@body)
           (error "No load procedure defined for language %s"
                  ,lang-mode-var))
         (error "No eproj/language defined for language %s"
                ,lang-mode-var)))))

(defun eproj-reload-tags (proj)
  "Reload tags for PROJ."
  (let ((files (eproj-get-project-files proj)))
    (setf (eproj-project/tags proj)
          (map (lambda (lang-mode)
                 (eproj-with-language-load-proc lang-mode load-proc
                   (let ((new-tags (funcall load-proc proj files)))
                     (assert (and (not (null? new-tags))
                                  (hash-table-p new-tags)))
                     (when (= 0 (hash-table-count new-tags))
                       (error "Warning while reloading: project %s loaded no tags for language %s"
                              (eproj-project/root proj)
                              lang-mode))
                     (cons lang-mode new-tags))))
               (eproj-project/languages proj))))
  nil)

(defun eproj-populate-from-eproj-info! (proj aux-info)
  (let ((languages (let ((languages-entry (assoc 'languages aux-info)))
                     (if (null? languages-entry)
                       (begin
                         (message "warning: no languages defined for project %s"
                                  (eproj-project/root proj))
                         ;; language inference is somewhat dubious feature
                         ;; because it may infer something nonsensical
                         ;; (e.g. treat some templates as legitimate project language)
                         (eproj/infer-project-languages proj))
                       (rest languages-entry))))
        (specified-project-type (cadr-safe (assoc 'project-type aux-info)))
        (inferred-project-type (eproj-project-type/name
                                (eproj-project/type proj))))
    (when (and (not (null? specified-project-type))
               (not (symbol? specified-project-type)))
      (error "Incorrect specified project type, symbol expected: %s"
             specified-project-type))
    (setf (eproj-project/aux-info proj) aux-info
          (eproj-project/related-projects proj)
          (eproj-get-related-projects (eproj-project/root proj) aux-info)
          (eproj-project/aux-files-source proj)
          (eproj-make-aux-files-constructor (eproj-project/root proj) aux-info)
          (eproj-project/languages proj)
          languages)
    (eproj-reload-tags proj)
    nil))

(defun eproj-get-eproj-info-from-dir (dir)
  "Get filename of .eproj-info file from directory DIR if it exists, else return nil."
  (let ((eproj-info-file (concat (eproj-normalize-file-name dir)
                                 "/.eproj-info")))
    (when (file-exists? eproj-info-file)
      eproj-info-file)))

(defun eproj-read-eproj-info-file (filename)
  "Read .eproj-info file from FILENAME."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (goto-char (point-min))
    (read (current-buffer))))

(defun eproj-reload-project! (proj)
  "Update project PROJ - re-read its .eproj-info file and update project
variables accordingly."
  (assert (not (null? (eproj-project/root proj))))
  (assert (string? (eproj-project/root proj)))
  (eproj-populate-from-eproj-info!
   proj
   (eproj-read-eproj-info-file
    (eproj-get-eproj-info-from-dir (eproj-project/root proj)))))

(defun eproj/infer-project-languages (proj)
  "Try to infer languages used in project PROJ by its files."
  (let ((files (eproj-get-project-files proj)))
    (map #'eproj-language/mode
         (filter (lambda (lang)
                   (awhen (eproj-language/applies-to-files-procedure lang)
                     (funcall it files)))
                 eproj/languages))))

;;;; project types and project creation

(defstruct (eproj-project-type
            (:conc-name eproj-project-type/))
  name ;; one of symbols: git, eproj-file
  ;; These functions are sorted by rough order in which results of the earlier
  ;; functions will affect how later ones will be called, i.e. results of the
  ;; former will be passed to the latter.
  get-initial-project-root-proc ;; function with signature: (path)
  make-project-proc ;; function-with-signature: (root)
  get-project-files-proc ;; function with signature: (proj)
  )

(defparameter eproj-project-types
  (list
   (letrec ((proj-type-entry
             (make-eproj-project-type
              :name 'git
              :get-initial-project-root-proc
              (lambda (path)
                (when *have-git?*
                  (awhen (if (file-directory? path)
                           (git-get-repository-root path)
                           (for-buffer-with-file path
                             (git-update-file-repository)
                             git-repository))
                    (let ((dir (eproj-normalize-file-name it)))
                      ;; strip trailing .git, if any
                      (save-match-data
                        (if (string-match? "^\\(.*\\)/\\.git$" dir)
                          (replace-match "\\1" nil nil dir)
                          dir))))))
              :make-project-proc
              (lambda (proj-root)
                (make-eproj-project :type proj-type-entry
                                    :root proj-root
                                    :tags nil
                                    :aux-info nil
                                    :related-projects nil
                                    :aux-files-source nil
                                    :languages nil))
              :get-project-files-proc
              (lambda (proj)
                (append (hash-table-keys (git-get-tracked-files
                                          (eproj-project/root proj)))
                        (eproj-project/aux-files proj))))))
     proj-type-entry)
   (letrec ((proj-type-entry
             (make-eproj-project-type
              :name 'eproj-file
              :get-initial-project-root-proc
              #'eproj/find-eproj-file-location
              :make-project-proc
              (lambda (proj-root)
                (make-eproj-project :type proj-type-entry
                                    :root proj-root
                                    :tags nil
                                    :aux-info nil
                                    :related-projects nil
                                    :aux-files-source nil
                                    :languages nil))
              :get-project-files-proc
              (lambda (proj)
                (find-rec (eproj-project/root proj)
                          :filep
                          (lambda (path)
                            (any? (lambda (lang)
                                    (assert (symbol? lang))
                                    (string-match-pure?
                                     (eproj-language/extension-re
                                      (gethash lang eproj/languages-table))
                                     path))
                                  (eproj-project/languages proj))))))))
     proj-type-entry))
  "List of `eproj-project-type' structures, defines order in
which to try loading/root finding/etc.")

(defun eproj-make-project (root type aux-info)
  (assert (string? root)
          nil
          "Project root must be a string: %s" root)
  (assert (eproj-project-type-p type))
  (unless (and (file-exists? root)
               (file-directory? root))
    (error "Invalid project root, existing directory required: %s" root))
  (let ((proj (funcall (eproj-project-type/make-project-proc type)
                       (eproj-normalize-file-name root))))
    (when (null? proj)
      (error "Error while trying to obtain project for root %s" root))
    (eproj-populate-from-eproj-info! proj aux-info)
    proj))

;;;; description

(defun eproj-describe-all-projects ()
  (interactive)
  (let ((buf (get-buffer-create "*eproj projects*")))
    (switch-to-buffer-other-window buf)
    (with-current-buffer buf
      (erase-buffer)
      (text-mode)
      (let ((projs (sort (hash-table->alist *eproj-projects*)
                         (lambda (a b)
                           (string< (car a)
                                    (car b))))))
        (map (lambda (entry)
               (destructuring-bind (root . proj) entry
                 (eproj-descibe-proj buf proj nil t)
                 (insert (make-string 80 ?\-) "\n")))
             projs))
      (goto-char (point-min)))))

(defun eproj-describe-buffer-project ()
  (interactive)
  (if-let (proj (eproj-get-project-for-buf (current-buffer)))
    (let ((buf (get-buffer-create (format "*%s description*" (eproj-project/root proj)))))
      (switch-to-buffer-other-window buf)
      (with-current-buffer buf
        (erase-buffer)
        (text-mode)
        (eproj-descibe-proj buf proj t nil)
        (goto-char (point-min))))
    (error "no project for buffer %s" (buffer-name (current-buffer)))))

(defun eproj-descibe-proj (buf proj &optional describe-tags describe-buffers)
  "Insert description of PROJ in current buffer BUF."
  (let ((indent "    "))
    (with-current-buffer buf
      (insert "type: " (pp-to-string (eproj-project-type/name
                                      (eproj-project/type proj)))
              "\n")
      (insert "root: " (eproj-project/root proj) "\n")
      (insert (format "languages: %s\n" (eproj-project/languages proj)))
      (insert "related projects:\n")
      (dolist (related-proj (eproj-project/related-projects proj))
        (insert indent related-proj "\n"))
      (when describe-buffers
        (insert "buffers:\n")
        (dolist (buf (filter (lambda (buf)
                               (condition-case nil
                                   (string=
                                    (eproj-project/root proj)
                                    (eproj-project/root
                                     (eproj-get-project-for-buf buf)))
                                 (error nil)))
                             (visible-buffers)))
          (insert indent (buffer-name buf) "\n")))
      (when describe-tags
        (insert "tags:\n")
        (dolist (tags-entry (eproj-project/tags proj))
          (let ((lang-tags (sort (hash-table->alist (cdr tags-entry))
                                 (lambda (a b) (string< (car a) (car b))))))
            (insert indent "lang: "
                    (pp-to-string (car tags-entry))
                    ", total amount = "
                    (number->string (length lang-tags))
                    "\n")
            (dolist (entry lang-tags)
              (insert indent indent (pp-to-string (car entry)) "\n")
              (dolist (subentry (cdr entry))
                (insert indent indent indent
                        (format "%s:%s\n"
                                (file-relative-name
                                 (expand-file-name
                                  (eproj-resolve-abs-or-rel-name
                                   (eproj-tag/file subentry)
                                   (eproj-project/root proj)))
                                 (expand-file-name (eproj-project/root proj)))
                                (eproj-tag/line subentry)))))))))))

;;;; utilities

(defun eproj-get-project (root type aux-info)
  (aif (gethash root *eproj-projects* nil)
    it
    (let ((proj (eproj-make-project root type aux-info)))
      (puthash (eproj-project/root proj)
               proj
               *eproj-projects*)
      proj)))

(defun-caching eproj-get-initial-project-root-type-and-aux-info (path) (path)
  "Get (<initial-project-root> <project-type> <aux-info>) triple for project
containing PATH as its part."
  (if-let (initial-root (eproj/find-eproj-file-location path))
    (if-let (proj (gethash initial-root *eproj-projects* nil))
      (values initial-root (eproj-project/type proj) (eproj-project/aux-info proj))
      (if-let (eproj-info-file (eproj-get-eproj-info-from-dir initial-root))
        (let* ((aux-info (eproj-read-eproj-info-file eproj-info-file))
               (proj-type (or (cadr-safe (assoc 'project-type aux-info))
                              'eproj-file))
               (actual-type (find-if (lambda (type)
                                       (eq? (eproj-project-type/name type) proj-type))
                                     eproj-project-types)))
          (unless actual-type
            (error "invalid project type in .eproj-file at %s: %s" path proj-type))
          (values initial-root
                  actual-type
                  aux-info))
        (error ".eproj-info file does not exist at %s"
               initial-root)))
    (error "Error while obtaining project for path %s: no potential project roots can be constructed"
           path)))

(defun eproj-get-initial-project-root (path)
  "Retrieve root for project that would contain PATH."
  (multiple-value-bind (initial-root proj-type aux_info)
      (eproj-get-initial-project-root-type-and-aux-info path)
    initial-root))

(defmacro eproj/evaluate-with-caching-buffer-local-var (value-expr
                                                        buffer-expr
                                                        caching-var
                                                        value-predicate)

  (let* ((buffer-var (gensym "buffer"))
         (is-nil (gensym "is-nil"))
         (is-nil-value `(quote ,is-nil)))
    `(let ((,buffer-var ,buffer-expr))
       (with-current-buffer ,buffer-var
         (when (null? ,caching-var)
           (setf ,caching-var (or ,value-expr ,is-nil-value)))
         (if (eq? ,caching-var ,is-nil-value)
           nil
           (progn
             (assert (funcall ,value-predicate ,caching-var)
                     nil
                     ,(format "Variable `%s' must contain value that satisfies predicate %s"
                              caching-var
                              value-predicate))
             ,caching-var))))))

(defun eproj/reset-buffer-local-cache ()
  "Reset all caching buffer-local values associated with eproj in all buffers"
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (kill-local-variable 'eproj/buffer-initial-project-root-cache)
      (kill-local-variable 'eproj/buffer-project-cache))))

(defvar-local eproj/buffer-initial-project-root-cache nil
  "Is set to initial project root (i.e. string) for buffer containing this
variable or symbol 'unresolved.")

(defun eproj-get-initial-project-root-for-buf (buffer)
  "Retrieve root for project that would contain BUFFER's content."
  (eproj/evaluate-with-caching-buffer-local-var
   (condition-case nil
        (eproj-get-initial-project-root (eproj--get-buffer-directory buffer))
      (error nil))
   buffer
   eproj/buffer-initial-project-root-cache
   #'string?))

(defvar-local eproj/buffer-project-cache nil
  "Caches value computed by `eproj-get-project-for-buf'.

Set to project that corresponds to buffer containing this variable or
symbol 'unresolved.")

(defun eproj-get-project-for-buf (buffer)
  (eproj/evaluate-with-caching-buffer-local-var
   (eproj-get-project-for-path
    ;; Take directory since file visited by buffer may not be
    ;; under version control per se.
    (eproj--get-buffer-directory buffer))
   buffer
   eproj/buffer-project-cache
   #'eproj-project-p))

(defun eproj-get-project-for-path (path)
  "Retrieve project that contains PATH as its part."
  (multiple-value-bind (initial-root proj-type aux-info)
      (eproj-get-initial-project-root-type-and-aux-info path)
    (eproj-get-project initial-root
                       proj-type
                       aux-info)))

(defun eproj-get-project-files (proj)
  "Retrieve project files for PROJ depending on it's type."
  ;; Cached files are necessarily from file-list and intended for projects whose
  ;; list of files does not change and may be cached.
  (if-let (cached-files (cadr-safe
                         (assoc 'eproj-get-project-files/cached-files
                                (eproj-project/aux-info proj))))
    cached-files
    (let* ((ignored-files-regexps
            (rest-safe (assoc 'ignored-files (eproj-project/aux-info proj))))
           (filter-ignored-files
            (lambda (files)
              (aif ignored-files-regexps
                (let ((regexp
                       (mapconcat (lambda (x) (concat "\\(?:" x "\\)"))
                                  it
                                  "\\|")))
                  (filter (lambda (fname)
                            (not (string-match-pure? regexp fname)))
                          files))
                files))))
      ;; if there's file-list then read it and store to cache
      (if-let (file-list (cadr-safe
                          (assoc 'file-list (eproj-project/aux-info proj))))
        (let ((file-list-filename (eproj-resolve-abs-or-rel-name
                                   file-list
                                   (eproj-project/root proj))))
          (when (or (null? file-list-filename)
                    (not (file-exists? file-list-filename)))
            (error "Cannot find file list: filename %s at %s" file-list file-list-filename))
          (let ((list-of-files
                 (with-temp-buffer
                   (insert-file-contents-literally file-list-filename)
                   (goto-char (point-min))
                   (read (current-buffer)))))
            (assert (list? list-of-files))
            (let ((resolved-files
                   (funcall filter-ignored-files
                            (map (lambda (filename)
                                   (eproj-resolve-abs-or-rel-name
                                    filename
                                    (eproj-project/root proj)))
                                 list-of-files))))
              (assert (all? (lambda (filename)
                              (and (string? filename)
                                   (file-exists? filename)))
                            resolved-files))
              ;; add to cache
              (push (list 'eproj-get-project-files/cached-files resolved-files)
                    (eproj-project/aux-info proj))
              resolved-files)))
        (let ((files
               (funcall (eproj-project-type/get-project-files-proc (eproj-project/type proj))
                        proj)))
          (funcall filter-ignored-files files))))))

(defun eproj-get-related-projects (root aux-info)
  "Return list of roots of related project for folder ROOT and AUX-INFO.
AUX-INFO is expected to be a list with entry (related { <abs-path> | <rel-path> }* ).
Returns nil if no relevant entry found in AUX-INFO."
  (let ((project-root root))
    (when-let (related-entry (rest-safe
                              (assoc 'related
                                     aux-info)))
      (map (lambda (path)
             (assert (string? path) nil
                     "invalid entry under related clause, string expected %s"
                     path)
             (cond ((file-directory? path)
                    path)
                   ((file-directory? (expand-file-name path project-root))
                    (expand-file-name path project-root))
                   (t
                    (error "invalid related-project entry: non-existing absolute nor relative directory: %s"
                           path))))
           related-entry))))

(defun eproj-make-aux-files-constructor (root aux-info)
  "Make up function that will return list of absolute names for auxiliary files
of project upon invokation. Aux files usually are files in repository that
are not listed by `eproj-get-project-files' \(e.g. files not tracked by version
control system, etc).

ROOT should be directory with project to make auxiliary files for. AUX-INFO is
datastructure found in ROOT/.eproj-info file, if such file exists.

AUX-INFO is expected to be a list of zero or more constructs:
1. \(tree <tree-root> <pattern>*)
<tree-root> should be a directory to recursively search files in
<pattern> should be regular expression string."
  (let ((project-root root))
    (when-let (aux-files-entry (rest-safe
                                (assoc 'aux-files
                                       aux-info)))
      (lambda ()
        (with-temp-buffer
          (cd project-root)
          (mapcan (lambda (item)
                    (assert (list? item) nil
                            "invalid entry under aux-files clause, list expected: %s"
                            item)
                    (cond ((eq? (car-safe item) 'tree)
                           (let ((tree-root (cadr-safe item))
                                 (patterns (cddr-safe item)))
                             (assert (and (not (null? tree-root))
                                          (file-exists? tree-root)
                                          (file-directory? tree-root))
                                     nil
                                     "Invalid tree root under aux-files/tree clause: %s"
                                     tree-root)
                             (assert (and (list? patterns)
                                          (not (null? patterns)))
                                     nil
                                     "Invalid patterns under aux-files/tree clause: %s"
                                     patterns)
                             (find-rec tree-root
                                       :filep
                                       (lambda (path)
                                         (any? (lambda (regexp)
                                                 (string-match-pure? regexp path))
                                               patterns)))))
                          (t
                           nil)))
                  aux-files-entry))))))

(defun eproj/find-eproj-file-location (path)
  "Find closest directory parent of PATH that contains .eproj-info file."
  (assert (string? path))
  (let ((dir (if (file-directory? path)
               path
               (file-name-directory path))))
    (awhen (locate-dominating-file dir ".eproj-info")
      (eproj-normalize-file-name it))))

(defun eproj-get-all-related-projects (proj)
  "Return eproj-project structures of projects realted to PROJ except PROJ itself."
  (letrec ((collect
            (lambda (projs visited items)
              (if projs
                (if (member* (car projs) visited
                             :test #'eproj-project/root=)
                  (funcall collect (cdr projs) visited items)
                  (funcall collect
                           (append (map #'eproj-get-project-for-path
                                        (eproj-project/related-projects (car projs)))
                                   (cdr projs))
                           (cons (car projs) visited)
                           (cons (car projs) items)))
                items))))
    (assert (eproj-project-p proj) nil
            "Not a eproj-project structure: %s" proj)
    (funcall collect
             (map #'eproj-get-project-for-path
                  (eproj-project/related-projects proj))
             (list proj)
             nil)))

(defun-caching eproj-resolve-abs-or-rel-name (path dir) (path dir)
  (if (or (file-exists? path)
          (file-directory? path))
    path
    (if (file-name-absolute-p path)
      (error "Non-existing absolute file name: %s, probably something went wrong" path)
      (let ((abs-path (concat (eproj-normalize-file-name dir) "/" path)))
        (if (or (file-exists? abs-path)
                (file-directory? abs-path))
          abs-path
          (error "File %s does not exist, try `eproj-update-buffer-project'"
                 abs-path))))))

(defun-caching eproj-normalize-file-name (path) (path)
  (strip-trailing-slash (normalize-file-name (expand-file-name path))))

(defun eproj--get-buffer-directory (buffer)
  "Get directory associated with BUFFER, either throug visited file
or `default-directory', if no file is visited."
  (with-current-buffer buffer
    (or (when-let (fname buffer-file-truename)
          (file-name-directory fname))
        default-directory)))

(defun eproj/extract-tag-line (proj tag)
  "Fetch line where TAG is defined."
  (assert (eproj-tag-p tag) nil "Eproj tag is required.")
  (for-buffer-with-file
      (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
                                     (eproj-project/root proj))
    (save-excursion
      (goto-line1 (eproj-tag/line tag))
      (current-line))))

;;; tag/symbol navigation (navigation over homes)

(defparameter eproj-symbnav/homes-history (list nil nil)
  "Two stacks of locations (previous next) from which
`eproj-symbnav/go-to-symbol-home' was invoked.")

(defparameter eproj-symbnav/previous-homes nil
  "Previous locations from which symbol search was invoked.")

(defparameter eproj-symbnav/selected-loc nil
  "Home entry corresponding to the most recently visited tag.")

(defparameter eproj-symbnav/next-homes nil
  "Next locations that were visited but now obscured by going back.")

(defvar-local eproj-symbnav/identifier-type 'symbol
  "Type of identifiers to look for when retrieving name at point to
search for in tags. This should be a symbol
as accepted by `bounds-of-thing-at-point'.")

(defun eproj-symbnav/identifier-at-point (&optional noerror)
  (or (awhen (get-region-string-no-properties)
        (trim-whitespace it))
      (let ((bounds (bounds-of-thing-at-point eproj-symbnav/identifier-type)))
        (cond ((not (null? bounds))
               (funcall (eproj-language/normalize-identifier-before-navigation-procedure
                         (gethash (eproj-symbnav/resolve-synonym-modes major-mode)
                                  eproj/languages-table))
                        (buffer-substring-no-properties (car bounds)
                                                        (cdr bounds))))
              ((null? noerror)
               (error "No identifier at point found"))
              (t
               nil)))))

(defun eproj-symbnav/show-home (entry)
  (when (not (null? entry))
    (with-current-buffer (eproj-home-entry/buffer entry)
      (concat (eproj-home-entry/symbol entry)
              "@"
              (file-name-nondirectory buffer-file-name)
              ":"
              (save-excursion
                (number->string
                 (line-number-at-pos (eproj-home-entry/point entry))))))))

(defun eproj-symbnav/describe ()
  (interactive)
  (message "Previous homes: %s\nSelected loc: %s\nNext homes: %s\n"
           (map #'eproj-symbnav/show-home eproj-symbnav/previous-homes)
           (eproj-symbnav/show-home eproj-symbnav/selected-loc)
           (map #'eproj-symbnav/show-home eproj-symbnav/next-homes)))

(defun eproj-symbnav/reset ()
  (interactive)
  (setf eproj-symbnav/previous-homes nil
        eproj-symbnav/selected-loc nil
        eproj-symbnav/next-homes nil))

(defstruct (eproj-home-entry
            (:conc-name eproj-home-entry/))
  buffer
  point
  symbol ;; == name - string, or nil if this entry was not selected explicitly
  )

(defun eproj-home-entry=? (entry-a entry-b)
  (and (eq? (eproj-home-entry/buffer entry-a)
            (eproj-home-entry/buffer entry-b))
       (= (eproj-home-entry/point entry-a)
          (eproj-home-entry/point entry-b))
       (eq? (eproj-home-entry/symbol entry-a)
            (eproj-home-entry/symbol entry-b))))


(defun eproj-symbnav/switch-to-home-entry (home-entry)
  (unless (buffer-live-p (eproj-home-entry/buffer home-entry))
    (setf (eproj-home-entry/buffer home-entry)
          (find-file-noselect
           (buffer-file-name (eproj-home-entry/buffer home-entry)))))
  (switch-to-buffer (eproj-home-entry/buffer home-entry))
  (goto-char (eproj-home-entry/point home-entry)))

(defun eproj-symbnav/resolve-synonym-modes (mode)
  "Replace modes that are similar to some other known modes"
  (aif (gethash mode eproj/synonym-modes-table)
    it
    mode))

(defun eproj-symbnav/go-to-symbol-home (&optional use-regexp)
  (interactive "P")
  (let* ((proj (eproj-get-project-for-buf (current-buffer)))
         (case-fold-search (and (not (null? current-prefix-arg))
                                (<= 16 (car current-prefix-arg))))
         (identifier (if use-regexp
                       (read-regexp "enter regexp to search for")
                       (eproj-symbnav/identifier-at-point nil)))
         (orig-major-mode (eproj-symbnav/resolve-synonym-modes major-mode))
         (orig-file-name (expand-file-name buffer-file-name))
         (current-home-entry (make-eproj-home-entry :buffer (current-buffer)
                                                    :point (point)
                                                    :symbol nil))
         (jump-to-home
          (lambda (entry entry-proj)
            (let ((file
                   (eproj-resolve-abs-or-rel-name (eproj-tag/file entry)
                                                  (eproj-project/root entry-proj))))
              (push current-home-entry eproj-symbnav/previous-homes)
              (setf eproj-symbnav/next-homes nil)
              (unless (file-exists? file)
                (error "file %s does not exist" file))
              (find-file file)
              (goto-line (eproj-tag/line entry))
              (save-match-data
                (when (re-search-forward (regexp-quote (eproj-tag/symbol entry))
                                         (line-end-position)
                                         t)
                  (goto-char (match-beginning 0))))
              ;; remove annoying "Mark set" message
              (message "")
              (setf eproj-symbnav/selected-loc
                    (make-eproj-home-entry :buffer (current-buffer)
                                           :point (point)
                                           :symbol (eproj-tag/symbol entry))))))
         (next-home-entry (car-safe eproj-symbnav/next-homes)))
    ;; load tags if there're none
    (unless (or (eproj-project/tags proj)
                (assq orig-major-mode (eproj-project/tags proj)))
      (eproj-reload-project! proj)
      (unless (eproj-project/tags proj)
        (error "Project %s loaded no names\nProject: %s"
               (eproj-project/root proj)
               proj))
      (unless (assq orig-major-mode (eproj-project/tags proj))
        (error "No names in project %s for language %s"
               (eproj-project/root proj)
               orig-major-mode)))
    (if (and next-home-entry
             (when-let (next-symbol (eproj-home-entry/symbol next-home-entry))
               (if use-regexp
                 (string-match-pure? identifier next-symbol)
                 (string=? identifier next-symbol))))
      (begin
        (eproj-symbnav/switch-to-home-entry next-home-entry)
        (push current-home-entry
              eproj-symbnav/previous-homes)
        (setf eproj-symbnav/selected-loc (pop eproj-symbnav/next-homes)))
      (let* ((entry->string
              (eproj-language/tag->string-procedure
               (aif (gethash orig-major-mode eproj/languages-table)
                 it
                 (error "unsupported language %s" orig-major-mode))))
             (expanded-project-root
              (expand-file-name (eproj-project/root proj)))
             (tag->string
              (lambda (tag tag-proj)
                (let ((txt (funcall entry->string tag-proj tag))
                      (expanded-tag-file
                       (expand-file-name (eproj-tag/file tag))))
                  (cond ((string=? orig-file-name
                                   expanded-tag-file)
                         (propertize txt 'face 'font-lock-negation-char-face))
                        ((string-prefix? expanded-project-root
                                         expanded-tag-file)
                         ;; use italic instead of underscore
                         (propertize txt 'face 'italic))
                        (t
                         txt)))))
             (entry-tag #'first)
             (entry-string #'second)
             (entry-proj #'third)
             (entries
              ;; I'm not entirely sure where duplicates come from, but it's cheap
              ;; to remove them and at the same time I'm reluctant to tweak my
              ;; Emacs because of it's dynamically-typed lisp.
              (remove-duplicates-from-sorted-list-by
               (lambda (a b)
                 ;; compare results of tag->string
                 (string= (funcall entry-string a) (funcall entry-string b)))
               (sort
                ;; (loop
                ;;   for project in (cons proj
                ;;                        (eproj-get-all-related-projects proj))
                ;;   for check = (rest-safe
                ;;                (assq orig-major-mode
                ;;                      (eproj-project/tags project)))
                ;;   if check
                ;;   nconc
                ;;   (loop
                ;;     with identifiers =
                ;;     (if use-regexp
                ;;       (concat-lists
                ;;        (hash-table-entries-matching-re it identifier))
                ;;       (gethash identifier it nil))
                ;;     for tag in identifiers
                ;;     collect (cons tag
                ;;                   (funcall tag->string tag))))
                (map (lambda (tag-entry)
                       (destructuring-bind (tag . tag-proj)
                           tag-entry
                         (list tag
                               (funcall tag->string tag tag-proj)
                               tag-proj)))
                     (concatMap (lambda (proj)
                                  (aif (rest-safe
                                        (assq orig-major-mode
                                              (eproj-project/tags proj)))
                                    (map (lambda (tag)
                                           (cons tag proj))
                                         (if use-regexp
                                           (concat-lists
                                            (hash-table-entries-matching-re it identifier))
                                           (gethash identifier it nil)))
                                    nil))
                                (cons proj
                                      (eproj-get-all-related-projects proj))))
                (lambda (a b)
                  ;; compare results of tag->string
                  (string< (funcall entry-string a) (funcall entry-string b)))))))
        (cond ((null? entries)
               (error "No entries for %s %s"
                      (if use-regexp "regexp" "identifier")
                      identifier))
              ((null? (cdr entries))
               (funcall jump-to-home
                        (funcall entry-tag (car entries))
                        (funcall entry-proj (car entries))))
              (t
               (select-start-selection
                entries
                :buffer-name "Symbol homes"
                :after-init #'ignore
                :on-selection
                (lambda (idx)
                  (select-exit)
                  (let ((entry (elt entries idx)))
                    (funcall jump-to-home
                             (funcall entry-tag entry)
                             (funcall entry-proj entry))))
                :predisplay-function
                entry-string
                :preamble-function
                (lambda () "Choose symbol\n\n"))))))))

(defun eproj-symbnav/go-back ()
  (interactive)
  (if (null? eproj-symbnav/previous-homes)
    (error "no more previous go-to-definition entries")
    (begin
      (when (or (null? eproj-symbnav/next-homes)
                (and (not (null? eproj-symbnav/next-homes))
                     (not (eproj-home-entry=? eproj-symbnav/selected-loc
                                              (car eproj-symbnav/next-homes)))))
        (push eproj-symbnav/selected-loc eproj-symbnav/next-homes))
      (let ((prev-home (pop eproj-symbnav/previous-homes)))
        (setf eproj-symbnav/selected-loc prev-home)
        (eproj-symbnav/switch-to-home-entry prev-home)))))

(defun setup-eproj-symbnav ()
  (awhen (current-local-map)
    (def-keys-for-map it
      ("M-." eproj-symbnav/go-to-symbol-home)
      ("M-," eproj-symbnav/go-back)))
  (def-keys-for-map vim:normal-mode-local-keymap
    ("M-." eproj-symbnav/go-to-symbol-home)
    ("M-," eproj-symbnav/go-back)))

;;; epilogue

(provide 'eproj)

;; Local Variables:
;; End:

;; eproj.el ends here
