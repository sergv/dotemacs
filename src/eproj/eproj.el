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
;;                                should match absolute file names. Will be applied
;;                                to file-list argument too.
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
  extensions ;; list of <ext> strings
  extension-re
  load-procedure
  ;; function taking two arguments, eproj/project structure and
  ;; function of zero arguments returning list of files to load from.
  ;; Returns hashtable of (<identifier> . <eproj-tags>) bindings for specified
  ;; files, <eproj-tags> is a list of tags
  tag->string-procedure ;; function of one argument returning string
  synonym-modes ;; list of symbols, these modes will resolve to this language
                ;; during tag search
  normalize-identifier-before-navigation-procedure ;; Possibly strip unneeded
                                                   ;; information before
                                                   ;; performing navigation
  )

;;;; ctags facility

(defparameter *ctags-exec* (executable-find "ctags"))

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
    (concat "^\\("
            (macroexpand
             `(rx (or ,@+ctags-aux-fields+)))
            "\\):\\(.*\\)$")))

(defun eproj/run-ctags-on-files (lang-mode root-dir files out-buffer)
  (if (not (null *ctags-exec*))
    (with-current-buffer out-buffer
      (goto-char (point-max))
      (unless (looking-at-pure? "^$")
        (insert "\n"))
      (let ((ext-re (eproj-language/extension-re
                     (gethash lang-mode eproj/languages-table))))
        (with-temp-buffer
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
                     (buffer-substring-no-properties (point-min) (point-max))))))))
    (message "ctags executable not found")))

(defparameter eproj/ctags-string-cache
  (make-hash-table :test #'equal :size 997 :weakness t))

(defsubst eproj/ctags-cache-string (x)
  (assert (stringp x))
  (if-let (cached-x (gethash x eproj/ctags-string-cache))
    cached-x
    (puthash x x eproj/ctags-string-cache)))

;; tags parsing
(defun eproj/ctags-get-tags-from-buffer (buffer proj &optional simple-format?)
  "Constructs hash-table of (tag . eproj-tag) bindings extracted from buffer BUFFER.
BUFFER is expected to contain output of ctags command.

If SIMPLE-FORMAT? is t then do not attempt to parse <key>=<value> pairs
after ;\", and expect single character there instead (this won't be checked at
runtime but rather will be silently relied on)."
  (with-current-buffer buffer
    (save-match-data
      (goto-char (point-min))
      (let ((tags-table (make-hash-table :test #'equal)))
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
                                     (if (string-match +ctags-aux-fields-re+ entry)
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
                               fields)))
                (puthash symbol
                         (cons new-tag
                               (gethash symbol tags-table nil))
                         tags-table))))
          (forward-line 1))
        tags-table))))

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

(defun eproj/extract-tag-line (proj tag)
  "Fetch line where TAG is defined."
  (assert (eproj-tag-p tag) nil "Eproj tag is required.")
  (for-buffer-with-file
      (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
                                     (eproj-project/root proj))
    (save-excursion
      (goto-line1 (eproj-tag/line tag))
      (current-line))))


(defun eproj/load-ctags-project (lang-mode proj make-project-files)
  (let ((root (eproj-project/root proj)))
    (with-temp-buffer
      (eproj/run-ctags-on-files lang-mode
                                root
                                (funcall make-project-files)
                                (current-buffer))
      (prog1 (eproj/ctags-get-tags-from-buffer (current-buffer) proj)
        (erase-buffer)))))

(defun eproj/clojure-load-procedure (proj make-project-files)
  (assert (eproj-project-p proj))
  (when (memq 'java-mode (eproj-project/languages proj))
    (eproj/load-ctags-project 'java-mode proj make-project-files))
  )

(autoload 'eproj/load-haskell-project "eproj-haskell" nil nil)
(autoload 'eproj/haskell-tag->string "eproj-haskell" nil nil)

(defparameter eproj/languages
  (list
   (make-eproj-language
    :mode 'haskell-mode
    :extensions *haskell-extensions*
    :extension-re (concat "\\."
                          (regexp-opt *haskell-extensions*)
                          "$")
    :load-procedure
    (lambda (proj make-project-files)
      (eproj/load-haskell-project proj make-project-files))
    :tag->string-procedure #'eproj/haskell-tag->string
    :synonym-modes '(literate-haskell-mode
                     haskell-c-mode
                     c2hs-mode)
    :normalize-identifier-before-navigation-procedure
    #'haskell-remove-module-qualification)
   (make-eproj-language
    :mode 'c-mode
    :extensions '("c" "h")
    :extension-re (rx "."
                      (or "c" "h")
                      eol)
    :load-procedure
    (lambda (proj make-project-files)
      (eproj/load-ctags-project 'c-mode proj make-project-files))
    :tag->string-procedure #'eproj/c-tag->string
    :synonym-modes nil
    :normalize-identifier-before-navigation-procedure
    #'identity)
   (make-eproj-language
    :mode 'c++-mode
    :extensions '("c"
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
    (lambda (proj make-project-files)
      (eproj/load-ctags-project 'c++-mode proj make-project-files))
    :tag->string-procedure #'eproj/c-tag->string
    :synonym-modes nil
    :normalize-identifier-before-navigation-procedure
    #'identity)
   (make-eproj-language
    :mode 'python-mode
    :extensions '("py" "pyx" "pxd" "pxi")
    :extension-re (rx "."
                      (or "py" "pyx" "pxd" "pxi")
                      eol)
    :load-procedure
    (lambda (proj make-project-files)
      (eproj/load-ctags-project 'python-mode proj make-project-files))
    :tag->string-procedure #'eproj/generic-tag->string
    :synonym-modes nil
    :normalize-identifier-before-navigation-procedure
    #'identity)
   (make-eproj-language
    :mode 'clojure-mode
    :extensions '("clj" "java")
    :extension-re (rx "."
                      (or "clj"
                          "java")
                      eol)
    :load-procedure #'eproj/clojure-load-procedure
    :tag->string-procedure #'eproj/generic-tag->string
    :synonym-modes nil
    :normalize-identifier-before-navigation-procedure
    #'identity)
   (make-eproj-language
    :mode 'java-mode
    :extensions '("java")
    :extension-re (rx "."
                      (or "java")
                      eol)
    :load-procedure
    (lambda (proj make-project-files)
      (eproj/load-ctags-project 'java-mode proj make-project-files))
    :tag->string-procedure #'eproj/generic-tag->string
    :synonym-modes nil
    :normalize-identifier-before-navigation-procedure
    #'identity)))

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
    table)
  "Used by eproj-symbnav facility.")

;;; eproj-project

;;;; projects themselves

(defstruct (eproj-project
            (:conc-name eproj-project/))
  root
  aux-info ;; alist of (<symbol> . <symbol-dependent-info>) entries)
  tags ;; list of (language-major-mode . <tags-table>);
  ;; <tags-table> - hashtable of (symbol-str . eproj-tag) bindings
  related-projects ;; list of other project roots
  aux-files-source ;; list of other files or function that yields such list
  languages        ;; list of symbols - major-modes for related languages
  )

(defsubst eproj-project/get-aux-info (proj key)
  "Retrieve aux-data associated with a KEY in the project PROJ."
  (cadr-safe
   (assq key
         (eproj-project/aux-info proj))))

(defun eproj-project/aux-files (proj)
  (aif (eproj-project/aux-files-source proj)
    (cond ((functionp it)
           (funcall it))
          ((listp it)
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
  (eproj-get-initial-project-root/reset-cache)
  (eproj-resolve-abs-or-rel-name/reset-cache)
  (eproj-normalize-file-name/reset-cache)
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
  (let* ((root (eproj-get-initial-project-root-for-buf (current-buffer)))
         (proj (gethash root *eproj-projects* nil)))
    (when (not (null proj))
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
            (let ((new-tags (funcall load-proc proj (lambda () (list fname)))))
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
                 mode))))))

;; (defun eproj-get-aux-info-for-buffer-project (key)
;;   "Query aux info for current buffer's project for KEY."
;;   (rest-safe (assoc key
;;                     (eproj-project/aux-info
;;                      (eproj-get-project-for-buf (current-buffer))))))

(defmacro eproj-with-language-load-proc (lang-mode-var
                                         load-proc-var
                                         &rest
                                         body)
  "Execute BODY with LOAD-PROC-VAR bound to load procedure for mode in LANG-MODE-VAR."
  (declare (indent 2))
  (let ((lang-var (gensym "lang")))
    `(progn
       (assert (symbolp ,lang-mode-var)
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
  (let* ((files nil)
         (made-files nil)
         (make-project-files-func
          (lambda ()
            (if made-files
              files
              (progn
                (setf files (eproj-get-project-files proj)
                      made-files t)
                files)))))
    (setf (eproj-project/tags proj)
          (map (lambda (lang-mode)
                 (eproj-with-language-load-proc lang-mode load-proc
                   (let ((new-tags (funcall load-proc proj make-project-files-func)))
                     (assert (and (not (null new-tags))
                                  (hash-table-p new-tags)))
                     (when (= 0 (hash-table-count new-tags))
                       (error "Warning while reloading: project %s loaded no tags for language %s"
                              (eproj-project/root proj)
                              lang-mode))
                     (cons lang-mode new-tags))))
               (eproj-project/languages proj))))
  nil)

(defun eproj-populate-from-eproj-info! (proj aux-info)
  (let ((languages (aif (cdr-safe (assq 'languages aux-info))
                     it
                     (begin
                       (message "warning: no languages defined for project %s"
                                (eproj-project/root proj))
                       nil))))
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
    (when (file-exists-p eproj-info-file)
      eproj-info-file)))

(defun eproj-read-eproj-info-file (filename)
  "Read .eproj-info file from FILENAME."
  (unless (file-exists-p filename)
    (error ".eproj-info file does not exist: %s" filename))
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (goto-char (point-min))
    (read (current-buffer))))

(defun eproj-reload-project! (proj)
  "Update project PROJ - re-read its .eproj-info file and update project
variables accordingly."
  (assert (not (null (eproj-project/root proj))))
  (assert (stringp (eproj-project/root proj)))
  (eproj-populate-from-eproj-info!
   proj
   (eproj-read-eproj-info-file
    (eproj-get-eproj-info-from-dir (eproj-project/root proj)))))

;;;; project creation

(defun eproj-make-project (root aux-info)
  (assert (stringp root)
          nil
          "Project root must be a string: %s" root)
  (unless (and (file-exists-p root)
               (file-directory-p root))
    (error "Invalid project root, existing directory required: %s" root))
  (let ((proj
         (make-eproj-project :root root
                             :tags nil
                             :aux-info nil
                             :related-projects nil
                             :aux-files-source nil
                             :languages nil)))
    (when (null proj)
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
      (insert "number of tags loaded: "
              (let ((tag-count 0))
                (dolist (tags-entry (eproj-project/tags proj))
                  (let ((lang-tags (hash-table->alist (cdr tags-entry))))
                    (setf tag-count
                          (+ tag-count (length lang-tags)))))
                (number->string tag-count))
              "\n")
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

(defun eproj-get-project (root aux-info)
  (aif (gethash root *eproj-projects* nil)
    it
    (let ((proj (eproj-make-project root aux-info)))
      (puthash (eproj-project/root proj)
               proj
               *eproj-projects*)
      proj)))

(defun-caching eproj-get-initial-project-root (path) eproj-get-initial-project-root/reset-cache (path)
  "Get (<initial-project-root> <project-type> <aux-info>) triple for project
governing PATH."
  (if-let (initial-root (eproj/find-eproj-file-location path))
    initial-root
    (error "File .eproj-info not found when looking from %s directory"
           path)))

(defun eproj-get-initial-project-root-and-aux-info (path)
  "Get (<initial-project-root> <project-type> <aux-info>) triple for project
governing PATH."
  (let ((initial-root (eproj-get-initial-project-root path)))
    (if-let (proj (gethash initial-root *eproj-projects* nil))
      (values initial-root (eproj-project/aux-info proj))
      (if-let (eproj-info-file (eproj-get-eproj-info-from-dir initial-root))
        (values initial-root
                (eproj-read-eproj-info-file eproj-info-file))
        (error ".eproj-info file does not exist at %s"
               initial-root)))))

(defmacro eproj/evaluate-with-caching-buffer-local-var (value-expr
                                                        buffer-expr
                                                        caching-var
                                                        value-predicate)

  (let* ((buffer-var (gensym "buffer"))
         (is-nil (gensym "is-nil"))
         (is-nil-value `(quote ,is-nil)))
    `(let ((,buffer-var ,buffer-expr))
       (with-current-buffer ,buffer-var
         (when (null ,caching-var)
           (setf ,caching-var (or ,value-expr ,is-nil-value)))
         (if (eq ,caching-var ,is-nil-value)
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
   #'stringp))

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
  (assert (or (file-exists-p path)
              (file-directory-p path))
          nil
          "Cannot get eproj project for nonexisting path: %s"
          path)
  (multiple-value-bind (initial-root aux-info)
      (eproj-get-initial-project-root-and-aux-info path)
    (eproj-get-project initial-root
                       aux-info)))

(defun eproj-get-project-files (proj)
  "Retrieve project files for PROJ depending on it's type."
  ;; Cached files are necessarily from file-list and intended for projects whose
  ;; list of files does not change and may be cached.
  (let ((ignored-files-absolute-regexps
         (cdr-safe (assq 'ignored-files (eproj-project/aux-info proj)))))
    (if-let (cached-files
             (eproj-project/get-aux-info proj
                                         'eproj-get-project-files/cached-files))
      cached-files
      ;; if there's file-list then read it and store to cache
      (if-let (file-list (eproj-project/get-aux-info proj 'file-list))
        (let ((file-list-filename (eproj-resolve-abs-or-rel-name
                                   file-list
                                   (eproj-project/root proj))))
          (when (or (null file-list-filename)
                    (not (file-exists-p file-list-filename)))
            (error "Cannot find file list: filename %s at %s" file-list file-list-filename))
          (let ((filter-ignored-files
                 (lambda (files)
                   (aif ignored-files-absolute-regexps
                     (let ((regexp
                            (mapconcat (lambda (x) (concat "\\(?:" x "\\)"))
                                       it
                                       "\\|")))
                       (filter (lambda (fname)
                                 (not (string-match-p regexp fname)))
                               files))
                     files)))
                (list-of-files
                 (with-temp-buffer
                   (insert-file-contents-literally file-list-filename)
                   (goto-char (point-min))
                   (read (current-buffer)))))
            (assert (listp list-of-files))
            (let ((resolved-files
                   (funcall filter-ignored-files
                            (map (lambda (filename)
                                   (eproj-resolve-abs-or-rel-name
                                    filename
                                    (eproj-project/root proj)))
                                 list-of-files))))
              (assert (all? (lambda (filename)
                              (and (stringp filename)
                                   (file-exists-p filename)))
                            resolved-files))
              ;; add to cache
              (push (list 'eproj-get-project-files/cached-files resolved-files)
                    (eproj-project/aux-info proj))
              resolved-files)))
        (eproj/find-rec
         (eproj-project/root proj)
         (concatMap (lambda (lang)
                      (assert (symbolp lang))
                      (eproj-language/extensions
                       (gethash lang eproj/languages-table)))
                    (eproj-project/languages proj))
         ignored-files-absolute-regexps
         *ignored-directories*
         *ignored-directory-prefixes*)
        ))))

(defvar eproj/find-program-type
  (or (and (not (platform-os-type? 'windows))
           (executable-find "find")
           'find)
      (and (executable-find "busybox")
           'busybox))
  "Can be either 'find, 'busybox or nil.")

(defvar eproj/find-program-executable
  (pcase eproj/find-program-type
    (`find "find")
    (`busybox "busybox")))

(defun eproj/find-rec (root
                       extensions
                       ignored-files-absolute-regexps
                       ignored-directories
                       ignored-directory-prefixes)
  (when (null? extensions)
    (error "no extensions for project %s" root))
  (if eproj/find-program-type
    (let* ((ignored-dirs
            (nconc
             (map (lambda (dir) (list "-ipath" (concat "*/" dir)))
                  ignored-directories)
             (map (lambda (dir) (list "-ipath" (concat "*/" dir "*")))
                  ignored-directory-prefixes)))
           (ignored-files
            (map (pcase eproj/find-program-type
                   (`find
                    (lambda (re) (list "-iregex" re)))
                   (`busybox
                    (lambda (re) (list "-regex" re)))
                   (_
                    (error "eproj/find-program-type has invalid value: %s"
                           eproj/find-program-type)))
                 ignored-files-absolute-regexps))
           (exts
            (map (lambda (ext) (list "-iname" (concat "*." ext)))
                 extensions))
           (find-cmd (or eproj/find-program-executable
                         (error "eproj/find-program-type has invalid value: %s"
                                eproj/find-program-type)))
           (cmd
            (util:flatten
             (list (when (eq? 'busybox eproj/find-program-type)
                     "find")
                   (when (eq? 'find eproj/find-program-type)
                     '("-O3"))
                   root
                   (when (eq? 'find eproj/find-program-type)
                     '("-regextype" "emacs"))
                   (when ignored-dirs
                     (list
                      "-type" "d"
                      "("
                      (sep-by "-o" ignored-dirs)
                      ")"
                      "-prune"
                      "-o"))
                   (when ignored-files
                     (list
                      "-type" "f"
                      "("
                      (sep-by "-o" ignored-files)
                      ")"
                      "-prune"
                      "-o"))
                   "-type" "f"
                   "("
                   (sep-by "-o" exts)
                   ")"
                   "-print"))))
      (with-temp-buffer
        (with-disabled-undo
         (with-inhibited-modification-hooks
          (apply #'call-process
                 find-cmd
                 nil
                 t
                 nil
                 cmd)
          (split-into-lines
           (buffer-substring-no-properties (point-min)
                                           (point-max)))))))
    (let ((ext-re (concat "\\."
                          (regexp-opt extensions)
                          "$"))
          (ignored-files-absolute-re
           (mapconcat (lambda (x) (concat "\\(?:" x "\\)"))
                      ignored-files-absolute-regexps
                      "\\|"))
          (ignored-dirs-re
           (concat "\\(?:/\\|^\\)\\(?:"
                   (regexp-opt ignored-directories)
                   "\\)\\(?:/\\|$\\)")))
      (find-rec root
                :filep
                (if ignored-files-absolute-regexps
                  (lambda (path)
                    (and (string-match-p ext-re path)
                         (not (string-match-p ignored-files-absolute-re path))))
                  (lambda (path)
                    (string-match-p ext-re path)))
                :do-not-visitp
                (lambda (path)
                  (string-match-pure? ignored-dirs-re
                                      (file-name-nondirectory path)))))))

(defun eproj-get-related-projects (root aux-info)
  "Return list of roots of related project for folder ROOT and AUX-INFO.
AUX-INFO is expected to be a list with entry (related { <abs-path> | <rel-path> }* ).
Returns nil if no relevant entry found in AUX-INFO."
  (let ((project-root root))
    (when-let (related-entry (cdr-safe (assq 'related aux-info)))
      (map (lambda (path)
             (assert (stringp path) nil
                     "invalid entry under related clause, string expected %s"
                     path)
             (cond ((file-directory-p path)
                    path)
                   ((file-directory-p (expand-file-name path project-root))
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
    (when-let (aux-files-entry (cdr-safe (assq 'aux-files aux-info)))
      (lambda ()
        (with-temp-buffer
          (cd project-root)
          (mapcan (lambda (item)
                    (assert (listp item) nil
                            "invalid entry under aux-files clause, list expected: %s"
                            item)
                    (cond ((eq (car-safe item) 'tree)
                           (let ((tree-root (cadr-safe item))
                                 (patterns (cddr-safe item)))
                             (assert (and (not (null tree-root))
                                          (file-exists-p tree-root)
                                          (file-directory-p tree-root))
                                     nil
                                     "Invalid tree root under aux-files/tree clause: %s"
                                     tree-root)
                             (assert (and (listp patterns)
                                          (not (null patterns)))
                                     nil
                                     "Invalid patterns under aux-files/tree clause: %s"
                                     patterns)
                             (find-rec tree-root
                                       :filep
                                       (lambda (path)
                                         (any? (lambda (regexp)
                                                 (string-match-p regexp path))
                                               patterns)))))
                          (t
                           nil)))
                  aux-files-entry))))))

(defun eproj/find-eproj-file-location (path)
  "Find closest directory parent of PATH that contains .eproj-info file."
  (assert (stringp path))
  (let ((dir (if (file-directory-p path)
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

(defun-caching eproj-resolve-abs-or-rel-name (path dir) eproj-resolve-abs-or-rel-name/reset-cache (path dir)
  (if (or (file-exists-p path)
          (file-directory-p path))
    path
    (if (file-name-absolute-p path)
      (error "Non-existing absolute file name: %s, probably something went wrong" path)
      (let ((abs-path (concat (eproj-normalize-file-name dir) "/" path)))
        (if (or (file-exists-p abs-path)
                (file-directory-p abs-path))
          abs-path
          (error "File %s does not exist, try `eproj-update-buffer-project'"
                 abs-path))))))

(defun-caching eproj-normalize-file-name (path) eproj-normalize-file-name/reset-cache (path)
  (strip-trailing-slash (normalize-file-name (expand-file-name path))))

(defun eproj--get-buffer-directory (buffer)
  "Get directory associated with BUFFER, either throug visited file
or `default-directory', if no file is visited."
  (with-current-buffer buffer
    (or (when buffer-file-truename
          (file-name-directory buffer-file-truename))
        default-directory)))

(autoload 'eproj-symbnav/describe "eproj-symbnav" nil t)
(autoload 'eproj-symbnav/reset "eproj-symbnav" nil t)
(autoload 'eproj-symbnav/resolve-synonym-modes "eproj-symbnav" nil t)
(autoload 'eproj-symbnav/go-to-symbol-home "eproj-symbnav" nil t)
(autoload 'eproj-symbnav/go-back "eproj-symbnav" nil t)
(autoload 'setup-eproj-symbnav "eproj-symbnav" nil nil)

;;; epilogue

(provide 'eproj)

;; Local Variables:
;; End:

;; eproj.el ends here
