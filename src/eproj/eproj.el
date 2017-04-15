;; eproj.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 30 December 2012
;; Description:
;;
;; Format of .eproj-info
;; [(languages <langs>)] - it's not a good practice to omit this; if it's empty
;;                         then no attempt will be made to infer languages used
;; [(related <abs-or-rel-dir>*])
;; [(aux-files
;;   [(tree <tree-root> <pattern>*)])]
;; [(ignored-files <regexp>+)] - ignored filenames, <regexp>
;;                               should match absolute file names. Will be applied
;;                               to file-list argument too.
;; [(file-list <abs-or-rel-file>)] - filename listing all files in lisp format,
;;                                   e.g. ("foo" "c:/bar.txt" "../quux.log")
;;
;; [(create-tag-files <t-or-nil>)] - whether to cache tags in tag files for this project
;;
;; ;; these are mostly for haskell
;; [(tag-file <abs-or-rel-file>)]
;;
;; [...] - optional directive
;; <abs-or-rel-dir> - absolute or relative path to directory
;; <abs-or-rel-file> - absolute or relative path to file
;; <langs> - list of major-mode symbols
;; <proj-type> - symbol naming type of project, will be verified
;;               agains inferred project type
;; <tree-root> - absolute path to existing directory
;; <pattern> - regular expression
;; <arg> - emacs strings, arguments to the command

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'common)
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

;; Return associative list of tag properties.
(defsubst eproj-tag/properties (tag-struct)
  (cdddr tag-struct))

;;; eproj languages

(defstruct (eproj-language
            (:conc-name eproj-language/))
  mode
  extensions ;; list of <ext> strings
  extension-re
  create-tags-procedure
  ;; Function of three arguments:
  ;; 1. current eproj/project structure
  ;; 2. function of zero arguments returning list of files to load from
  ;; 3. function of one argument - a buffer, that should parse tags from the
  ;; passed buffer.
  ;;
  ;; Returns whatever the third function returned.
  parse-tags-procedure
  ;; Function of one argument - buffer with tag text to parse,
  ;; created by e.g. create-tags-procedure.
  ;; Should return hash table of tags - hashtable of (<identifier> . <eproj-tags>)
  ;; bindings for specified files, where <eproj-tags> is a list of tags.

  show-tag-kind-procedure ;; function that takes a tag and returs a string
  tag->string-func ;; function of one argument, a tag, returning string
  synonym-modes ;; list of symbols, these modes will resolve to this language
  ;; during tag search
  normalize-identifier-before-navigation-procedure ;; Possibly strip unneeded
  ;; information before
  ;; performing navigation
  )

(defun* mk-eproj-lang (&key mode
                            extensions
                            create-tags-procedure
                            parse-tags-procedure
                            show-tag-kind-procedure
                            tag->string-func
                            synonym-modes
                            normalize-identifier-before-navigation-procedure)
  (make-eproj-language
   :mode mode
   :extensions extensions
   :extension-re (concat "\\."
                         (regexp-opt extensions)
                         "$")
   :create-tags-procedure create-tags-procedure
   :parse-tags-procedure parse-tags-procedure
   :show-tag-kind-procedure show-tag-kind-procedure
   :tag->string-func tag->string-func
   :synonym-modes synonym-modes
   :normalize-identifier-before-navigation-procedure
   normalize-identifier-before-navigation-procedure))

;;;; language definitions

(defun eproj/generic-tag-kind (tag)
  (format "%s" (eproj-tag/properties tag)))

(defun eproj/generic-tag->string (proj tag)
  (cl-assert (eproj-tag-p tag))
  (concat "Generic tag "
          (eproj-tag/symbol tag)
          "\n"
          (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
                                         (eproj-project/root proj))
          ":"
          (number->string (eproj-tag/line tag))
          "\n"
          (eproj/generic-tag-kind tag)
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

(defun eproj/c-tag-kind (tag)
  (cdr-safe (assq 'kind (eproj-tag/properties tag))))

(defun eproj/c-tag->string (proj tag)
  (cl-assert (eproj-tag-p tag))
  (concat (eproj-tag/symbol tag)
          (awhen (eproj/c-tag-kind tag)
            (concat " [" it "]"))
          "\n"
          (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
                                         (eproj-project/root proj))
          ":"
          (number->string (eproj-tag/line tag))
          "\n"
          (eproj/extract-tag-line proj tag)
          "\n"))

(defun eproj/java-tag-kind (tag)
  (concat
   (cdr-safe (assq 'kind (eproj-tag/properties tag)))
   (awhen (assq 'access (eproj-tag/properties tag))
     (concat "/" (cdr it)))))

(defun eproj/java-tag->string (proj tag)
  (cl-assert (eproj-tag-p tag))
  (concat (eproj-tag/symbol tag)
          (awhen (eproj/java-tag-kind tag)
            (concat " [" it "]"))
          "\n"
          (awhen (assq 'class (eproj-tag/properties tag))
            (concat (cdr it)
                    "."
                    (eproj-tag/symbol tag)
                    "\n"))
          (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
                                         (eproj-project/root proj))
          ":"
          (number->string (eproj-tag/line tag))
          "\n"
          (when (eproj-tag/line tag)
            (concat (eproj/extract-tag-line proj tag)
                    "\n"))))

(defun eproj/extract-tag-line (proj tag)
  "Fetch line where TAG is defined."
  (cl-assert (eproj-tag-p tag) nil "Eproj tag is required.")
  (for-buffer-with-file
      (eproj-resolve-abs-or-rel-name (eproj-tag/file tag)
                                     (eproj-project/root proj))
    (save-excursion
      (goto-line1 (eproj-tag/line tag))
      (current-line))))


(defun eproj/load-ctags-project (lang-mode proj make-project-files parse-tags-proc)
  (with-temp-buffer
    ;; (when eproj-verbose-tag-loading
    ;;   (message "Running ctags for %s in project %s"
    ;;            lang-mode
    ;;            (eproj-project/root proj))
    ;;   (redisplay))
    (eproj/run-ctags-on-files lang-mode
                              (eproj-project/root proj)
                              (funcall make-project-files)
                              (current-buffer))
    ;; (when eproj-verbose-tag-loading
    ;;   (message "Loading %s tags in project %s"
    ;;            lang-mode
    ;;            (eproj-project/root proj)
    ;;            (redisplay)))
    (prog1 (funcall parse-tags-proc (current-buffer))
      (erase-buffer))))

(autoload 'eproj/get-fast-tags-tags-from-buffer "eproj-haskell")

(defparameter eproj/languages
  (list
   (mk-eproj-lang
    :mode 'haskell-mode
    :extensions (--filter (not (member it '("x" "alex" "y" "happy" "ly")))
                          *haskell-extensions*)
    :create-tags-procedure
    #'eproj/create-haskell-tags
    :parse-tags-procedure
    #'eproj/get-fast-tags-tags-from-buffer
    :show-tag-kind-procedure #'eproj/haskell-tag-kind
    :tag->string-func #'eproj/haskell-tag->string
    :synonym-modes '(literate-haskell-mode
                     haskell-c-mode
                     haskell-c2hs-mode)
    :normalize-identifier-before-navigation-procedure
    #'haskell-remove-module-qualification)
   (mk-eproj-lang
    :mode 'c-mode
    :extensions '("c" "h")
    :create-tags-procedure
    (lambda (proj make-project-files parse-tags-proc)
      (eproj/load-ctags-project 'c-mode proj make-project-files parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/c-tag-kind
    :tag->string-func #'eproj/c-tag->string
    :synonym-modes nil
    :normalize-identifier-before-navigation-procedure
    #'identity)
   (mk-eproj-lang
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
                  "incl"
                  "ino")
    :create-tags-procedure
    (lambda (proj make-project-files parse-tags-proc)
      (eproj/load-ctags-project 'c++-mode proj make-project-files parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/c-tag-kind
    :tag->string-func #'eproj/c-tag->string
    :synonym-modes nil
    :normalize-identifier-before-navigation-procedure
    #'identity)
   (mk-eproj-lang
    :mode 'python-mode
    :extensions '("py" "pyx" "pxd" "pxi")
    :create-tags-procedure
    (lambda (proj make-project-files parse-tags-proc)
      (eproj/load-ctags-project 'python-mode proj make-project-files parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/generic-tag-kind
    :tag->string-func #'eproj/generic-tag->string
    :synonym-modes nil
    :normalize-identifier-before-navigation-procedure
    #'identity)
   (mk-eproj-lang
    :mode 'clojure-mode
    :extensions '("clj" "java")
    :create-tags-procedure
    (lambda (proj make-project-files parse-tags-proc)
      ;; Use Java mode for now since there's no dedicated Clojure config yet.
      (eproj/load-ctags-project 'java-mode proj make-project-files parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/generic-tag-kind
    :tag->string-func #'eproj/generic-tag->string
    :synonym-modes nil
    :normalize-identifier-before-navigation-procedure
    #'identity)
   (mk-eproj-lang
    :mode 'java-mode
    :extensions '("java")
    :create-tags-procedure
    (lambda (proj make-project-files parse-tags-proc)
      (eproj/load-ctags-project 'java-mode proj make-project-files parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/java-tag-kind
    :tag->string-func #'eproj/java-tag->string
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

(defun eproj/resolve-synonym-modes (mode)
  "Replace modes that are similar to some other known modes"
  (aif (gethash mode eproj/synonym-modes-table)
      it
    mode))

;;; eproj-project

;;;; projects themselves

(defstruct (eproj-project
            (:conc-name eproj-project/))
  root     ;; normalized directory name
  aux-info ;; alist of (<symbol> . <symbol-dependent-info>) entries
  tags     ;; list of (language-major-mode . <tags-table>);
  ;; <tags-table> - hashtable of (symbol-str . eproj-tag) bindings
  related-projects ;; list of other project roots
  aux-files-source ;; list of other files or function that yields such list
  languages ;; list of symbols - major-modes for related languages
  cached-file-list ;; stores list of filenames, if file list is specified in .eproj-info
  ignored-files-regexps ;; list of absolute filename regexps to ignore in current
  ;; project
  file-list-filename ;; list of files, if specified in aux-info via 'file-list
  create-tag-files ;; boolean, whether to cache tags for this project
  ;; in files
  )

(defsubst eproj-project/query-aux-info (aux-info key)
  "Retrieve aux-data associated with a KEY in the project PROJ."
  (let ((entry (assq key aux-info)))
    (cl-assert (or (null? entry)
                   (= (length entry) 2)))
    (cadr-safe entry))
  ;; (cadr-safe
  ;;  (assq key
  ;;        (eproj-project/aux-info proj)))
  )

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

(defun eproj-project/root< (proj-a proj-b)
  (string< (eproj-project/root proj-a)
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
         (proj (gethash root *eproj-projects* nil))
         (eproj-verbose-tag-loading nil))
    (when (not (null proj))
      (let* ((proj (eproj-get-project-for-buf (current-buffer)))
             (fname (expand-file-name buffer-file-name))
             (non-fname-tag-func
              (lambda (tag)
                (not (string= fname
                              (expand-file-name
                               (eproj-tag/file tag))))))
             (mode (eproj/resolve-synonym-modes major-mode)))
        (unless (memq mode
                      (eproj-project/languages proj))
          (error "Project %s does not manage %s files"
                 (eproj-project/root proj)
                 mode))
        (if-let (old-tags (cdr-safe (assoc mode (eproj-project/tags proj))))
            (let ((new-tags
                   (eproj/load-tags-for-mode
                    proj
                    mode
                    (lambda () (list fname))
                    ;; Ignore tag files since we want to reload tags for a
                    ;; single file and collect tags exactly in the file, not
                    ;; the cached ones!
                    :consider-tag-files nil)))
              (maphash (lambda (symbol-str tags)
                         (puthash symbol-str
                                  (-filter non-fname-tag-func tags)
                                  old-tags))
                       old-tags)
              (hash-table-merge-with!
               (lambda (symbol-str tags-old tags-new)
                 (append tags-old
                         tags-new))
               old-tags
               new-tags))
          (error "Project '%s' does not have tags for '%s'"
                 (eproj-project/root proj)
                 mode))))))

(defun eproj/tag-file-name (proj mode)
  "Return absolute path for tag file for project PROJ and language mode MODE to
cache tags in."
  (concat +tmp-global-path+
          "/tags-"
          (sha1 (eproj-project/root proj))
          "-"
          (format "%s" mode)))

(defun* eproj/load-tags-for-mode (proj mode make-project-files &key (consider-tag-files t))
  (if-let ((lang (gethash mode eproj/languages-table))
           (create-tags-procedure (eproj-language/create-tags-procedure lang))
           (parse-tags-procedure (eproj-language/parse-tags-procedure lang)))
      (if (and consider-tag-files
               (eproj-project/create-tag-files proj))
          (let ((tag-file (eproj/tag-file-name proj mode)))
            (if (file-exists-p tag-file)
                (with-temp-buffer
                  (insert-file-contents-literally tag-file)
                  (funcall parse-tags-procedure (current-buffer)))
              (funcall create-tags-procedure
                       proj
                       make-project-files
                       (lambda (buf)
                         (with-current-buffer buf
                           (write-region (point-min) (point-max) tag-file)
                           (funcall parse-tags-procedure buf))))))
        (funcall create-tags-procedure
                 proj
                 make-project-files
                 parse-tags-procedure))
    (error "Failed loading tags for mode '%s': cannot resolve language" mode)))

(defun eproj-reload-tags (proj)
  "Reload tags for PROJ."
  (let* ((files nil)
         (made-files nil)
         (make-project-files-func
          (lambda ()
            (if made-files
                files
              (progn
                (setf files (aif (eproj-project/aux-files proj)
                                (append
                                 (eproj-get-project-files proj)
                                 it)
                              (eproj-get-project-files proj))
                      made-files t)
                files)))))
    (setf (eproj-project/tags proj)
          (-map (lambda (lang-mode)
                  (let ((new-tags (eproj/load-tags-for-mode proj
                                                            lang-mode
                                                            make-project-files-func
                                                            :consider-tag-files t)))
                    (cl-assert (and (not (null new-tags))
                                    (hash-table-p new-tags)))
                    (when (= 0 (hash-table-count new-tags))
                      (error "Warning while reloading: project %s loaded no tags for language %s"
                             (eproj-project/root proj)
                             lang-mode))
                    (cons lang-mode new-tags)))
                (eproj-project/languages proj))))
  nil)

(defun eproj-populate-from-eproj-info! (proj aux-info)
  (let ((languages (aif (cdr-safe (assq 'languages aux-info))
                       it
                     (progn
                       (message "warning: no languages defined for project %s"
                                (eproj-project/root proj))
                       nil)))
        (ignored-files-regexps
         (cdr-safe (assq 'ignored-files aux-info)))
        (file-list-filename
         (awhen (eproj-project/query-aux-info aux-info 'file-list)
           (let ((fname (eproj-resolve-abs-or-rel-name it (eproj-project/root proj))))
             (when (or (null fname)
                       (not (file-exists-p fname)))
               (error "File list filename does not exist: %s" fname))
             fname)))
        (create-tag-files
         (eproj-project/query-aux-info aux-info 'create-tag-files)))
    (setf (eproj-project/aux-info proj) aux-info
          (eproj-project/related-projects proj) (eproj-get-related-projects (eproj-project/root proj) aux-info)
          (eproj-project/aux-files-source proj) (eproj-make-aux-files-constructor (eproj-project/root proj) aux-info)
          (eproj-project/languages proj) languages
          (eproj-project/ignored-files-regexps proj) ignored-files-regexps
          (eproj-project/file-list-filename proj) file-list-filename
          (eproj-project/create-tag-files proj) create-tag-files)
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
  (cl-assert (not (null (eproj-project/root proj))))
  (cl-assert (stringp (eproj-project/root proj)))
  (eproj-populate-from-eproj-info!
   proj
   (eproj-read-eproj-info-file
    (eproj-get-eproj-info-from-dir (eproj-project/root proj)))))

;;;; project creation

(defun eproj-make-project (root aux-info)
  (cl-assert (stringp root)
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
                             :languages nil
                             :cached-file-list nil
                             :ignored-files-regexps nil)))
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
        (-map (lambda (entry)
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
        (dolist (buf (-filter (lambda (buf)
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

;; Get <initial-project-root> for project governing PATH.
(defun-caching eproj-get-initial-project-root (path) eproj-get-initial-project-root/reset-cache (path)
  (aif (eproj/find-eproj-file-location path)
      it
    (error "File .eproj-info not found when looking from %s directory"
           path)))

(defmacro eproj/evaluate-with-caching-buffer-local-var (value-expr
                                                        buffer-expr
                                                        caching-var
                                                        value-predicate)
  (let* ((buffer-var '#:buffer)
         (is-nil '#:is-nil)
         (is-nil-value `(quote ,is-nil)))
    `(let ((,buffer-var ,buffer-expr))
       (with-current-buffer ,buffer-var
         (when (null ,caching-var)
           (setf ,caching-var (or ,value-expr ,is-nil-value)))
         (if (eq ,caching-var ,is-nil-value)
             nil
           (progn
             (cl-assert (funcall ,value-predicate ,caching-var)
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
   (ignore-errors
     (eproj-get-initial-project-root (eproj--get-buffer-directory buffer)))
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
  (cl-assert (or (file-exists-p path)
                 (file-directory-p path))
             nil
             "Cannot get eproj project for nonexisting path: %s"
             path)
  ;; Try looking for project with PATH root, if there's none then construct
  ;; proper initial project root by looking for .eproj-info file and try with
  ;; those.
  (if-let (path-proj (gethash path *eproj-projects* nil))
      path-proj
    (let ((initial-root (eproj-get-initial-project-root path)))
      (if-let (proj (gethash initial-root *eproj-projects* nil))
          proj
        (if-let (eproj-info-file (eproj-get-eproj-info-from-dir initial-root))
            (let ((proj (eproj-make-project initial-root
                                            (eproj-read-eproj-info-file eproj-info-file))))
              (puthash (eproj-project/root proj)
                       proj
                       *eproj-projects*)
              proj)
          (error ".eproj-info file does not exist at %s"
                 initial-root))))))

(defun eproj-get-project-files (proj)
  "Retrieve project files for PROJ depending on it's type."
  ;; Cached files are necessarily from file-list and intended for projects whose
  ;; list of files does not change and may be cached.
  (if-let (cached-files (eproj-project/cached-file-list proj))
      cached-files
    ;; if there's file-list then read it and store to cache
    (if-let (file-list-filename (eproj-project/file-list-filename proj))
        (let ((filter-ignored-files
               (lambda (files)
                 (aif (eproj-project/ignored-files-regexps proj)
                     (let ((regexp
                            (mapconcat (lambda (x) (concat "\\(?:" x "\\)"))
                                       it
                                       "\\|")))
                       (-filter (lambda (fname)
                                  (not (string-match-p regexp fname)))
                                files))
                   files)))
              (list-of-files
               (with-temp-buffer
                 (insert-file-contents-literally file-list-filename)
                 (goto-char (point-min))
                 (read (current-buffer)))))
          (cl-assert (listp list-of-files))
          (let ((resolved-files
                 (funcall filter-ignored-files
                          (-map (lambda (filename)
                                  (eproj-resolve-abs-or-rel-name
                                   filename
                                   (eproj-project/root proj)))
                                list-of-files))))
            (cl-assert (--all? (and (stringp it)
                                    (file-exists-p it))
                               resolved-files))
            (setf (eproj-project/cached-file-list proj) resolved-files)
            resolved-files))
      (find-rec*
       :root (eproj-project/root proj)
       :extensions-globs (-mapcat (lambda (lang)
                                    (cl-assert (symbolp lang))
                                    (--map (concat "*." it)
                                           (eproj-language/extensions
                                            (gethash lang eproj/languages-table))))
                                  (eproj-project/languages proj))
       :ignored-files-absolute-regexps (eproj-project/ignored-files-regexps proj)
       :ignored-directories *ignored-directories*
       :ignored-directory-prefixes *ignored-directory-prefixes*))))

(defun eproj-get-related-projects (root aux-info)
  "Return list of roots of related project for folder ROOT and AUX-INFO.
AUX-INFO is expected to be a list with entry (related { <abs-path> | <rel-path> }* ).
Returns nil if no relevant entry found in AUX-INFO."
  (awhen (cdr-safe (assq 'related aux-info))
    (-map (lambda (path)
            (cl-assert (stringp path) nil
                       "invalid entry under related clause, string expected %s"
                       path)
            (progn ;; condition-case err
              (eproj-resolve-abs-or-rel-name path root)
              ;; (error
              ;;  (error "invalid related-project entry: non-existing absolute/relative directory: %s\n%s"
              ;;         path
              ;;         err))
              ))
          it)))

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
          (-mapcat (lambda (item)
                     (cl-assert (listp item) nil
                                "invalid entry under aux-files clause, list expected: %s"
                                item)
                     (cond ((eq (car-safe item) 'tree)
                            (let ((tree-root (cadr-safe item))
                                  (patterns (cddr-safe item)))
                              (cl-assert (and (not (null tree-root))
                                              (file-exists-p tree-root)
                                              (file-directory-p tree-root))
                                         nil
                                         "Invalid tree root under aux-files/tree clause: %s"
                                         tree-root)
                              (cl-assert (and (listp patterns)
                                              (not (null patterns)))
                                         nil
                                         "Invalid patterns under aux-files/tree clause: %s"
                                         patterns)
                              (find-rec tree-root
                                        :filep
                                        (lambda (path)
                                          (--any? (string-match-p it path)
                                                  patterns)))))
                           (t
                            (error "Invalid 'aux-files entry: 'tree clause not found"))))
                   aux-files-entry))))))

(defun eproj/find-eproj-file-location (path)
  "Find closest directory parent of PATH that contains .eproj-info file."
  (cl-assert (stringp path))
  (let ((dir (if (file-directory-p path)
                 path
               (file-name-directory path))))
    (awhen (locate-dominating-file dir ".eproj-info")
      (eproj-normalize-file-name it))))

(defvar eproj/default-projects (make-hash-table :test #'eq)
  "Hash table mapping major mode symbols to lists of project roots, that should
be regarded as related projects when looking for tags in said major mode from any
project.")

(defun eproj-get-all-related-projects (proj major-mode)
  "Return eproj-project structures of projects realted to PROJ including PROJ itself."
  (cl-assert (eproj-project-p proj) nil
             "Not a eproj-project structure: %s" proj)
  (let ((items nil)
        (visited (let ((tbl (make-hash-table :test #'equal)))
                   (puthash (eproj-project/root proj) t tbl)
                   tbl))
        (projs (-map #'eproj-get-project-for-path
                     (append
                      (gethash major-mode eproj/default-projects nil)
                      (eproj-project/related-projects proj)))))
    (while projs
      (let* ((p (pop projs))
             (root (eproj-project/root p)))
        (unless (gethash root visited nil)
          (setf projs (append (-map #'eproj-get-project-for-path
                                    (eproj-project/related-projects p))
                              projs)
                (gethash root visited) t)
          (push p items))))
    (remove-duplicates-sorting
     (cons proj items)
     #'eproj-project/root=
     #'eproj-project/root<)))

;; If PATH is existing absoute file then return it, otherwise try to check
;; whether it's existing file relative to DIR and return that. Report error if
;; both conditions don't hold.
(defun-caching eproj-resolve-abs-or-rel-name (path dir) eproj-resolve-abs-or-rel-name/reset-cache (path dir)
  (resolve-obs-or-rel-filename path dir))

(defun-caching eproj-normalize-file-name (path) eproj-normalize-file-name/reset-cache (path)
  (strip-trailing-slash (normalize-file-name (expand-file-name path))))

(defun eproj--get-buffer-directory (buffer)
  "Get directory associated with BUFFER, either throug visited file
or `default-directory', if no file is visited."
  (with-current-buffer buffer
    (or (when buffer-file-truename
          (file-name-directory buffer-file-truename))
        default-directory)))

(defun eproj-get-matching-tags (proj tag-major-mode identifier search-with-regexp?)
  "Get all tags from PROJ and its related projects from mode TAG-MAJOR-MODE
whose name equals IDENTIFIER or matches regexp IDENTIFIER if SEARCH-WITH-REGEXP?
is non-nil.

Returns list of (tag . project) pairs."
  (-mapcat (lambda (proj)
             (aif (rest-safe
                   (assq tag-major-mode
                         (eproj-project/tags proj)))
                 (-map (lambda (tag)
                         (cons tag proj))
                       (if search-with-regexp?
                           (apply
                            #'-concat
                            (hash-table-entries-matching-re it identifier))
                         (gethash identifier it nil)))
               nil))
           (eproj-get-all-related-projects proj tag-major-mode)))


(autoload 'eproj/create-haskell-tags "eproj-haskell" nil nil)
(autoload 'eproj/haskell-tag->string "eproj-haskell" nil nil)

(autoload 'eproj/run-ctags-on-files "eproj-ctags")
(autoload 'eproj/run-ctags-on-files "eproj-ctags")

(autoload 'eproj-symbnav/describe "eproj-symbnav" nil t)
(autoload 'eproj-symbnav/reset "eproj-symbnav" nil t)
(autoload 'eproj-symbnav/go-to-symbol-home "eproj-symbnav" nil t)
(autoload 'eproj-symbnav/go-back "eproj-symbnav" nil t)
(autoload 'setup-eproj-symbnav "eproj-symbnav" nil nil)

;;; epilogue

(provide 'eproj)

;; Local Variables:
;; End:

;; eproj.el ends here
