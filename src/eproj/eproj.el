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
;;
;; These are different from ‘extra-navigation-files’: these are included in all files
;; and return together with e.g. *.hs and *.c files.
;; [(aux-files
;;   [(tree <tree-root> <glob>*)])]
;; [(ignored-files <glob>+)] - ignored filenames, <glob>
;;                             should match absolute file names. Will be applied
;;                             to file-list and aux files arguments.
;;                             Can use ${eproj-root} variable which points to
;;                             the directory of the .eproj-info file, without
;;                             trailing slash.
;; [(file-list <abs-or-rel-file>)] - filename listing all files on on each line
;; [(extra-navigation-files <glob>+)] - more files to include into navigation via `eproj-switch-to-file-or-buffer'.
;;
;; [(create-tag-files <t-or-nil>)] - whether to cache tags in tag files for this project
;;
;; Use this file as a source of tags instead of creating tags ourselves.
;; If specified together with 'create-tag-files then it will be created
;; automatically.
;; [(tag-file <abs-or-rel-file>)]
;;
;; [(language-specific
;;    [(haskell-mode
;;       [(indent-offset <integer>)])])]
;;
;; [(checker
;;    [(haskell-mode <nil|haskell-stack-ghc|haskell-ghc|haskell-dante|lsp|...>)])]
;;
;; [(disabled-checkers
;;    [(haskell-mode
;;        [<haskell-stack-ghc|haskell-ghc|haskell-dante|lsp|...>]
;;        [<haskell-stack-ghc|haskell-ghc|haskell-dante|lsp|...>]
;;        ...)])]
;;
;; Alternative to cabal’s /tmp/dist, etc.
;; [(build-dir <abs-or-rel-dir>)]
;;
;; Don’t use default projects for these modes
;; [(no-default-proj haskell-mode c-mode...)]
;;
;; If the same (up to a reasonable heuristic) tag is found in current
;; project and in some of the related ones, including the default
;; ones, then show the tag only from current project.
;; [(authoritative-tag-source-for haskell-mode c-mode...)]
;;
;; [...] - optional directive
;; <abs-or-rel-dir> - absolute or relative path to directory
;; <abs-or-rel-file> - absolute or relative path to file
;; <langs> - list of major-mode symbols
;; <proj-type> - symbol naming type of project, will be verified
;;               agains inferred project type
;; <tree-root> - absolute or relative to project root path to an existing directory
;; <glob>      - glob expression
;; <arg> - emacs strings, arguments to the command

(eval-when-compile
  (require 'cl)
  (require 'subr-x)
  (require 'macro-util))

(require 'eproj-customization)
;; Provide here to resolve load cycles.
(provide 'eproj)

(require 'common)
(require 'eproj-symbnav)
(require 'eproj-tag-index)
(require 'ivy)

(require 'cc-autoload)
(require 'haskell-autoload)
(require 'rust-autoloads)

;;; eproj languages

(cl-defstruct (eproj-language
               (:conc-name eproj-language/))
  mode
  ;; list of <ext> strings
  extensions
  extension-re
  ;; Nil or function of three arguments:
  ;; 1. current eproj/project structure
  ;; 2. function of zero arguments returning list of files to load from
  ;; 3. function of one argument - a buffer, that should parse tags from the
  ;; passed buffer.
  ;;
  ;; Returns whatever the third function returned.
  create-tags-procedure

  ;; Function of two arguments:
  ;; 1. project root, absolute file path pointing to a directory
  ;; 2. buffer with tag text to parse, created by e.g. create-tags-procedure.
  ;;
  ;; Should return `eproj-tag-index' structure, which is isomorphic to
  ;; hash table of tags - hashtable of (<identifier> . <eproj-tags>)
  ;; bindings for specified files, where <eproj-tags> is a list of tags.
  parse-tags-procedure

  ;; function that takes a tag and returns a string
  show-tag-kind-procedure
  ;; function of 3 arguments, a project, a string tag name and a tag struct, returning string
  tag->string-func
  ;; function of 2 arguments, a project and a tag struct, returning string or nil
  tag->signature-func
  ;; list of symbols, these modes will resolve to this language during
  ;; tag search
  synonym-modes

  ;; Possibly strip unneeded information before performing navigation
  normalise-identifier-before-navigation-procedure
  ;; List of strings - globs for files to consider during quick navigation.
  extra-navigation-globs)

(cl-defun mk-eproj-lang (&key mode
                              extensions
                              create-tags-procedure
                              parse-tags-procedure
                              show-tag-kind-procedure
                              tag->string-func
                              tag->signature-func
                              synonym-modes
                              normalise-identifier-before-navigation-procedure
                              extra-navigation-globs)
  (declare (pure t) (side-effect-free t))
  (cl-assert (symbolp mode))
  (cl-assert (listp extensions))
  (cl-assert (-all? #'stringp extensions))
  (cl-assert (or (null create-tags-procedure)
                 (functionp create-tags-procedure)
                 (autoloadp create-tags-procedure)
                 (subr-native-elisp-p create-tags-procedure))
             nil
             "invalid create-tags-procedure: %s"
             #'create-tags-procedure)
  (cl-assert (or (null parse-tags-procedure)
                 (functionp parse-tags-procedure)
                 (autoloadp parse-tags-procedure)
                 (subr-native-elisp-p parse-tags-procedure)))
  (cl-assert (listp synonym-modes))
  (cl-assert (or (functionp tag->string-func)
                 (autoloadp tag->string-func)
                 (subr-native-elisp-p tag->string-func))
             nil
             "Invalid tag->string-func: %s"
             tag->string-func)
  (cl-assert (or (functionp tag->signature-func)
                 (autoload-p tag->signature-func)
                 (subr-native-elisp-p tag->signature-func))
             nil
             "Invalid tag->signature-func: %s"
             tag->signature-func)
  (cl-assert (listp synonym-modes))
  (cl-assert (-all? #'symbolp synonym-modes))
  (cl-assert (or (null normalise-identifier-before-navigation-procedure)
                 (functionp normalise-identifier-before-navigation-procedure)
                 (autoloadp normalise-identifier-before-navigation-procedure)
                 (subr-native-elisp-p normalise-identifier-before-navigation-procedure)))
  (cl-assert (or (null extra-navigation-globs)
                 (-all? #'stringp extra-navigation-globs)))
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
   :tag->signature-func tag->signature-func
   :synonym-modes synonym-modes
   :normalise-identifier-before-navigation-procedure
   normalise-identifier-before-navigation-procedure
   :extra-navigation-globs
   extra-navigation-globs))

;;;; language definitions

(defun eproj/load-ctags-project (lang-mode proj project-files-thunk parse-tags-proc)
  (with-temp-buffer
    (eproj/run-ctags-on-files lang-mode
                              (eproj-project/root proj)
                              (eproj-thunk-get-value project-files-thunk)
                              (current-buffer))
    (prog1 (funcall parse-tags-proc (eproj-project/root proj) (current-buffer))
      (erase-buffer))))

(defvar eproj/languages
  (list
   (mk-eproj-lang
    :mode 'haskell-mode
    :extensions +haskell-extensions+
    :create-tags-procedure
    #'eproj/create-haskell-tags
    :parse-tags-procedure
    #'eproj/get-fast-tags-tags-from-buffer
    :show-tag-kind-procedure #'eproj/haskell-tag-kind
    :tag->string-func #'eproj/haskell-tag->string
    :tag->signature-func #'eproj/haskell-extract-tag-signature
    :synonym-modes '(haskell-literate-mode
                     haskell-c-mode
                     haskell-hsc-mode
                     haskell-c2hs-mode
                     alex-mode
                     happy-mode
                     uuag-mode)
    :normalise-identifier-before-navigation-procedure
    #'haskell-remove-module-qualification
    :extra-navigation-globs
    (eval-when-compile
      (append '("*.cabal"
                "cabal.project"
                "cabal.project.*"
                "cabal.project.local"
                "cabal.project.*.local"
                "package.yaml"
                "stack*.yaml")
              (--map (concat "*." it) +cpp-extensions+))))
   (mk-eproj-lang
    :mode 'rust-mode
    :extensions +rust-extensions+
    :create-tags-procedure
    (lambda (proj project-files-thunk parse-tags-proc)
      (eproj/load-ctags-project 'rust-mode proj project-files-thunk parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/rust-tag-kind
    :tag->string-func #'eproj/rust-tag->string
    :tag->signature-func #'eproj/extract-tag-line
    :extra-navigation-globs
    (eval-when-compile
      (--map (concat "*." it)
             (cons "toml"
                   +cpp-extensions+))))
   (mk-eproj-lang
    :mode 'c-mode
    :extensions +c-extensions+
    :create-tags-procedure
    (lambda (proj project-files-thunk parse-tags-proc)
      (eproj/load-ctags-project 'c-mode proj project-files-thunk parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/c-tag-kind
    :tag->string-func #'eproj/c-tag->string
    :tag->signature-func #'eproj/extract-tag-line)
   (mk-eproj-lang
    :mode 'c++-mode
    :extensions +cpp-extensions+
    :create-tags-procedure
    (lambda (proj project-files-thunk parse-tags-proc)
      (eproj/load-ctags-project 'c++-mode proj project-files-thunk parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/c-tag-kind
    :tag->string-func #'eproj/c-tag->string
    :tag->signature-func #'eproj/extract-tag-line)
   (mk-eproj-lang
    :mode 'python-mode
    :extensions '("py" "pyx" "pxd" "pxi")
    :create-tags-procedure
    (lambda (proj project-files-thunk parse-tags-proc)
      (eproj/load-ctags-project 'python-mode proj project-files-thunk parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/generic-tag-kind
    :tag->string-func #'eproj/generic-tag->string
    :tag->signature-func #'eproj/extract-tag-line)
   (mk-eproj-lang
    :mode 'clojure-mode
    :extensions '("clj" "java")
    :create-tags-procedure
    (lambda (proj project-files-thunk parse-tags-proc)
      ;; Use Java mode for now since there's no dedicated Clojure config yet.
      (eproj/load-ctags-project 'java-mode proj project-files-thunk parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/generic-tag-kind
    :tag->string-func #'eproj/generic-tag->string
    :tag->signature-func #'eproj/extract-tag-line)
   (mk-eproj-lang
    :mode 'java-mode
    :extensions '("java")
    :create-tags-procedure
    (lambda (proj project-files-thunk parse-tags-proc)
      (eproj/load-ctags-project 'java-mode proj project-files-thunk parse-tags-proc))
    :parse-tags-procedure
    #'eproj/ctags-get-tags-from-buffer
    :show-tag-kind-procedure #'eproj/java-tag-kind
    :tag->string-func #'eproj/java-tag->string
    :tag->signature-func #'eproj/extract-tag-line)
   (mk-eproj-lang
    :mode 'emacs-lisp-mode
    :extensions '("el")
    :create-tags-procedure nil
    :parse-tags-procedure nil
    :show-tag-kind-procedure #'eproj/generic-tag-kind
    :tag->string-func #'eproj/generic-tag->string
    :tag->signature-func #'eproj/extract-tag-line)
   (mk-eproj-lang
    :mode 'nix-mode
    :extensions '("nix")
    :create-tags-procedure nil
    :parse-tags-procedure nil
    :show-tag-kind-procedure #'eproj/generic-tag-kind
    :tag->string-func #'eproj/generic-tag->string
    :tag->signature-func #'eproj/extract-tag-line)))

(defvar eproj/languages-table
  (let ((table (make-hash-table :test #'eq)))
    (dolist (lang eproj/languages)
      (puthash (eproj-language/mode lang) lang table))
    table))

(defvar eproj/synonym-modes-table
  (let ((table (make-hash-table :test #'eq)))
    (dolist (lang eproj/languages)
      (dolist (synonym (eproj-language/synonym-modes lang))
        (puthash synonym (eproj-language/mode lang) table)))
    table)
  "Used by eproj-symbnav facility.")

;;;###autoload
(defun eproj/resolve-synonym-modes (mode)
  "Replace modes that are similar to some other known modes.

e.g. resolves synonyms so that literate haskell mode & others will
get proper flycheck checker."
  (gethash mode eproj/synonym-modes-table mode))

;;; eproj-project

;;;; projects themselves

;; Thunk below is a function of 0 arguments.
(cl-defstruct (eproj-project
               (:conc-name eproj-project/))
  ;; Normalized directory name, without trailing slash.
  (root                  nil :read-only t)
  ;; alist of (<symbol> . <symbol-dependent-info>) entries for eproj-query.el
  (aux-info              nil :read-only t)

  ;; List of aux-files-entries:
  ;; (tree <tree-root> <glob>*)
  (aux-files-entries     nil :read-only t)

  ;; Thunk of list of (language-major-mode . <eproj-tag-index>);
  ;; <eproj-tag-index> - datastructure mapping 'symbol-str's to 'eproj-tag's. See `eproj-tag-index.el'.
  (tags                  nil)
  ;; list of other project roots
  (related-projects      nil :read-only t)
  ;; list of symbols - major-modes for related languages
  (languages             nil :read-only t)
  ;; Stores list of filenames, if specified in aux-info via 'file-list
  (cached-file-list      nil)
  ;; List of absolute filename globs to ignore in current project.
  (ignored-files-globs   nil :read-only t)
  ;; list of files, if specified in aux-info via 'file-list
  (file-list-filename    nil :read-only t)
  ;; boolean, whether to cache tags for this project in files
  (create-tag-files      nil :read-only t)
  ;; string, provided path to the tags file
  (tag-file              nil :read-only t)
  ;; list of symbols, may be empty
  (no-default-project-for nil :read-only t)

  ;; List of glob strings to include into navigation lists
  (extra-navigation-globs nil :read-only t)

  ;; Hash table mapping absolute file paths this project manages to
  ;; the same paths relative to project's root. May not be 100%
  ;; accurate (meaning that it may contain files that are no longer
  ;; present on disk) and should be used only for user navigation.
  ;; This field is initialised lazily when file list is first
  ;; constructed or user does a search.
  (cached-files-for-navigation nil)
  ;; Similar to ‘cached-files-for-navigation’ field but get’s populated
  ;; with project’s files that are saved. This way new files may end
  ;; up here whereas ‘cached-files-for-navigation’ could contain
  ;; stale snapshot of navigation files taken previously.
  (transient-files-for-navigation nil)

  ;; Regexp that matches any files that should be ignored
  (cached-ignored-files-re nil :read-only t)

  ;; List of symbols - mode names.
  (authoritative-tag-source-for nil :read-only t))

(defmacro eproj-project/query-aux-info-entry (aux-info &rest keys)
  "Retrieve aux-data assoc entry associated with a KEY in the aux info AUX-INFO."
  (declare (indent 1))
  `(let ((entry ,(foldl (lambda (acc key)
                          `(assq ,key ,acc))
                        aux-info
                        keys)))
     (cl-assert
      (or (null? entry)
          (eq 'languages (car-safe entry)) ;; 'languages entry can have any length
          (= (length entry) 2))
      nil
      "Invalid entry in .eproj-info: %s"
      entry)
     (cdr-safe entry)))

(defmacro eproj-project/query-aux-info (aux-info &rest keys)
  "Retrieve aux-data value associated with a KEY in the aux info AUX-INFO."
  (declare (indent 1))
  `(car-safe (eproj-project/query-aux-info-entry ,aux-info ,@keys)))

(defun eproj-project/root= (proj-a proj-b)
  (string= (eproj-project/root proj-a)
           (eproj-project/root proj-b)))

(defun eproj-project/root< (proj-a proj-b)
  (string< (eproj-project/root proj-a)
           (eproj-project/root proj-b)))

(defvar *eproj-projects*
  (make-hash-table :test #'equal)
  "Hash table mapping project roots to projects.")

(defun eproj-reset-projects ()
  "Clear project database `*eproj-projects*'."
  (interactive)
  (clrhash *eproj-projects*)
  (eproj-get-initial-project-root/reset-cache)
  (eproj--resolve-to-abs-path-cached/reset-cache)
  (eproj-normalise-file-name-expand-cached/reset-cache)
  ;; do not forget to reset cache
  (eproj/reset-buffer-local-cache)
  (when (fboundp #'flycheck-haskell-clear-config-cache)
    (flycheck-haskell-clear-config-cache))
  (garbage-collect))

(defun eproj-update-projects ()
  "Update projects in database `*eproj-projects*'."
  (interactive)
  (let ((roots (hash-table-keys *eproj-projects*)))
    (clrhash *eproj-projects*)
    (dolist (root roots)
      (eproj--make-project-and-register! root))))

(defun eproj-update-buffer-project ()
  "Re-create project for current buffer."
  (interactive)
  (eproj--make-project-and-register!
   (eproj-project/root (eproj-get-project-for-buf (current-buffer))))
  (notify "done"))

;; careful: quite complex procedure
;;;###autoload
(defun eproj-update-current-buffer-within-its-project! ()
  "Update tags only for current buffer in project that contains it."
  (interactive)
  (let ((proj (eproj-get-project-for-buf (current-buffer)))
        (eproj-verbose-tag-loading nil))
    (when proj
      (let* ((fname (expand-file-name buffer-file-name))
             (mode (eproj/resolve-synonym-modes major-mode)))
        (unless (memq mode
                      (eproj-project/languages proj))
          (error "Project %s does not manage %s files"
                 (eproj-project/root proj)
                 mode))
        (unless (eproj-project/transient-files-for-navigation proj)
          (setf (eproj-project/transient-files-for-navigation proj)
                (make-hash-table :test #'equal)))
        (eproj--add-cached-file-for-navigation
         (eproj-project/root proj)
         fname
         (eproj-project/transient-files-for-navigation proj))
        (let ((tags-thunk (eproj-project/tags proj)))
          (cl-assert (not (null tags-thunk)) nil
                     "Got nil tags thunk for project %s"
                     (eproj-project/root proj))
          ;; If tags are still a thunk (i.e. value is *not* ready yet) then
          ;; we should not do anything here - tags will emerge once thunk
          ;; will become forced.
          (when (eproj-thunk/value-ready? tags-thunk)
            (if-let (old-tags (cdr-safe (assq mode (eproj-thunk/value tags-thunk))))
                (let ((new-tags
                       (eproj/load-tags-for-mode
                        proj
                        mode
                        (eproj-make-evaluated-thunk (list fname))
                        ;; Ignore tag files since we want to reload tags for a
                        ;; single file and collect tags exactly in the file, not
                        ;; the cached ones!
                        :consider-tag-files nil)))
                  (eproj-tag-index-drop-tags-from-file! fname
                                                        (eproj-project/root proj)
                                                        old-tags)
                  (eproj-tag-index-merge!
                   old-tags
                   new-tags))
              (error "Project '%s' does not have tags for '%s'"
                     (eproj-project/root proj)
                     mode))))))))

(defun eproj/tag-file-name (proj mode)
  "Return absolute path for tag file for project PROJ and language mode MODE to
cache tags in."
  (concat +tmp-global-path+
          "/tags-"
          (sha1 (eproj-project/root proj))
          "-"
          (format "%s" mode)))

(cl-defun eproj/load-tags-for-mode (proj mode project-files-thunk &key (consider-tag-files t))
  (if-let ((lang (gethash mode eproj/languages-table)))
      (if-let ((create-tags-procedure (eproj-language/create-tags-procedure lang)))
          (if-let ((parse-tags-procedure (eproj-language/parse-tags-procedure lang)))
              (if consider-tag-files
                  (cond
                    ((eproj-project/create-tag-files proj)
                     (let ((tag-file (or (eproj-project/tag-file proj)
                                         (eproj/tag-file-name proj mode))))
                       (if (file-exists-p tag-file)
                           (with-temp-buffer
                             (insert-file-contents-literally tag-file)
                             (funcall parse-tags-procedure (eproj-project/root proj) (current-buffer)))
                         (funcall create-tags-procedure
                                  proj
                                  project-files-thunk
                                  (lambda (proj-root buf)
                                    (with-current-buffer buf
                                      (write-region (point-min) (point-max) tag-file)
                                      (funcall parse-tags-procedure proj-root buf)))))))
                    ((eproj-project/tag-file proj)
                     (let ((tag-file (or (eproj-project/tag-file proj)
                                         (eproj/tag-file-name proj mode))))
                       (if (file-exists-p tag-file)
                           (with-temp-buffer
                             (insert-file-contents-literally tag-file)
                             (funcall parse-tags-procedure (eproj-project/root proj) (current-buffer)))
                         (error "The specified tag file does not exist and create-tag-files was not specified in the .eproj-info: %s"
                                tag-file))))
                    (t
                     (funcall create-tags-procedure
                              proj
                              project-files-thunk
                              parse-tags-procedure)))
                (funcall create-tags-procedure
                         proj
                         project-files-thunk
                         parse-tags-procedure))
            (error "Failed to load tags for mode '%s': language spec has no function to parse tags" mode))
        (error "Failed to load tags for mode '%s': language spec has no function to load tags" mode))
    (error "Failed to load tags for mode '%s': cannot resolve language" mode)))

(defun eproj--get-tags (proj)
  "Get tags for project PROJ."
  (eproj-thunk-get-value (eproj-project/tags proj)))

(cl-defstruct (eproj-thunk
               (:conc-name eproj-thunk/))
  value
  value-ready?
  computation)

(defun eproj-thunk-get-value (thunk)
  (unless (eproj-thunk/value-ready? thunk)
    (setf (eproj-thunk/value thunk) (funcall (eproj-thunk/computation thunk))
          (eproj-thunk/value-ready? thunk) t
          ;; Free the closure.
          (eproj-thunk/computation thunk) nil))
  (eproj-thunk/value thunk))

(defun eproj-make-evaluated-thunk (value)
  (make-eproj-thunk
   :value value
   :value-ready? t
   :computation nil))

(defmacro eproj--make-thunk (&rest body)
  (let ((uninitialised-value '#:uninitialised))
    `(make-eproj-thunk
      :value ',uninitialised-value
      :value-ready? nil
      :computation (lambda ()
                     ,@body))))

(defun eproj--prepare-to-load-fresh-tags-lazily-on-demand! (proj)
  "Reload tags for PROJ."
  (let* ((project-files-thunk
          (eproj--make-thunk
           (eproj--get-all-files proj))))
    (setf (eproj-project/tags proj)
          (eproj--make-thunk
           (-map (lambda (lang-mode)
                   (let ((new-tags (eproj/load-tags-for-mode proj
                                                             lang-mode
                                                             project-files-thunk
                                                             :consider-tag-files t)))
                     (cl-assert (eproj-tag-index-p new-tags))
                     (when (= 0 (eproj-tag-index-size new-tags))
                       (error "Warning while reloading: project %s loaded no tags for language %s"
                              (eproj-project/root proj)
                              lang-mode))
                     (cons lang-mode new-tags)))
                 (eproj-project/languages proj)))))
  nil)

(defun eproj--get-eproj-info-from-dir (root)
  "Get filename of .eproj-info file from directory DIR if it exists, else return nil."
  (let ((path (concat root "/.eproj-info")))
    (when (file-exists-p path)
      path)))

(defconst eproj--known-eproj-info-entries
  '(languages
    related
    aux-files
    ignored-files
    file-list
    extra-navigation-files
    create-tag-files
    tag-file
    language-specific
    checker
    disabled-checkers
    build-dir)
  "List .eproj-info keys that are currently supported.")

(defun eproj-read-eproj-info-file (root filename)
  "Read .eproj-info file from FILENAME."
  (unless filename
    (error ".eproj-info file does not exist at: %s" root))
  (unless (file-exists-p filename)
    (error ".eproj-info file does not exist: %s" filename))
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (goto-char (point-min))
    (let ((info (read (current-buffer))))
      (cl-assert (listp info) nil "Expected eproj info to be a list: %s" nifo)
      (cl-assert (--every? (memq (car it) eproj--known-eproj-info-entries)
                           info)
                 nil
                 "Some entries in .eproj-info are not supported:\n%s"
                 (pp-to-string
                  (--filter (not (memq (car it) eproj--known-eproj-info-entries))
                            info)))
      info)))

;;;; project creation

(defun eproj--make-project-and-register! (root)
  "Create fresh project for ROOT directory and register it within
`*eproj-projects*'."
  (cl-assert (file-directory-p root))
  (puthash root
           (eproj-make-project root (eproj--get-info-from-root root t))
           *eproj-projects*))

(defun eproj-make-project (root aux-info)
  "Parse associative list AUX-INFO and construct `eproj-project' structure
for project at ROOT directory."
  (cl-assert (stringp root)
             nil
             "Project root must be a string: %s" root)
  (unless (and (file-exists-p root)
               (file-directory-p root))
    (error "Invalid project root, directory must exist: %s" root))
  (let* ((languages (aif (eproj-project/query-aux-info-entry aux-info 'languages)
                        it
                      (progn
                        (notify "warning: no languages defined for project %s"
                                root)
                        nil)))
         (ignored-files-globs
          (eproj--get-ignored-files root aux-info))
         (file-list-filename
          (awhen (eproj-project/query-aux-info aux-info 'file-list)
            (let ((fname (eproj--resolve-to-abs-path-cached it root)))
              (when (or (null fname)
                        (not (file-exists-p fname)))
                (error "File list filename does not exist: %s" fname))
              fname)))
         (create-tag-files
          (eproj-project/query-aux-info aux-info 'create-tag-files))
         (tag-file
          (eproj-project/query-aux-info aux-info 'tag-file))
         (extra-navigation-globs
          (eproj-project/query-aux-info-entry aux-info 'extra-navigation-files))

         (related-projects
          (eproj-get-related-projects root aux-info))
         (cached-ignored-files-re
          (let ((related-projs-globs
                 (--map (concat it "/*") related-projects)))
            (globs-to-regexp (append ignored-files-globs related-projs-globs))))
         (no-default-project-for (cdr-safe (assq 'no-default-proj aux-info)))

         (authoritative-tag-source-for (cdr-safe (assq 'authoritative-tag-source-for aux-info))))
    (cl-assert (sequencep languages) nil "Project languages is not a sequence: %s" languages)
    (cl-assert (listp extra-navigation-globs))
    (cl-assert (-all? #'stringp extra-navigation-globs))
    (cl-assert (-all? #'symbolp no-default-project-for))
    (let ((proj
           (make-eproj-project :root root
                               :aux-info aux-info
                               :aux-files-entries (cdr-safe (assq 'aux-files aux-info))
                               :tags nil
                               :related-projects related-projects
                               :languages languages
                               :cached-file-list nil
                               :ignored-files-globs ignored-files-globs
                               :file-list-filename file-list-filename
                               :create-tag-files create-tag-files
                               :tag-file (awhen tag-file
                                           (eproj--resolve-to-abs-path-cached it root))
                               :no-default-project-for no-default-project-for
                               :extra-navigation-globs extra-navigation-globs
                               :cached-files-for-navigation nil
                               :transient-files-for-navigation nil
                               :cached-ignored-files-re cached-ignored-files-re
                               :authoritative-tag-source-for authoritative-tag-source-for)))
      (eproj--prepare-to-load-fresh-tags-lazily-on-demand! proj)
      proj)))

;;;; description

(defun eproj-describe-all-projects ()
  (interactive)
  (let ((buf (get-buffer-create "*eproj projects*")))
    (switch-to-buffer-other-window buf)
    (with-current-buffer buf
      (erase-buffer)
      (text-mode)
      (let ((projs (sort (hash-table-values *eproj-projects*)
                         (lambda (a b)
                           (string< (eproj-project/root a)
                                    (eproj-project/root b))))))
        (mapc (lambda (proj)
                (condition-case err
                    (eproj-descibe-proj buf proj nil t)
                  (error
                   (insert (format "Error while describing project: %s\n" (cdr err)))))
                (insert-char ?\- 80)
                (insert-char ?\n))
              projs))
      (goto-char (point-min)))))

(defun eproj-describe-buffer-project (&optional omit-tags)
  (interactive "P")
  (if-let (proj (eproj-get-project-for-buf (current-buffer)))
      (let ((buf (get-buffer-create (format "*%s description*" (eproj-project/root proj)))))
        (switch-to-buffer-other-window buf)
        (with-current-buffer buf
          (erase-buffer)
          (text-mode)
          (eproj-descibe-proj buf proj (not omit-tags) nil)
          (goto-char (point-min))))
    (error "no project for buffer %s" (buffer-name (current-buffer)))))

(defun eproj-descibe-proj (buf proj &optional describe-tags describe-buffers)
  "Insert description of PROJ in current buffer BUF."
  (let ((indent "  "))
    (with-current-buffer buf
      (insert "root: " (eproj-project/root proj) "\n")
      (insert (format "languages: %s\n" (eproj-project/languages proj)))
      (insert "related projects:\n")
      (dolist (related-proj (eproj-project/related-projects proj))
        (insert indent related-proj "\n"))
      (insert (format "aux info: %s\n" (eproj-project/aux-info proj)))
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
      (when describe-tags
        (insert "number of tags loaded: "
                (let ((tag-count 0))
                  (dolist (tags-entry (eproj--get-tags proj))
                    (dolist (entry (eproj-tag-index-entries (cdr tags-entry)))
                      (setf tag-count
                            (+ tag-count (length (cdr entry))))))
                  (number->string tag-count))
                "\n"))
      (insert "\nnavigation globs: "
              (mapconcat #'identity (eproj--navigation-globs proj) " "))
      (insert "\n")

      (insert "\nnavigation files:\n")
      (eproj-with-all-project-files-for-navigation proj
                                                   (lambda (_abs-path rel-path)
                                                     (insert rel-path "\n")))
      (insert "\n")
      (when describe-tags
        (insert "tags:\n")
        (dolist (tags-entry (eproj--get-tags proj))
          (let ((lang-tags (-sort (lambda (a b) (string< (car a) (car b)))
                                  (eproj-tag-index-entries (cdr tags-entry)))))
            (insert "lang: "
                    (pp-to-string (car tags-entry))
                    ", total amount = "
                    (number->string (length lang-tags))
                    "\n")
            (dolist (entry lang-tags)
              (insert indent (pp-to-string (car entry)) "\n")
              (dolist (subentry (cdr entry))
                (insert indent indent
                        (format "%s:%s\n"
                                (file-relative-name
                                 (expand-file-name
                                  (eproj-resolve-to-abs-path (eproj-tag/file subentry) proj))
                                 (expand-file-name (eproj-project/root proj)))
                                (eproj-tag/line subentry)))))))))))

;;;; utilities

(defmacro eproj/evaluate-with-caching-buffer-local-var (value-expr
                                                        buffer-expr
                                                        caching-var
                                                        value-predicate)
  (let* ((buffer-var '#:buffer)
         (is-nil '#:is-nil)
         (is-nil-value `(quote ,is-nil)))
    `(let ((,buffer-var ,buffer-expr))
       (with-current-buffer ,buffer-var
         (when (or (null ,caching-var)
                   (eq ,caching-var ,is-nil-value))
           (setf ,caching-var (or ,value-expr ,is-nil-value)))
         (unless (eq ,caching-var ,is-nil-value)
           (cl-assert (funcall ,value-predicate ,caching-var)
                      nil
                      (format
                       ,(format "Variable `%s' must contain value that satisfies predicate %s. Value: %%s"
                                caching-var
                                value-predicate)
                       ,caching-var))
           ,caching-var)))))

(defun eproj/reset-buffer-local-cache ()
  "Reset all caching buffer-local values associated with eproj in all buffers"
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (kill-local-variable 'eproj/buffer-initial-project-root-cache)
      (kill-local-variable 'eproj/buffer-directory))))

(defvar-local eproj/buffer-initial-project-root-cache nil
  "Is set to initial project root (i.e. string) for buffer containing this
variable or symbol 'unresolved.")

(defun-caching eproj-get-initial-project-root (path) eproj-get-initial-project-root/reset-cache path
  "Find closest directory parent of PATH that contains .eproj-info file, cabal.project file, or .git directory."
  (cl-assert (stringp path))
  (let* ((is-directory? (file-directory-p path))
         (path-dir (if is-directory?
                       path
                     (file-name-directory path)))
         (exists? (if is-directory?
                      t
                    (file-directory-p path-dir))))
    (awhen (and exists?
                (or (locate-dominating-file path-dir ".eproj-info")
                    (locate-dominating-file path-dir
                                            (lambda (dir)
                                              (directory-files dir
                                                               nil ;; absolute names
                                                               (rx bos
                                                                   (or ".git"
                                                                       (seq "cabal.project"
                                                                            (? "." (* any))
                                                                            (? ".local")))
                                                                   eos)
                                                               t ;; nosort
                                                               )))))
      (eproj-normalise-file-name-expand-cached it))))

(defvar eproj--inferrable-project-infos
  (list #'eproj--infer-haskell-project
        #'eproj--infer-rust-project))

(defun eproj--infer-rust-project (root)
  (when (file-exists-p (concat root "/Cargo.toml"))
    '((languages 'rust-mode))))

(defun eproj--get-info-from-root (root strict)
  "Either read existing .eproj-info file ot try to make up its contents if we can."
  (if-let (eproj-info-file (eproj--get-eproj-info-from-dir root))
      (eproj-read-eproj-info-file root eproj-info-file)
    (let ((fs eproj--inferrable-project-infos)
          (result nil))
      (while (and (not result)
                  fs)
        (setf result (funcall (car fs) root)
              fs (cdr fs)))
      (when (and strict
                 (not result))
        (error "Filed to infer project info for root %s" root))
      result)))

(defvar-local eproj/buffer-directory nil
  "Caches value computed by `eproj--get-buffer-directory'.

Set to project that corresponds to buffer containing this variable or
symbol 'unresolved.")

(defun eproj-get-project-for-buf (buffer)
  "Get project for BUFFER. Throw error if there's no project for it."
  (eproj-get-project-for-path (eproj--get-buffer-directory buffer)))

;;;###autoload
(defun eproj-sha1-of-project-root-for-buf (buffer)
  (awhen (eproj-get-project-for-buf-lax buffer)
    (sha1 (eproj-project/root it))))

;;;###autoload
(defun eproj-get-project-for-buf-lax (buffer)
  "Get project for BUFFER. Return nil if there's no project for it."
  (eproj-get-project-for-path-lax (eproj--get-buffer-directory buffer)))

;;;###autoload
(defun eproj-get-project-for-path-lax (path)
  "Retrieve project that contains PATH as its part. Similar to
`eproj-get-project-for-path' but returns nil if there's no
project for PATH."
  (if-let (proj (gethash path *eproj-projects* nil))
      proj
    (if-let (proj-root (eproj-get-initial-project-root path))
        (if-let (proj (gethash proj-root *eproj-projects* nil))
            proj
          (if-let ((info (eproj--get-info-from-root proj-root nil)))
              (let ((proj (eproj-make-project proj-root info)))
                (puthash (eproj-project/root proj)
                         proj
                         *eproj-projects*)
                proj)
            nil))
      nil)))

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
  (if-let (proj (gethash path *eproj-projects* nil))
      proj
    (if-let (proj-root (eproj-get-initial-project-root path))
        (if-let (proj (gethash proj-root *eproj-projects* nil))
            proj
          (eproj--make-project-and-register! proj-root))
      (error "Could not infer eproj project when looking from %s directory (no .eproj-info file, cabal.project file or .git directory found)"
             path))))

(defun eproj--filter-ignored-files-from-file-list (proj files)
  "Filter list of FILES using ignored-files-globs of project PROJ."
  (if-let ((regexp (eproj-project/cached-ignored-files-re proj)))
      (cl-delete-if (lambda (x) (string-match-p regexp x)) files)
    files))

(defun eproj-with-all-project-files-for-navigation (proj func)
  "Preapre all files related to project PROJ that user might want to quickly
jump to. Call FUNC with be called with 2 arguments: absolute path and path relative to the
current project’s root."
  (unless (eproj-project/cached-files-for-navigation proj)
    (notify "Constructing file navigation list for %s"
            (eproj-project/root proj))
    (eproj--get-all-files proj))
  (maphash func (eproj-project/cached-files-for-navigation proj))
  (awhen (eproj-project/transient-files-for-navigation proj)
    (maphash func it)))

(defun eproj--navigation-globs (proj)
  "Get globs for files to consider during quick navigation."
  (let ((globs
         (append (eproj-project/extra-navigation-globs proj)
                 '("*.org" "*.md" "*.markdown" "*.rst" "*.json" "*.sh" "*.mk" "*.txt" "*.yaml" "*.xml" "*.nix" "makefile*" "Makefile*" "*.inc" "*.spec" "README" "ChangeLog*" "Changelog*" "*.nix"))))
    (dolist (mode (eproj-project/languages proj))
      (let ((lang (gethash (eproj/resolve-synonym-modes mode)
                           eproj/languages-table)))
        (unless lang
          (error "Project %s specifies unrecognised language: %s" (eproj-project/root proj) mode))
        (setf globs
              (append (eproj-language/extra-navigation-globs lang)
                      globs))))
    globs))

(defun eproj--navigation-files (proj)
  "Get language-independent files that are useful to have when
doing `eproj-switch-to-file-or-buffer'."
  (let ((files
         (find-rec*
          :root (eproj-project/root proj)
          :globs-to-find (eproj--navigation-globs proj)
          :ignored-files-globs (eproj-project/ignored-files-globs proj)
          :ignored-absolute-dirs (eproj-project/related-projects proj)
          :ignored-directories +ignored-directories+
          :ignored-directory-prefixes +ignored-directory-prefixes+)))
    (eproj--filter-ignored-files-from-file-list proj files)))

(defun eproj--get-all-files (proj)
  "Get all files that are managed by a project PROJ."
  (let* ((files (eproj-get-project-files proj))
         (aux (eproj-project/aux-files proj))
         (all-files (nconc aux files))
         (extra (eproj--navigation-files proj))
         (files-cache (eproj-project/cached-files-for-navigation proj))
         (proj-root (eproj-project/root proj)))
    ;; Cache files
    (if files-cache
        (clrhash files-cache)
      (setf (eproj-project/cached-files-for-navigation proj)
            (setf files-cache (make-hash-table :test #'equal))))
    (dolist (file all-files)
      (eproj--add-cached-file-for-navigation proj-root file files-cache))
    (dolist (file extra)
      (eproj--add-cached-file-for-navigation proj-root file files-cache))
    all-files))

(defun eproj--read-file-list (file)
  (let* ((res (cons nil nil))
         (tmp res))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((path (trim-whitespace
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))))
          (setf tmp (setcdr-sure tmp (cons path nil))))
        (forward-line 1)))
    (cdr res)))

(defun eproj-get-project-files (proj)
  "Retrieve project files for PROJ depending on it's type. Returns absolute
paths."
  ;; Cached files are necessarily from file-list and intended for projects whose
  ;; list of files does not change and may be cached.
  (if-let (cached-files (eproj-project/cached-file-list proj))
      cached-files
    (let ((related-projects-roots (eproj-project/related-projects proj)))
      ;; If there's a file-list then read it and store to cache.
      (if-let (file-list-filename (eproj-project/file-list-filename proj))
          (let ((list-of-files (eproj--read-file-list file-list-filename)))
            (cl-assert (listp list-of-files))
            (let* ((absolute-files
                    (-map (lambda (filename)
                            (eproj-resolve-to-abs-path filename proj))
                          list-of-files))
                   (resolved-files
                    (eproj--filter-ignored-files-from-file-list proj absolute-files)))
              (cl-assert (--all? (and (stringp it)
                                      (file-exists-p it))
                                 resolved-files))
              (setf (eproj-project/cached-file-list proj) resolved-files)
              resolved-files))
        (when-let (globs
                   (-mapcat (lambda (lang)
                              (cl-assert (and lang (symbolp lang)) nil
                                         "Expected a symbol for language but got: %s"
                                         lang)
                              (aif (gethash lang eproj/languages-table)
                                  (--map (concat "*." it)
                                         (eproj-language/extensions it))
                                (error "Unknown language: %s" lang)))
                            (eproj-project/languages proj)))
          (find-rec*
           :root (eproj-project/root proj)
           :globs-to-find (-mapcat (lambda (lang)
                                     (cl-assert (and lang (symbolp lang)) nil
                                                "Expected a symbor for language but got: %s"
                                                lang)
                                     (aif (gethash lang eproj/languages-table)
                                         (--map (concat "*." it)
                                                (eproj-language/extensions it))
                                       (error "Unknown language: %s" lang)))
                                   (eproj-project/languages proj))
           :ignored-files-globs (eproj-project/ignored-files-globs proj)
           :ignored-absolute-dirs related-projects-roots
           :ignored-directories +ignored-directories+
           :ignored-directory-prefixes +ignored-directory-prefixes+))))))

(defun eproj-get-related-projects (root aux-info)
  "Return list of roots of related project for folder ROOT and AUX-INFO.
AUX-INFO is expected to be a list with entry (related { <abs-path> | <rel-path> }* ).
Returns nil if no relevant entry found in AUX-INFO."
  (awhen (cdr-safe (assq 'related aux-info))
    (-map (lambda (path)
            (cl-assert (stringp path) nil
                       "invalid entry under related clause, string expected %s"
                       path)
            (eproj--resolve-to-abs-path-cached path root))
          it)))

(defun eproj-project/aux-files (proj)
  "Get aux files, mainly for navigation."
  (when-let (aux-files-entry (eproj-project/aux-files-entries proj))
    (let ((project-root (eproj-project/root proj))
          (aux-trees (make-hash-table :test #'equal)))
      (with-temp-buffer
        (cd project-root)
        (mapc (lambda (entry) (eproj--interpret-aux-files-entry project-root entry aux-trees))
              aux-files-entry))

      (let ((result nil))
        (maphash (lambda (root globs)
                   (let ((raw-files (find-rec*
                                     :root root
                                     :globs-to-find globs
                                     :ignored-files-globs (eproj-project/ignored-files-globs proj)
                                     :ignored-absolute-dirs (eproj-project/related-projects proj)
                                     ;; Allow aux files to look into ignored paths if needed.
                                     :ignored-directories nil
                                     :ignored-directory-prefixes nil)))
                     (setf result
                           (nconc (eproj--filter-ignored-files-from-file-list proj raw-files)
                                  result))))
                 aux-trees)
        result))))

(defun eproj--interpret-aux-files-entry (project-root item aux-trees)
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
           (let ((resolved-tree-root
                  (eproj--resolve-to-abs-path-cached tree-root project-root)))
             (cl-assert (file-name-absolute-p resolved-tree-root)
                        nil
                        "Resolved aux tree root is not absolute: %s"
                        resolved-tree-root)
             (puthash resolved-tree-root
                      (append patterns
                              (gethash resolved-tree-root aux-trees nil))
                      aux-trees))))
        (t
         (error "Invalid 'aux-files entry: 'tree clause not found"))))

(defvar eproj/default-projects (make-hash-table :test #'eq)
  "Hash table mapping major mode symbols to lists of project roots, that should
be regarded as related projects when looking for tags in said major mode from any
project.")

(defun eproj--get-default-projects (proj mode)
  "Get default projects associated with MODE for PROJ."
  (cl-assert (symbolp mode))
  (cl-assert (eproj-project-p proj))
  (unless (memq mode (eproj-project/no-default-project-for proj))
    (gethash mode eproj/default-projects nil)))

(defun eproj-get-all-related-projects (proj)
  "Return transitive closure all projects realted to PROJ."
  (let ((all-related-default-projects
         (-mapcat (lambda (mode)
                    (-map #'eproj-get-project-for-path
                          (eproj--get-default-projects proj mode)))
                  (eproj-project/languages proj))))
    (eproj--transitive-closure-of-related-projects
     (cons proj all-related-default-projects))))

(defun eproj-get-all-related-projects-for-mode (proj mode)
  "Return eproj-project structures of projects realted to PROJ
throuct specific MAJOR-MODE including PROJ itself. MAJOR-MODE will add some default
projects into the mix."
  (cl-assert (eproj-project-p proj) nil
             "Not a eproj-project structure: %s" proj)
  (eproj--transitive-closure-of-related-projects
   (cons proj
         (-map #'eproj-get-project-for-path
               (eproj--get-default-projects proj mode)))))

(defun eproj--transitive-closure-of-related-projects (projs-to-close-over)
  (cl-assert (-all? #'eproj-project-p projs-to-close-over))
  (let ((visited (make-hash-table :test #'equal))
        (projs projs-to-close-over))
    (while projs
      (let* ((p (pop projs))
             (root (eproj-project/root p)))
        (unless (gethash root visited nil)
          (setf (gethash root visited) p)
          (setf projs (nconc (-map #'eproj-get-project-for-path
                                   (eproj-project/related-projects p))
                             projs)))))
    (hash-table-values visited)))

(defun eproj--get-ignored-files (root aux-info)
  (cl-assert (stringp root))
  (--map (replace-regexp-in-string "[$]{eproj-root}" root it)
         (cdr-safe (assq 'ignored-files aux-info))))

(defmacro eproj-resolve-to-abs-path (path proj)
  `(if (file-name-absolute-p ,path)
       ,path
     ,(if proj
          `(eproj--resolve-to-abs-path-cached ,path (eproj-project/root ,proj))
        `(error "Path is not absolute and no project available to resolve it: %s"
                ,path))))


(defvar eproj-translate-file-name nil
  "If set should be a function that amends paths according to some rule. E.g. when
running in WSL it could translate c: as /mnt/c, for example.")

(defun-caching eproj--resolve-to-abs-path-cached (path dir) eproj--resolve-to-abs-path-cached/reset-cache (cons path dir)
  "If PATH is existing absoute file then return it, otherwise try
to check whether it’s an existing file relative to DIR and return
that. Report error if both conditions don’t hold."
  (resolve-to-abs-path (if eproj-translate-file-name (funcall eproj-translate-file-name path) path)
                       dir))

(defun-caching-extended
  eproj-normalise-file-name-expand-cached (path &optional dir)
  eproj-normalise-file-name-expand-cached/with-explicit-cache
  eproj-normalise-file-name-expand-cached/make-cache
  eproj-normalise-file-name-expand-cached/reset-cache
  (cons path dir)
  (normalise-file-name (expand-file-name path dir)))

(defun eproj--get-buffer-directory (buf)
  "Get directory associated with BUFFER, either through visited file
or `default-directory', if no file is visited."
  (with-current-buffer buf
    (eproj/evaluate-with-caching-buffer-local-var
     ;; Take directory since the file visited by buf may not be
     ;; under version control per se.
     (or (and buffer-file-truename
              (file-name-directory buffer-file-truename))
         default-directory)
     buf
     eproj/buffer-directory
     #'stringp)))

(defun eproj-get-matching-tags (proj tag-major-mode identifier search-with-regexp?)
  "Get all tags from PROJ and its related projects from mode TAG-MAJOR-MODE
whose name equals IDENTIFIER or matches regexp IDENTIFIER if SEARCH-WITH-REGEXP?
is non-nil.

Returns list of (tag-name tag project is-authoritative?) lists."
  (let* ((has-authoritative-projects? nil)
         (matched-tags
          (-mapcat (lambda (proj)
                     (aif (cdr-safe
                           (assq tag-major-mode
                                 (eproj--get-tags proj)))
                         (let* ((is-authoritative? (memq tag-major-mode (eproj-project/authoritative-tag-source-for proj)))
                                (tags
                                 (if search-with-regexp?
                                     (mapcan (lambda (key-and-tags)
                                               (let ((key (car key-and-tags)))
                                                 (-map (lambda (tag) (list key tag proj is-authoritative?))
                                                       (cdr key-and-tags))))
                                             (eproj-tag-index-values-where-key-matches-regexp identifier it))
                                   (-map (lambda (x) (list identifier x proj is-authoritative?))
                                         (eproj-tag-index-get identifier it nil)))))
                           (when tags
                             (setf has-authoritative-projects? (or has-authoritative-projects?
                                                                   is-authoritative?)))
                           tags)
                       nil))
                   (eproj-get-all-related-projects-for-mode proj tag-major-mode))))
    (if has-authoritative-projects?
        (let* ((tbl (make-hash-table :test #'equal))
               (lang (aif (gethash tag-major-mode eproj/languages-table)
                         it
                       (error "unsupported language %s" tag-major-mode)))
               (tag->string (eproj-language/tag->signature-func lang))
               (tag->kind (eproj-language/show-tag-kind-procedure lang)))
          (dolist (entry matched-tags)
            (let* ((tag (cadr entry))
                   (key (cons (car entry)
                              (cons (funcall tag->kind tag)
                                    (funcall tag->string proj tag)))))
              (puthash key (cons entry (gethash key tbl nil)) tbl)))
          (let ((result nil))
            (maphash (lambda (_k vs)
                       (let ((filtered (--filter (cl-fourth it) vs)))
                         (setf result (nconc (or filtered vs) result))))
                     tbl)
            result))
      matched-tags)))

(defsubst eproj--add-cached-file-for-navigation (proj-root fname files-cache)
  (cl-assert (stringp proj-root))
  (cl-assert (stringp fname))
  (puthash fname
           (file-relative-name fname proj-root)
           files-cache))

(defvar eproj-switch-to-file--history nil)

(add-to-list 'ivy-sort-functions-alist
             '(eproj-switch-to-file-or-buffer . nil))
(add-to-list 'ivy-sort-matches-functions-alist
             '(eproj-switch-to-file-or-buffer . ivy--flx-sort-filenames))
(add-to-list 'ivy-re-builders-alist
             '(eproj-switch-to-file-or-buffer . ivy--regex-fuzzy-filenames))

;;;###autoload
(defun eproj-switch-to-file-or-buffer (proj include-related-projects? include-all-buffers?)
  (let ((root (eproj-project/root proj))
        (this-command 'eproj-switch-to-file-or-buffer))
    (unless proj
      (error "No project for current buffer"))
    (let* ((all-related-projects
            (if include-related-projects?
                (eproj-get-all-related-projects proj)
              (list proj)))
           (related-roots (let ((tbl (make-hash-table :test #'equal)))
                            (dolist (pr all-related-projects)
                              (puthash (eproj-project/root pr) t tbl))
                            tbl))
           ;; List of (<display-name> . <absolute-file-name>).
           ;; <display-name> will be shown to the user anad used for completion.
           ;; <absolute-file-name> will be used to actually locate the file.
           (files nil)
           ;; Hash table to filter out duplicates.
           (collected-entries
            (make-hash-table :test #'equal :size 997))
           (on-item-selected
            (lambda (selected-entry)
              (if (stringp selected-entry)
                  (switch-to-buffer-create-if-missing selected-entry)
                (let ((target (cdr selected-entry)))
                  (if (bufferp target)
                      (switch-to-buffer-create-if-missing target)
                    (find-file target))))))
           (buffers-cache (let ((tbl (make-hash-table :test #'equal)))
                            (dolist (buf (buffer-list))
                              (awhen (buffer-file-name buf)
                                (puthash (eproj-normalise-file-name-expand-cached it) buf tbl)))
                            tbl))
           (bufs-to-add (make-hash-table :test #'equal))
           (add-file
            (lambda (abs-path rel-path)
              (let ((buf (gethash abs-path buffers-cache)))
                (if (and buf
                         (not (invisible-buffer? buf)))
                    (puthash buf buf bufs-to-add)
                  (progn
                    (dolist (p (if rel-path
                                   (list abs-path rel-path)
                                 (list abs-path)))
                      (unless (gethash p collected-entries nil)
                        (puthash p t collected-entries)
                        (push (cons p abs-path) files)))))))))
      (let ((eproj-file (concat root "/.eproj-info")))
        (when (file-exists-p eproj-file)
          (funcall add-file eproj-file nil)))
      (dolist (related-proj all-related-projects)
        (eproj-with-all-project-files-for-navigation related-proj add-file)
        (let ((eproj-file (concat (eproj-project/root related-proj) "/.eproj-info")))
          (when (file-exists-p eproj-file)
            (funcall add-file eproj-file nil))))
      (dolist (buf (nreverse (if include-all-buffers?
                                 (buffer-list)
                               (--filter (or (null (buffer-file-name it))
                                             (gethash it bufs-to-add)
                                             (when-let (pr (eproj-get-project-for-buf-lax it))
                                               (gethash (eproj-project/root pr) related-roots)))
                                         (visible-buffers)))))
        (let* ((buffer-abs-file (buffer-file-name buf))
               (names
                (if buffer-abs-file
                    ;; If buffer is under current project's root, add it under
                    ;; both relative and absolute names.
                    (if (string-prefix-p root buffer-abs-file)
                        (list (file-relative-name buffer-abs-file root)
                              buffer-abs-file)
                      (list buffer-abs-file))
                  ;; Buffer has no file, use buffer name instead.
                  (list (buffer-name buf)))))
          (dolist (name names)
            (unless (gethash name collected-entries nil)
              (puthash name t collected-entries)
              (push (cons name buf) files)))))
      (ivy-read "Buffer or file: "
                files
                :require-match nil
                :caller 'eproj-switch-to-file-or-buffer
                :history 'eproj-switch-to-file--history
                :preselect (or (if-let (abs-name (buffer-file-name))
                                   (let ((rel-name (file-relative-name abs-name root)))
                                     (cond
                                       ((gethash rel-name collected-entries nil)
                                        rel-name)
                                       ((gethash abs-name collected-entries nil)
                                        abs-name)
                                       (t
                                        nil)))
                                 (buffer-name (other-buffer (current-buffer)))))
                :action on-item-selected))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.eproj-info\\'" . emacs-lisp-mode))

(defun eproj-setup-local-variables (proj)
  (dolist (entry (eproj-query/local-variables proj major-mode nil))
    (set (make-local-variable (car entry)) (cadr entry))))

;; Local Variables:
;; End:

;; eproj.el ends here
