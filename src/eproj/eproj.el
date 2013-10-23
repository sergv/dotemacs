;; eproj.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 30 December 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'custom)
(require 'common)
(require 'custom-predicates)
(require 'select-mode)

;;;; eproj-tag

(defstruct (eproj-tag
            (:conc-name eproj-tag/))
  symbol ;; == name - string
  file
  line
  properties)

;;;; eproj languages

(defstruct (eproj-language
            (:conc-name eproj-language/))
  mode
  extension-re
  load-procedure ;; function taking eproj/project structure and
  ;; returning hashtable of (<identifier> . <eproj-tags>) bindings
  tag->string-procedure ;; function of one argument returning string
  )

;;; ctags facility

(defvar *ctags-exec*
  (platform-dependent-executable (concat +execs-path+ "/ctags")))

(defvar *ctags-language-flags*
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
      (group (+ (not (any ?\t ?\s ?\n))))
      "\t"
      (group (+ (not (any ?\t ?\s ?\n))))
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
      (or ";\""
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
                 (error "unknown ctags language: %s" lang-mode)))))))

(defun eproj/ctags-get-tags-from-buffer (buffer &optional root)
  "Constructs hash-table of (tag . eproj-tag) bindings extracted from buffer BUFFER.
BUFFER is expected to contain output of ctags command."
  (with-current-buffer buffer
    (save-match-data
      (goto-char (point-min))
      (let ((tags-table (make-hash-table :test #'equal)))
        (while (not (eob?))
          (when (looking-at +ctags-line-re+)
            (let ((symbol (match-string-no-properties 1))
                  (file (concat (when root (concat root "/"))
                                (match-string-no-properties 2)))
                  (line (string->number (match-string-no-properties 3))))
              (goto-char (match-end 0))
              ;; now we're past ;"
              (let* ((fields-str (buffer-substring-no-properties
                                  (point)
                                  (line-end-position)))
                     (fields
                      (delq nil
                            (map (lambda (entry)
                                   (if (string-match? (concat "^\\("
                                                              +ctags-aux-fields-re+
                                                              "\\):\\(.*\\)$")
                                                      entry)
                                     (let ((identifier (match-string-no-properties 1 entry))
                                           (value (match-string-no-properties 2 entry)))
                                       ;; when value is nonempty
                                       (when (< 0 (length value))
                                         (cons (string->symbol identifier) value)))
                                     (error "invalid entry: %s" entry)))
                                 (split-string fields-str "\t" t))))
                     (new-tag (make-eproj-tag
                               :symbol symbol
                               :file (common/registered-filename file)
                               :line line
                               :properties fields)
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


;;; language definitions

(defun eproj/generic-tag->string (tag)
  (assert (eproj-tag-p tag))
  (concat "Generic tag "
          (eproj-tag/symbol tag)
          "\n"
          (eproj-tag/file tag)
          ":"
          (eproj-tag/line tag)
          "\n"
          (eproj-tag/properties tag))
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

(defun eproj/generic-load-procedure (proj)
  "Generic loading procedure doing nothing"
  (error "not implemented yet"))

(defun eproj/load-ctags-project (lang-mode proj)
  (let ((root (eproj-project/root proj))
        (ctags-buf (get-buffer-create (concat " *"
                                              (eproj-project/root proj)
                                              "-ctags-"
                                              (symbol->string lang-mode)
                                              "*"))))
    (with-current-buffer ctags-buf
      (cd root)
      (erase-buffer))
    (eproj/run-ctags-on-files lang-mode
                              root
                              (eproj-get-project-files proj)
                              ctags-buf)
    (let ((table (eproj/ctags-get-tags-from-buffer ctags-buf)))
      (kill-buffer ctags-buf)
      table)))

(defun eproj/clojure-load-procedure (proj)
  (assert (eproj-project-p proj))
  (when (memq 'java-mode (eproj-project/languages proj))
    (load-ctags-project 'java-mode proj))
  )


(defvar eproj/languages
  (list (make-eproj-language :mode 'c-mode
                             :extension-re (rx "."
                                               (or "c" "h")
                                               eol)
                             :load-procedure
                             (lambda (proj)
                               (eproj/load-ctags-project 'c-mode proj))
                             :tag->string-procedure #'eproj/generic-tag->string)
        (make-eproj-language :mode 'c++-mode
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
                             (lambda (proj)
                               (eproj/load-ctags-project 'c++-mode proj))
                             :tag->string-procedure
                             (lambda (proj)
                               (eproj/load-ctags-project 'c++-mode proj)))
        (make-eproj-language :mode 'python-mode
                             :extension-re (rx "."
                                               (or "py" "pyx" "pxd" "pxi")
                                               eol)
                             :load-procedure
                             (lambda (proj)
                               (eproj/load-ctags-project 'python-mode proj))
                             :tag->string-procedure #'eproj/generic-tag->string)
        (make-eproj-language :mode 'clojure-mode
                             :extension-re (rx "."
                                               (or "clj"
                                                   "java")
                                               eol)
                             :load-procedure #'eproj/clojure-load-procedure
                             :tag->string-procedure #'eproj/generic-tag->string)
        (make-eproj-language :mode 'java-mode
                             :extension-re (rx "."
                                               (or "java")
                                               eol)
                             :load-procedure
                             (lambda (proj)
                               (eproj/load-ctags-project 'java-mode proj))
                             :tag->string-procedure #'eproj/generic-tag->string)))

(defvar eproj/languages-table
  (let ((table (make-hash-table :test #'eq)))
    (dolist (lang eproj/languages)
      (puthash (eproj-language/mode lang) lang table))
    table))

;;;; eproj-project

(defstruct (eproj-project
            (:conc-name eproj-project/))
  type ;; one of symbols: git
  root
  aux-info
  tags ;; list of (language-major-mode . <tags-table>);
  ;; <tags-table> - hashtable of (symbol-str . eproj-tag) bindings
  related-projects ;; list of other project roots
  aux-files-source ;; list of other files or function that yields such list
  languages        ;; list of symbols - major-modes for related languages
  )

(defun eproj-project/aux-files (proj)
  (aif (eproj-project/aux-files-source proj)
    (map (lambda (path)
           (expand-file-name path (eproj-project/root proj)))
         (cond ((functionp it)
                (funcall it))
               ((list? it)
                it)
               (else
                nil)))
    nil))

(defun eproj-project/root= (proj-a proj-b)
  (string= (eproj-project/root proj-a)
           (eproj-project/root proj-b)))

(defvar *eproj-projects*
  (make-hash-table :test #'equal))

(defun eproj-reset-projects ()
  "Clear project database `*eproj-projects*'."
  (interactive)
  (setf *eproj-projects* (make-hash-table :test #'equal)))

(defun eproj-update-projects ()
  "Update projects in database `*eproj-projects*'."
  (interactive)
  (maphash (lambda (root porj)
             (eproj-reload-project! proj))
           *eproj-projects*))

(defun eproj-update-buffer-project ()
  "Update project for current one and create one if it does not exists."
  (interactive)
  (eproj-reload-project! (eproj-get-project-for-buf (current-buffer))))

(defun eproj-describe-buffer-project ()
  (interactive)
  (if-let (proj (eproj-get-project-for-buf (current-buffer)))
    (let ((indent "    ")
          (buf (get-buffer-create (format "*%s description*" (eproj-project/root proj))))
          (to-relative-name
           (lambda (filename dir)
             (if (string-prefix? dir filename)
               (concat "."
                       (substring filename
                                  (length (strip-trailing-slash dir))))
               filename))))
      (switch-to-buffer-other-window buf)
      (with-current-buffer buf
        (erase-buffer)
        (insert "type: " (pp-to-string (eproj-project/type proj)) "\n")
        (insert "root: " (eproj-project/root proj) "\n")
        (insert "related projects:\n")
        (dolist (related-proj (eproj-project/related-projects proj))
          (insert indent (eproj-project/root related-proj)) "\n")
        (insert "tags:\n")
        (dolist (tags-entry (eproj-project/tags proj))
          (insert indent "lang: " (pp-to-string (car tags-entry)) "\n")
          (dolist (entry (sort (hash-table->alist (cdr tags-entry))
                               (lambda (a b) (string< (car a) (car b)))))
            (insert indent indent (pp-to-string (car entry)) "\n")
            (dolist (subentry (cdr entry))
              (insert indent indent indent
                      (format "%s:%s\n"
                              (funcall to-relative-name
                                       (expand-file-name (eproj-tag/file subentry))
                                       (expand-file-name (eproj-project/root proj)))
                              (eproj-tag/line subentry))))))))
    (error "no project for buffer %s" (buffer-name (current-buffer)))))



(defun eproj-reload-tags (proj)
  "Reload tags for PROJ."
  (setf (eproj-project/tags proj)
        (map (lambda (lang-mode)
               (assert (symbol? lang-mode))
               (if-let (lang (gethash lang-mode eproj/languages-table))
                 (if-let (load-proc (eproj-language/load-procedure lang))
                   (cons lang-mode (funcall load-proc proj))
                   (error "No load procedure defined for language %s" lang-mode))
                 (error "No eproj/language defined for language %s" lang-mode)))
             (eproj-project/languages proj)))
  nil)

(defun eproj-reload-project! (proj)
  (if-let (proj-root (git-get-repository-root (eproj-project/root proj)))
    (let* ((eproj-info-file (concat (strip-trailing-slash proj-root)
                                    "/.eproj-info"))
           (aux-info (if (file-exists? eproj-info-file)
                       (with-temp-buffer
                         (cd proj-root)
                         (insert-file-contents-literally eproj-info-file)
                         (read
                          (buffer-substring-no-properties (point-min) (point-max))))
                       nil))
           (languages (aif (rest-safe (assoc 'languages aux-info))
                        it
                        (progn
                          (message "warning: no languages defined for project %s" proj)
                          nil))))
      (setf (eproj-project/aux-info proj)
            aux-info
            (eproj-project/related-projects proj)
            (eproj-get-related-projects proj-root aux-info)
            (eproj-project/aux-files-source proj)
            (eproj-make-aux-files-constructor proj-root aux-info)
            (eproj-project/languages proj)
            languages)
      (eproj-reload-tags proj)
      nil)
    (error "No git repository found for project root %s" (eproj-project/root proj))))

(defun eproj-make-project (root)
  (unless (and (file-exists? root)
               (file-directory? root))
    (error "invalid project root: %s" root))
  (if-let (proj-root (git-get-repository-root root))
    (let ((proj
           (make-eproj-project :type 'git
                               :root proj-root
                               :tags nil
                               :aux-info nil
                               :related-projects nil
                               :aux-files-source nil
                               :languages nil)))
      (eproj-reload-project! proj)
      proj)
    (error "only git projects are supported for now\nerror while trying to obtain project for root %s"
           root)))

;;; utilities

(defun eproj-get-project (root)
  (aif (gethash root *eproj-projects* nil)
    it
    (let ((proj (eproj-make-project root)))
      (puthash (eproj-project/root proj)
               proj
               *eproj-projects*)
      proj)))


(defun eproj-get-project-for-buf (buffer)
  (eproj-get-project (eproj-get-project-root-for-buf buffer)))

(defun eproj-get-project-root-for-buf (buffer)
  (with-current-buffer buffer
    (when *have-git?*
      (git-update-file-repository))
    (or git-repository
        (and (buffer-file-name) (file-name-directory (buffer-file-name)))
        default-directory)))

(defun eproj-get-project-files (proj)
  "Retrieve project files for PROJ depending on it's type."
  (when (eq? (eproj-project/type proj) 'git)
    (let ((tracked (git-get-tracked-files (eproj-project/root proj)))
          (tracked-list (list)))
      (maphash (lambda (key value)
                 (push key tracked-list))
               tracked)
      (append tracked-list
              (eproj-project/aux-files proj)))))

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
                   (else
                    (error "invalid related-project entry: non-existing absolute nor relative directory: %s"
                           path))))
           related-entry))))

(defun eproj-make-aux-files-constructor (root aux-info)
  "Make up function that will return list of relative names for auxiliary files
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
                             (map (lambda (path)
                                    (file-relative-name path project-root))
                                  (find-rec tree-root
                                            :filep
                                            (lambda (path)
                                              (any? (lambda (regexp)
                                                      (string-match-pure? regexp path))
                                                    patterns))))))
                          (else
                           nil)))
                  aux-files-entry))))))



(defun eproj-get-all-related-projects (proj)
  "Return eproj-project structures of projects realted to PROJ except PROJ itself."
  (letrec ((collect
            (lambda (projs visited items)
              (if projs
                (if (member* (car projs) visited
                             :test #'eproj-project/root=)
                  (funcall collect (cdr projs) visited items)
                  (funcall collect
                           (append (map #'eproj-get-project
                                        (eproj-project/related-projects (car projs)))
                                   (cdr projs))
                           (cons (car projs) visited)
                           (cons (car projs) items)))
                items))))
    (assert (eproj-project-p proj) nil
            "Not a eproj-project structure: %s" proj)
    (funcall collect
             (map #'eproj-get-project (eproj-project/related-projects proj))
             (list proj)
             nil)))

;;;; tag/symbol navigation (navigation over homes)

(defvar eproj-symbnav/homes-history (list nil nil)
  "Two stacks of locations (previous next) from which
`eproj-symbnav/go-to-symbol-home' was invoked.")

(defvar eproj-symbnav/previous-homes nil
  "Previous locations from which symbol search was invoked.")

(defvar eproj-symbnav/next-homes nil
  "Next locations that were visited but now obscured by going back.")


(defun eproj-symbnav/identifier-at-point (&optional noerror)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (cond ((not (null? bounds))
           (buffer-substring-no-properties (car bounds) (cdr bounds)))
          ((null? noerror)
           (error "No identifier at point found"))
          (else
           nil))))



(defstruct (eproj-home-entry
            (:conc-name eproj-home-entry/))
  buffer
  point
  symbol ;; == name - string
  )


(defun eproj-symbnav/switch-to-home-entry (home-entry)
  (switch-to-buffer (eproj-home-entry/buffer home-entry))
  (goto-char (eproj-home-entry/point home-entry)))

(defun eproj-symbnav/go-to-symbol-home ()
  (interactive)
  (let* ((identifier (eproj-symbnav/identifier-at-point nil))
         (orig-major-mode major-mode)
         (proj (eproj-get-project-for-buf (current-buffer)))
         (current-home-entry (make-eproj-home-entry :buffer (current-buffer)
                                                    :point (point)
                                                    :symbol identifier))
         (jump-to-home
          (lambda (entry)
            (let ((file (eproj-tag/file entry)))
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
                  (goto-char (match-beginning 0)))))))
         (next-home-entry (car-safe eproj-symbnav/next-homes)))
    (unless (or (eproj-project/tags proj)
                (assq major-mode (eproj-project/tags proj)))
      (eproj-reload-project! proj)
      (unless (eproj-project/tags proj)
        (error "Project %s loaded no names\nProject: %s"
               (eproj-project/root proj)
               proj))
      (unless (assq major-mode (eproj-project/tags proj))
        (error "No names in project %s for language %s"
               (eproj-project/root proj)
               major-mode)))
    (if (and next-home-entry
             (string=? identifier
                       (eproj-tag/symbol next-home-entry)))
      (begin
        (eproj-symbnav/switch-to-home-entry next-home-entry)
        (pop eproj-symbnav/next-homes)
        (push current-home-entry
              eproj-symbnav/previous-homes))
      (let* ((entry->string
              (eproj-language/tag->string-procedure
               (aif (gethash orig-major-mode eproj/languages-table)
                 it
                 (error "unsupported language %s" orig-major-mode))))
             (entries
              (sort (reduce #'append
                            (map (lambda (proj)
                                   (aif (rest-safe
                                         (assq major-mode
                                               (eproj-project/tags
                                                (eproj-get-project (eproj-project/root proj)))))
                                     (gethash identifier it nil)
                                     nil))
                                 (cons proj
                                       (eproj-get-all-related-projects proj))))
                    (lambda (a b)
                      (string< (funcall entry->string a)
                               (funcall entry->string b))))))
        (cond ((null? entries)
               (error "No entries for identifier %s" identifier))
              ((null? (cdr entries))
               (funcall jump-to-home (car entries)))
              (else
               (select-start-selection
                entries
                :buffer-name "Symbol homes"
                :after-init #'ignore
                :on-selection
                (lambda (idx)
                  (select-exit)
                  (funcall jump-to-home (elt entries idx)))
                :predisplay-function
                entry->string
                :preamble-function
                (lambda ()
                  "Choose symbol\n\n"))))))))

(defun eproj-symbnav/go-back ()
  (interactive)
  (if (null? eproj-symbnav/previous-homes)
    (error "no more previous go-to-definition entries")
    (progn
      (when-let (identifier (ctags-symbols-identifier-at-point t))
        (push (make-eproj-home-entry :buffer (current-buffer)
                                     :point (point)
                                     :symbol identifier)
              eproj-symbnav/next-homes))
      (eproj-symbnav/switch-to-home-entry
       (pop eproj-symbnav/previous-homes)))))

(defun setup-eproj-symbnav ()
  (awhen (current-local-map)
    (def-keys-for-map it
      ("M-." eproj-symbnav/go-to-symbol-home)
      ("M-," eproj-symbnav/go-back)))
  (def-keys-for-map vim:normal-mode-local-keymap
    ("M-." eproj-symbnav/go-to-symbol-home)
    ("M-," eproj-symbnav/go-back)))

;;;; epilogue

(provide 'eproj)

;; Local Variables:
;; End:

;; eproj.el ends here
