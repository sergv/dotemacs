;; eproj.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 30 December 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'custom)
(require 'custom-predicates)

;;;; eproj-tag

(defvar eproj/registered-filenames (make-hash-table :test #'equal)
  "Hashtable binding filename strings to themselves. Exists for memory
optimization reasons.")

(defun eproj/registered-filename (filename)
  (aif (gethash filename eproj/registered-filenames)
    it
    (progn
      (puthash filename filename eproj/registered-filenames)
      filename)))

(defstruct (eproj-tag
            (:conc-name eproj-tag/))
  symbol
  file
  line
  properties)

;;;; eproj languages

(defstruct (eproj-language
            (:conc-name eproj-language/))
  mode
  extension-re
  load-procedure ;; procedure taking project root and aux-info data structure
  tag->string-procedure)

(defvar eproj/languages
  (list (make-eproj-language :mode 'c-mode
                             :extension-re (rx bol
                                               (or "c" "h")
                                               eol)
                             :load-procedure nil
                             :tag->string-procedure nil)
        (make-eproj-language :mode 'c++-mode
                             :extension-re (rx bol
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
                             :load-procedure nil
                             :tag->string-procedure nil)
        (make-eproj-language :mode 'python-mode
                             :extension-re (rx bol
                                               (or "py" "pyx" "pxd" "pxi")
                                               eol)
                             :load-procedure nil
                             :tag->string-procedure nil)
        (make-eproj-language :mode 'clojure-mode
                             :extension-re (rx bol
                                               (or "clj")
                                               eol)
                             :load-procedure nil
                             :tag->string-procedure nil)
        (make-eproj-language :mode 'java-mode
                             :extension-re (rx bol
                                               (or "java")
                                               eol)
                             :load-procedure nil
                             :tag->string-procedure nil)))

;;;; eproj-project

(defstruct (eproj-project
            (:conc-name eproj-project/))
  type ;; one of symbols: git
  root
  aux-info
  tags             ;; list of (language-major-mode . <tags-table>);
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


(defun eproj-reload-tags (proj)
  "Reload tags for PROJ."
  (setf (eproj-project/tags proj)
        (map (lambda (lang)
               (aif (find lang eproj/languages :key #'eproj-language/mode)
                 (funcall (eproj-language/load-procedure lang)
                          (eproj-project/root proj)
                          (eproj-project/aux-info proj))
                 (error "No such language: %s" lang)))
             (eproj-project/languages proj)))
  nil)

(defun eproj-reload-project (proj)
  (aif (git-get-repository-root (eproj-project/root proj))
    (let* ((proj-root it)
           (eproj-info-file (concat (strip-trailing-slash proj-root)
                                    "/.eproj-info"))
           (aux-info (if (file-exists? eproj-info-file)
                       (with-temp-buffer
                         (cd proj-root)
                         (insert-file-contents-literally eproj-info-file)
                         (read
                          (buffer-substring-no-properties (point-min) (point-max))))
                       nil))
           (languages (aif (cdr-safe (assoc 'languages aux-info))
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
  (aif (git-get-repository-root root)
    (let* ((proj-root it)
           (proj
            (make-eproj-project :type 'git
                                :root proj-root
                                :tags nil
                                :aux-info nil
                                :related-projects nil
                                :aux-files-source nil
                                :languages nil)))
      (eproj-reload-project proj)
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
    (append (git-get-tracked-files (eproj-project/root proj))
            (eproj-project/aux-files proj))))

(defun eproj-get-related-projects (root aux-info)
  "Return list of roots of related project for folder ROOT and AUX-INFO.
AUX-INFO is expected to be a list with entry (related { <abs-path> | <rel-path> }* ).
Returns nil if no relevant entry found in AUX-INFO."
  (let ((project-root root))
    (when-let (related-entry (cdr-safe
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
    (when-let (aux-files-entry (cdr-safe
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
                                          (file-exist? tree-root)
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
  "Return all roots of projects realted to PROJ except PROJ itself."
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

;;;; navigation over homes



(provide 'eproj)

;; Local Variables:
;; End:

;; eproj.el ends here
