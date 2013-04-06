;; eproj.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 30 December 2012
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'custom)
(require 'custom-predicates)

(defstruct eproj-project
  type ;; one of symbols: git, single-file
  root
  names
  related-projects ;; list of other project roots
  aux-files        ;; list of other files or function that yields such list
  languages        ;; list of symbols - major-modes for related languages
  )

(defvar *eproj-projects*
  (make-hash-table :test #'equal))

(defun eproj-get-project (root)
  (aif (gethash root *eproj-projects* nil)
    it
    (aif (git-get-repository-root root)
      (let ((proj (make-eproj-project :type 'git
                                      :root it
                                      :names nil
                                      :related-projects
                                      (eproj-get-related-projects it)
                                      :aux-files
                                      (eproj-construct-aux-files it)
                                      :languages (list major-mode))))
        (puthash it
                 proj
                 *eproj-projects*)
        proj)
      (error "only git projects are supported for now, trying to obtain project for %s root"
             root))))

(defun eproj-get-project-for-buf (buffer)
  (let ((root (eproj-get-project-root-for-buf buffer)))
    (eproj-get-project root)))

(defun eproj-get-project-root-for-buf (buffer)
  (with-current-buffer buffer
    (when *have-git?*
      (git-update-file-repository))
    (or git-repository
        (and (buffer-file-name) (file-name-directory (buffer-file-name)))
        default-directory)))

(defun eproj-get-project-files (project)
  "Retrieve project files for PROJECT depending on it's type."
  (when (eq? (eproj-project-type project) 'git)
    (append (git-get-tracked-files (eproj-project-root project))
            (aif (eproj-project-aux-files project)
              (cond ((functionp it)
                     (funcall it))
                    ((list? it)
                     it)
                    (else
                     nil))
              nil))))

(defun eproj-get-related-projects (root)
  (let ((eproj-info-file (concat (strip-trailing-slash root)
                                 "/.eproj-info")))
    (when (file-exists? eproj-info-file)
      (with-temp-buffer
        (insert-file-contents-literally eproj-info-file)
        (map (lambda (path)
               (assert (string? path) nil
                       "invalid entry under related clause, string expected %s"
                       path)
               (cond ((file-directory? path)
                      path)
                     ((file-directory? (expand-file-name path root))
                      (expand-file-name path root))
                     (else
                      (error "invalid related-project entry: not existing absolute nor relative directory: %s"
                             path))))
             (cdr-safe
              (assoc 'related
                     (read (buffer-substring-no-properties (point-min) (point-max))))))))))

(defun eproj-construct-aux-files (root)
  (let ((eproj-info-file (concat (strip-trailing-slash root)
                                 "/.eproj-info"))
        (project-root root))
    (when (file-exists? eproj-info-file)
      (with-temp-buffer
        (cd root)
        (insert-file-contents-literally eproj-info-file)
        (let ((entry (cdr-safe
                      (assoc 'aux-files
                             (read (buffer-substring-no-properties (point-min)
                                                                   (point-max)))))))
          (when entry
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
                        entry)))))))))



(defun eproj-get-all-related-projects (root)
  "Return all roots of projects realted to PROJ except PROJ itself."
  (letrec ((collect
            (lambda (projs visited items)
              (if projs
                (if (member (car projs) visited)
                  (funcall collect (cdr projs) visited items)
                  (funcall collect
                           (append (eproj-get-related-projects (car projs))
                                   (cdr projs))
                           (cons (car projs) visited)
                           (cons (car projs) items)))
                items))))
    (funcall collect
             (eproj-get-related-projects root)
             (list root)
             nil)))

(provide 'eproj)

;; Local Variables:
;; End:

;; eproj.el ends here
