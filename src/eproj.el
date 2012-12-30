;; eproj.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 30 December 2012
;; Description:

(eval-when-compile
  (require 'cl))
(require 'custom-predicates)

(defstruct eproj-project
  type ;; one of symbols: git, single-file
  root
  names
  related-projects ;; list of other project roots
  languages ;; list of symbols - major-modes for related languages
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
                                      :languages (list major-mode))))
        (puthash it
                 proj
                 *eproj-projects*)
        proj)
      (error "only git projects are supported for now, trying to obtain project for %s rot"
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
    (git-get-tracked-files (eproj-project-root project))))

(defun eproj-get-related-projects (root)
  (let ((related-files-source (concat (strip-trailing-slash root)
                                      "/.eproj_related")))
    (when (file-exists? related-files-source)
      (with-temp-buffer
        (insert-file-contents-literally related-files-source)
        (mapcar (lambda (path)
                  (cond ((file-directory? path)
                         path)
                        ((file-directory? (expand-file-name path root))
                         (expand-file-name path root))
                        (else
                         (error "invalid related-project entry: not existing absolute nor relative directory: %s"
                                path))))
                (split-string (buffer-substring-no-properties (point-min) (point-max))
                              "[\n\0]+"
                              t))))))



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
