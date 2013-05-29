;; eproj-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 14 May 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'ert)

(require 'eproj)


(defun eproj-tests/non-special-files (path)
  "Construct list of non-special files (i.e. that probably would be under version control)
under ROOT directory."
  (filter (comp #'not
                (partial-first #'member '(".gitignore" ".eproj-info"))
                #'file-name-nondirectory)
          (filter (comp #'not #'file-directory?)
                  (get-directory-contents path :full t))))

(defun eproj-tests/normalize-file-list (items)
  (sort (map #'expand-file-name items) #'string<))

(defun eproj-tests/normalize-string-list (items)
  (sort (copy-list items) #'string<))

(defun eproj-tests/hash-table-keys (table)
  (let ((keys nil))
    (maphash (lambda (key value)
               (push key keys))
             table)
    keys))


(defconst eproj-tests/project-dir
  (concat +emacs-config-path+ "/tests/eproj-sample-projects"))
(defconst eproj-tests/project-without-git
  (expand-file-name (concat eproj-tests/project-dir "/project-without-git")))
(defconst eproj-tests/project-with-git-minimal
  (expand-file-name (concat eproj-tests/project-dir "/project-with-git")))

(defconst eproj-tests/folder-with-related-projects
  (expand-file-name (concat eproj-tests/project-dir "/related-projects")))
(defconst eproj-tests/project-with-git-with-related
  (expand-file-name (concat eproj-tests/folder-with-related-projects "/project-main")))

(defconst eproj-tests/project-with-aux-files
  (expand-file-name (concat eproj-tests/project-dir "/project-with-aux-files")))

(defconst eproj-tests/project-with-c-files
  (expand-file-name (concat eproj-tests/project-dir "/project-with-c-files")))


(ert-deftest eproj-tests/git-repository-test1 ()
  "Test `git-get-repository-root' for directory without git repository."
  (let ((path eproj-tests/project-without-git))
    (should-not (string= path (expand-file-name (git-get-repository-root path))))))

(ert-deftest eproj-tests/git-repository-test2 ()
  "Test `git-get-repository-root' for directory with git repository."
  (let ((path eproj-tests/project-with-git-minimal))
    (should (string= path (expand-file-name (git-get-repository-root path))))))


(ert-deftest eproj-tests/eproj-make-project-no-git-repo ()
  (let ((path eproj-tests/project-without-git))
    (should-not (condition-case nil
                    (let ((proj (eproj-make-project path)))
                      (string= path (expand-file-name (eproj-project/root proj))))
                  (error nil)))))

(ert-deftest eproj-tests/eproj-make-project-minimal-project ()
  (let* ((path eproj-tests/project-with-git-minimal)
         (proj (eproj-make-project path)))
    (should (not (null? proj)))
    (should (string= path (expand-file-name (eproj-project/root proj))))
    (should (eproj-project/root= proj proj))
    (should-not (null? (eproj-project/aux-info proj)))
    (should (null? (eproj-project/related-projects proj)))
    (should (null? (eproj-project/aux-files proj)))
    (should (equal? (eproj-tests/normalize-file-list
                     (map (lambda (f) (expand-file-name f (eproj-project/root proj)))
                          (eproj-get-project-files proj)))
                    (eproj-tests/normalize-file-list
                     (map (lambda (f) (expand-file-name f path))
                          (eproj-tests/non-special-files path)))))
    (should (all? #'symbol? (eproj-project/languages proj)))))

(ert-deftest eproj-tests/eproj-get-all-related-projects ()
  (let* ((path eproj-tests/project-with-git-minimal)
         (proj (eproj-make-project path)))
    (should (string= path (expand-file-name (eproj-project/root proj))))
    (should (null? (eproj-project/related-projects proj))))
  (let* ((path eproj-tests/project-with-git-with-related)
         (proj (eproj-make-project path)))
    (should (string= path (expand-file-name (eproj-project/root proj))))
    (should (not (null? (eproj-project/related-projects proj))))
    (should (equal (eproj-tests/normalize-file-list
                    (map #'eproj-project/root
                         (cons proj
                               (eproj-get-all-related-projects proj))))
                   (eproj-tests/normalize-file-list
                    (filter #'file-directory?
                            (get-directory-contents
                             eproj-tests/folder-with-related-projects
                             :full t)))))))

(ert-deftest eproj-tests/aux-files ()
  (let* ((path eproj-tests/project-with-aux-files)
         (proj (eproj-make-project path)))
    (should (not (null? proj)))
    (should (string= path (expand-file-name (eproj-project/root proj))))
    (should (equal (eproj-tests/normalize-file-list
                    (eproj-project/aux-files proj))
                   (eproj-tests/normalize-file-list
                    (filter #'file-regular?
                            (get-directory-contents
                             (concat eproj-tests/project-with-aux-files
                                     "/aux-files")
                             :full t)))))))

(ert-deftest eproj-tests/tags-of-c-files ()
  (let* ((path eproj-tests/project-with-c-files)
         (proj (eproj-make-project path))
         (tags-table
          (cdr-safe (assoc 'c-mode
                           (eproj-project/tags proj))))
         (project-names
          '("bigint"
            "bigint_list_s"
            "bigint_list_s::item"
            "bigint_list_s::next"
            "bigint_list_t"
            "factorial"
            "FACTORIAL_H_"
            "item"
            "next")))
    (should-not (null? proj))
    (should (string= path (expand-file-name (eproj-project/root proj))))
    (should-not (null? tags-table))
    (should-not (= 0 (length (eproj-project/tags proj))))
    (should (hash-table-p tags-table))
    (should-not (= 0 (hash-table-count tags-table)))
    (should-not (= 0 (length (eproj-tests/hash-table-keys tags-table))))
    (should (equal (eproj-tests/normalize-string-list project-names)
                   (eproj-tests/normalize-string-list
                    (eproj-tests/hash-table-keys tags-table))))))



(setf eproj-tests/tests
      '(eproj-tests/git-repository-test1
        eproj-tests/git-repository-test2
        eproj-tests/eproj-make-project-no-git-repo
        eproj-tests/eproj-make-project-minimal-project
        eproj-tests/eproj-get-all-related-projects
        eproj-tests/aux-files
        eproj-tests/tags-of-c-files))

(let ((ert-debug-on-error nil))
  (eproj-reset-projects)
  (ert (join-lines (map #'symbol->string eproj-tests/tests) "\\|")
       ;; "eproj-tests/.*"
       ))

;; Local Variables:
;; no-byte-compile: t
;; End:

;; eproj-tests.el ends here
