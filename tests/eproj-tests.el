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
  (filter (comp #'not (partial-first #'member '(".gitignore" ".eproj-info")))
          (filter (comp #'not #'file-directory?)
                  (get-directory-contents path :full t))))

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
   (should (null? (eproj-project/aux-info proj)))
   (should (null? (eproj-project/related-projects proj)))
   (should (null? (eproj-project/aux-files proj)))
   (should (equal? (map (lambda (f) (expand-file-name f (eproj-project/root proj)))
                        (eproj-get-project-files proj))
                   (map (lambda (f) (expand-file-name f path))
                        (eproj-tests/non-special-files path))))))

(ert-deftest eproj-tests/eproj-get-all-related-projects ()
 (let* ((path eproj-tests/project-with-git-minimal)
        (proj (eproj-make-project path)))
   (should (null? (eproj-project/related-projects proj))))
 (let* ((path eproj-tests/project-with-git-with-related)
        (proj (eproj-make-project path))
        (normalize
         (lambda (items)
           (sort (map #'expand-file-name items) #'string<))))
   (should (not (null? (eproj-project/related-projects proj))))
   (should (equal (funcall normalize (map #'eproj-project/root
                                          (cons proj
                                                (eproj-get-all-related-projects proj))))
                  (funcall normalize
                           (filter #'file-directory?
                                   (get-directory-contents
                                    eproj-tests/folder-with-related-projects
                                    :full t)))))))

(ert-deftest eproj-tests/aux-files ()
 (let* ((path eproj-tests/project-with-aux-files)
        (proj (eproj-make-project path))
        (normalize
         (lambda (items)
           (sort (map #'expand-file-name items) #'string<))))
   (should (not (null? proj)))
   (should (equal (funcall normalize (eproj-project/aux-files proj))
                  (funcall normalize
                           (filter #'file-regular?
                                   (get-directory-contents
                                    (concat eproj-tests/project-with-aux-files
                                            "/aux-files")
                                    :full t)))))))

;; (let* ((path eproj-tests/project-with-aux-files)
;;        (proj (eproj-make-project path))
;;        (normalize
;;         (lambda (items)
;;           (sort (map #'expand-file-name items) #'string<))))
;;   (should (not (null? proj)))
;;   (should (equal (funcall normalize (eproj-project/aux-files proj))
;;                  (funcall normalize
;;                           (filter #'file-regular?
;;                                   (get-directory-contents
;;                                    (concat eproj-tests/project-with-aux-files
;;                                            "/aux-files")
;;                                    :full t))))))


(setf eproj-tests/tests
      '(eproj-tests/git-repository-test1
        eproj-tests/git-repository-test2
        eproj-tests/eproj-make-project-no-git-repo
        eproj-tests/eproj-make-project-minimal-project
        eproj-tests/eproj-get-all-related-projects
        eproj-tests/aux-files))

(let ((ert-debug-on-error nil))
  (eproj-reset-projects)
  (ert (join-lines (map #'symbol->string eproj-tests/tests) "\\|")
       ;; "eproj-tests/.*"
       ))

;; Local Variables:
;; End:

;; eproj-tests.el ends here
