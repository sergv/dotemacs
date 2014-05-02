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
  (sort (map (comp #'strip-trailing-slash #'expand-file-name) items) #'string<))

(defun eproj-tests/normalize-string-list (items)
  (sort (copy-list items) #'string<))

(defun eproj-tests/hash-table-keys (table)
  (let ((keys nil))
    (maphash (lambda (key value)
               (push key keys))
             table)
    keys))

(defun eproj-tests/paths=? (path-a path-b)
  (string=? (expand-file-name (strip-trailing-slash path-a))
            (expand-file-name (strip-trailing-slash path-b))))



(defconst eproj-tests/project-dir
  (concat +emacs-config-path+ "/tests/eproj-sample-projects"))
(defconst eproj-tests/project-without-git
  (expand-file-name (concat eproj-tests/project-dir "/project-without-git")))
(defconst eproj-tests/project-with-git-minimal
  (expand-file-name (concat eproj-tests/project-dir "/project-with-git")))
(defconst eproj-tests/project-with-git-and-non-git-specified-type
  (expand-file-name (concat eproj-tests/project-dir
                            "/project-with-git-and-non-git-specified-type")))

(defconst eproj-tests/folder-with-related-projects
  (expand-file-name (concat eproj-tests/project-dir "/related-projects")))
(defconst eproj-tests/project-with-git-with-related
  (expand-file-name (concat eproj-tests/folder-with-related-projects "/project-main")))

(defconst eproj-tests/project-with-aux-files
  (expand-file-name (concat eproj-tests/project-dir "/project-with-aux-files")))

(defconst eproj-tests/project-with-c-files
  (expand-file-name (concat eproj-tests/project-dir "/project-with-c-files")))

(defconst eproj-tests/project-with-eproj-file
  (expand-file-name (concat eproj-tests/project-dir "/project-with-eproj-file")))


(ert-deftest eproj-tests/git-repository-test1 ()
  "Test `git-get-repository-root' for directory without git repository."
  (let ((path eproj-tests/project-without-git))
    (should-not (eproj-tests/paths=? path (git-get-repository-root path)))))

(ert-deftest eproj-tests/git-repository-test2 ()
  "Test `git-get-repository-root' for directory with git repository."
  (let ((path eproj-tests/project-with-git-minimal))
    (should (eproj-tests/paths=? path (git-get-repository-root path)))))


(ert-deftest eproj-tests/eproj-make-project-no-git-repo ()
  (let* ((path eproj-tests/project-without-git)
         (proj (eproj-get-project-for-path path)))
    (should-not (eq? 'git (eproj-project-type/name
                           (eproj-project/type
                            proj))))))

(ert-deftest eproj-tests/eproj-make-project-minimal-project ()
  (let* ((path eproj-tests/project-with-git-minimal)
         (proj (eproj-get-project-for-path path)))
    (should (not (null? proj)))
    (should (eq 'git (eproj-project-type/name
                                        (eproj-project/type proj))))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))
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

(ert-deftest eproj-tests/eproj-make-project-signal-incorrect-specified-project-type ()
  (let* ((path eproj-tests/project-with-git-and-non-git-specified-type))
    (should-error (eproj-get-project-for-path path)
                  :type 'error)))

(ert-deftest eproj-tests/eproj-get-all-related-projects ()
  (let* ((path eproj-tests/project-with-git-minimal)
         (proj (eproj-get-project-for-path path)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))
    (should (null? (eproj-project/related-projects proj))))
  (let* ((path eproj-tests/project-with-git-with-related)
         (proj (eproj-get-project-for-path path)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))
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
         (proj (eproj-get-project-for-path path)))
    (should (not (null? proj)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))
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
         (proj (eproj-get-project-for-path path))
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
    (should (eproj-tests/paths=? path (eproj-project/root proj)))
    (should-not (null? tags-table))
    (should-not (= 0 (length (eproj-project/tags proj))))
    (should (hash-table-p tags-table))
    (should-not (= 0 (hash-table-count tags-table)))
    (should-not (= 0 (length (eproj-tests/hash-table-keys tags-table))))
    (should (equal (eproj-tests/normalize-string-list project-names)
                   (eproj-tests/normalize-string-list
                    (eproj-tests/hash-table-keys tags-table))))))

(defmacro eproj-tests/test-ctags-get-tags-from-buffer
  (input
   table-var
   ctags-get-tags-from-buffer-invokation
   &rest
   body)
  (declare (indent nil))
  `(with-temp-buffer
     (insert ,input)
     (let ((,table-var ,ctags-get-tags-from-buffer-invokation))
       ,@body)))

(ert-deftest eproj-tests/eproj/ctags-get-tags-from-buffer ()
  (let ((test-filename "foo.bar"))
    (eproj-tests/test-ctags-get-tags-from-buffer
     (format
      "\
foo1	%s	100;\"	x
foo2	%s	101 ;\"	y
foo3	%s	102	;\"	z
"
      test-filename
      test-filename
      test-filename)
     tags-table
     (eproj/ctags-get-tags-from-buffer (current-buffer) nil t)
     (should-not (= 0 (hash-table-size tags-table)))

     (let ((tag1 (car-safe (gethash "foo1" tags-table))))
       (should tag1)
       (should (string=? test-filename (eproj-tag/file tag1)))
       (should (= 100 (eproj-tag/line tag1)))
       (should (equal (cons 'type "x") (assoc 'type (eproj-tag/properties tag1)))))

     (let ((tag2 (car-safe (gethash "foo2" tags-table))))
       (should tag2)
       (should (string=? test-filename (eproj-tag/file tag2)))
       (should (= 101 (eproj-tag/line tag2)))
       (should (equal (cons 'type "y") (assoc 'type (eproj-tag/properties tag2)))))

     (let ((tag3 (car-safe (gethash "foo3" tags-table))))
       (should tag3)
       (should (string=? test-filename (eproj-tag/file tag3)))
       (should (= 102 (eproj-tag/line tag3)))
       (should (equal (cons 'type "z") (assoc 'type (eproj-tag/properties tag3))))))))

(ert-deftest eproj-tests/eproj/ctags-get-tags-from-buffer/filenames-with-spaces ()
  (let ((test-filename "/home/admin/my projects/test project/hello.c"))
    (eproj-tests/test-ctags-get-tags-from-buffer
     (format
      "\
foo1	%s	100;\"	x
foo2	%s	101 ;\"	y
foo3	%s	102	;\"	z
"
      test-filename
      test-filename
      test-filename)
     tags-table
     (eproj/ctags-get-tags-from-buffer (current-buffer) nil t)
     (should-not (= 0 (hash-table-size tags-table)))

     (let ((tag1 (car-safe (gethash "foo1" tags-table))))
       (should tag1)
       (should (string=? test-filename (eproj-tag/file tag1)))
       (should (= 100 (eproj-tag/line tag1)))
       (should (equal (cons 'type "x") (assoc 'type (eproj-tag/properties tag1)))))

     (let ((tag2 (car-safe (gethash "foo2" tags-table))))
       (should tag2)
       (should (string=? test-filename (eproj-tag/file tag2)))
       (should (= 101 (eproj-tag/line tag2)))
       (should (equal (cons 'type "y") (assoc 'type (eproj-tag/properties tag2)))))

     (let ((tag3 (car-safe (gethash "foo3" tags-table))))
       (should tag3)
       (should (string=? test-filename (eproj-tag/file tag3)))
       (should (= 102 (eproj-tag/line tag3)))
       (should (equal (cons 'type "z") (assoc 'type (eproj-tag/properties tag3))))))))

;; (eproj-get-project-for-path eproj-tests/project-with-eproj-file)
(ert-deftest eproj-tests/project-with-eproj-file ()
  (let* ((path eproj-tests/project-with-eproj-file)
         (proj (eproj-get-project-for-path path)))
    (should (not (null? (eproj/find-eproj-file-location path))))
    (should (not (null? (eproj/find-eproj-file-location (concat path "/Foo")))))
    (should (not (null? (eproj/find-eproj-file-location (concat path "/Foo/Bar")))))
    (should (not (null? proj)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))
    (should (eq? 'eproj-file
                 (eproj-project-type/name (eproj-project/type proj))))

    (should (equal (eproj-tests/normalize-file-list
                    (find-rec path
                              :filep
                              (lambda (path) (string-match-pure? "\\.hs$" path))))
                   (eproj-tests/normalize-file-list (eproj-get-project-files proj))))))


(setf eproj-tests/tests
      '(eproj-tests/git-repository-test1
        eproj-tests/git-repository-test2
        eproj-tests/eproj-make-project-no-git-repo
        eproj-tests/eproj-make-project-minimal-project
        eproj-tests/eproj-make-project-signal-incorrect-specified-project-type
        eproj-tests/eproj-get-all-related-projects
        eproj-tests/aux-files
        eproj-tests/tags-of-c-files

        eproj-tests/eproj/ctags-get-tags-from-buffer
        eproj-tests/eproj/ctags-get-tags-from-buffer/filenames-with-spaces

        eproj-tests/project-with-eproj-file
        ))

(let ((ert-debug-on-error t))
  (eproj-reset-projects)
  (ert (join-lines (map #'symbol->string eproj-tests/tests) "\\|")
       ;; "eproj-tests/.*"
       ))

;; Local Variables:
;; no-byte-compile: t
;; End:

;; eproj-tests.el ends here
