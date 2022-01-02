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
(require 'eproj-haskell)

(defun eproj-tests/non-special-files (path)
  "Construct list of non-special files (i.e. that typically would be under version control)
under ROOT directory."
  (--filter (not (member (file-name-nondirectory t) '(".gitignore" ".eproj-info")))
            (--filter (not (file-directory? it))
                      (directory-files path t directory-files-no-dot-files-regexp))))

(defun eproj-tests/normalize-file-list (items)
  (cl-assert (-all? #'stringp items)
             nil
             "Expected a list of strings but got: %s"
             items)
  (remove-duplicates-sorted
   (sort (--map (strip-trailing-slash (expand-file-name it)) items) #'string<)
   #'string=))

(defun eproj-tests/normalize-string-list (items)
  (sort (copy-list items) #'string<))

(defun eproj-tests/paths=? (path-a path-b)
  (string=? (expand-file-name (strip-trailing-slash path-a))
            (expand-file-name (strip-trailing-slash path-b))))

(defmacro eproj-tests--define-tests (test-name-template &rest body)
  (declare (indent 1))
  (let ((test-name-default-file-search
         (string->symbol (format test-name-template "default-file-search")))
        (test-name-executable-file-search
         (string->symbol (format test-name-template "executable-file-search")))
        (test-name-foreign-file-search
         (string->symbol (format test-name-template "foreign-file-search"))))
    `(progn
       (ert-deftest ,test-name-default-file-search ()
         (let ((eproj/default-projects (make-hash-table :test #'eq))
               ;; don't want verbose messages in the output
               (eproj-verbose-tag-loading nil)
               (find-rec-backend 'elisp))
           (eproj-reset-projects)
           ,@body))
       (ert-deftest ,test-name-executable-file-search ()
         (let ((eproj/default-projects (make-hash-table :test #'eq))
               ;; don't want verbose messages in the output
               (eproj-verbose-tag-loading nil)
               (find-rec-backend 'executable))
           (eproj-reset-projects)
           ,@body))
       (when use-foreign-libraries?
         (ert-deftest ,test-name-foreign-file-search ()
           (let ((eproj/default-projects (make-hash-table :test #'eq))
                 ;; don't want verbose messages in the output
                 (eproj-verbose-tag-loading nil)
                 (find-rec-backend 'native))
             (eproj-reset-projects)
             ,@body))))))

(defconst eproj-tests/project-dir
  (concat +emacs-config-path+ "/tests/eproj-sample-projects"))

(defconst eproj-tests/folder-with-related-projects
  (expand-file-name (concat eproj-tests/project-dir "/related-projects")))
(defconst eproj-tests/project-with-related-projects-as-subdirs
  (expand-file-name (concat eproj-tests/project-dir "/related-projects-as-subdirs")))

(defconst eproj-tests/project-with-aux-files
  (expand-file-name (concat eproj-tests/project-dir "/project-with-aux-files")))

(defconst eproj-tests/project-with-c-files
  (expand-file-name (concat eproj-tests/project-dir "/project-with-c-files")))

(defconst eproj-tests/project-with-eproj-file-and-tags-file
  (expand-file-name (concat eproj-tests/project-dir "/haskell-project-with-eproj-file-and-tags-file")))
(defconst eproj-tests/project-with-file-list
  (expand-file-name (concat eproj-tests/project-dir "/haskell-project-with-file-list")))
(defconst eproj-tests/project-with-ignored-files
  (expand-file-name (concat eproj-tests/project-dir "/haskell-project-with-ignored-files")))

(eproj-tests--define-tests
    "eproj-tests/%s/eproj-get-all-related-projects"
  (let* ((path (concat eproj-tests/folder-with-related-projects "/project-main"))
         (proj (eproj-get-project-for-path path)))

    (should (eproj-tests/paths=? path (eproj-project/root proj)))
    (should (not (null? (eproj-project/related-projects proj))))

    (let ((related-roots-norm
           (eproj-tests/normalize-file-list
            (-map #'eproj-project/root
                  (cons proj
                        (eproj-get-all-related-projects proj))))))
      (should (equal
               (sort (-map #'file-name-base related-roots-norm) #'string<)
               '("project-main"
                 "subproject1"
                 "subproject2"
                 "subsubproject1"
                 "subsubproject2")))

      (should (equal related-roots-norm
                     (eproj-tests/normalize-file-list
                      (-filter #'file-directory?
                               (directory-files
                                eproj-tests/folder-with-related-projects
                                t
                                directory-files-no-dot-files-regexp))))))))

(eproj-tests--define-tests
    "eproj-tests/%s/aux-files"
  (let* ((path eproj-tests/project-with-aux-files)
         (proj (eproj-get-project-for-path path)))
    (should (not (null? proj)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))
    (should (not (null? (eproj-project/aux-files proj))))
    (should (equal (eproj-tests/normalize-file-list
                    (eproj-project/aux-files proj))
                   (eproj-tests/normalize-file-list
                    (-filter #'file-regular?
                             (directory-files
                              (concat eproj-tests/project-with-aux-files
                                      "/aux-files")
                              t
                              directory-files-no-dot-files-regexp)))))
    (let ((navigation-files nil))

      (eproj-with-all-project-files-for-navigation proj
                                                   (lambda (abs-path _)
                                                     (push abs-path navigation-files)))

      (should (-all? #'stringp navigation-files))
      (should (-all? #'file-name-absolute-p navigation-files))
      (dolist (file '("README.md"
                      "foo.extra"
                      "bar.extra"
                      "file1.txt"
                      "1.quux"
                      "2.quux"))
        (should (member file
                        (-map #'file-name-nondirectory
                              navigation-files)))))))

(eproj-tests--define-tests
    "eproj-tests/%s/tags-of-c-files"
  (let* ((path eproj-tests/project-with-c-files)
         (proj (eproj-get-project-for-path path))
         (tags-index
          (cdr-safe (assq 'c-mode
                          (eproj--get-tags proj))))
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
    (should-not (null? tags-index))
    (should-not (= 0 (length (eproj--get-tags proj))))
    (should (eproj-tag-index-p tags-index))
    (should-not (= 0 (eproj-tag-index-size tags-index)))
    (should-not (= 0 (length (eproj-tag-index-keys tags-index))))
    (should (equal (eproj-tests/normalize-string-list project-names)
                   (eproj-tests/normalize-string-list
                    (eproj-tag-index-keys tags-index))))))

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

(eproj-tests--define-tests
    "eproj-tests/%s/project-with-eproj-file-and-tags-file"
  (let* ((path eproj-tests/project-with-eproj-file-and-tags-file)
         (proj (eproj-get-project-for-path path)))
    (should (not (null? (eproj/find-eproj-file-location path))))
    (should (not (null? (eproj/find-eproj-file-location (concat path "/Foo")))))
    (should (not (null? (eproj/find-eproj-file-location (concat path "/Foo/Bar")))))
    (should (not (null? proj)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))

    (should (equal (eproj-tests/normalize-file-list
                    (find-rec path
                              :filep (lambda (path) (string-match-p "\\.hs$" path))))
                   (eproj-tests/normalize-file-list (eproj-get-project-files proj))))

    (let ((identity-monad-test-path (concat path "/Foo/Bar/IdentityMonad.hs"))
          (nonexistent-test-path (concat path "/Foo/Bar/Nonexistent.hs")))

      ;; IdentityM is there but not in the tags file. The file should override
      ;; the reality...
      (should-not (eproj-get-matching-tags proj
                                           'haskell-mode
                                           "IdentityM"
                                           nil))

      (should (equal
               (list (list ">>>="
                           (make-eproj-tag identity-monad-test-path
                                           13
                                           ?o
                                           nil)))
               (-map (lambda (x) (list (first x) (second x)))
                     (eproj-get-matching-tags proj
                                              'haskell-mode
                                              ">>>="
                                              nil))))

      ;; This is only present in the tags file
      (should (equal
               (list (list "outdated"
                           (make-eproj-tag nonexistent-test-path
                                           100000
                                           ?f
                                           nil)))
               (-map (lambda (x) (list (first x) (second x)))
                     (eproj-get-matching-tags proj
                                              'haskell-mode
                                              "outdated"
                                              nil)))))))

(eproj-tests--define-tests
    "eproj-tests/%s/project-with-file-list"
  (let* ((path eproj-tests/project-with-file-list)
         (proj (eproj-get-project-for-path path)))
    (should (not (null? proj)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))

    (should (equal (eproj-tests/normalize-file-list
                    (list (concat path "/Foo/Bar/Test.hs")))
                   (eproj-tests/normalize-file-list (eproj-get-project-files proj))))))

(eproj-tests--define-tests
 "eproj-tests/%s/project-with-ignored-files"
 (let* ((path eproj-tests/project-with-ignored-files)
        (proj (eproj-get-project-for-path path)))
   (should (not (null? proj)))
   (should (eproj-tests/paths=? path (eproj-project/root proj)))

   (let ((distribution-test-path (concat path "/Foo/Distribution/Test.hs")))
     (should (equal (eproj-tests/normalize-file-list
                     (list
                      (concat path "/Foo/Bar/IdentityMonad.hs")
                      distribution-test-path))
                    (eproj-tests/normalize-file-list (eproj-get-project-files proj))))
     (should (eproj-get-matching-tags proj
                                      'haskell-mode
                                      "IdentityM"
                                      nil))
     (should (equal
              (list (list "distributionTest1"
                          (make-eproj-tag distribution-test-path
                                          17
                                          ?f
                                          nil)))
              (-map (lambda (x) (list (first x) (second x)))
                    (eproj-get-matching-tags proj
                                             'haskell-mode
                                             "distributionTest1"
                                             nil))))
     (should (equal
              (list (list "distributionTest2"
                          (make-eproj-tag distribution-test-path
                                          17
                                          ?f
                                          nil)))
              (-map (lambda (x) (list (first x) (second x)))
                    (eproj-get-matching-tags proj
                                             'haskell-mode
                                             "distributionTest2"
                                             nil))))
     (should-not (eproj-get-matching-tags proj
                                          'haskell-mode
                                          "IgnoredM"
                                          nil)))))

(eproj-tests--define-tests
    "eproj-tests/%s/eproj-related-project-files-are-not-included-into-main-project"
  (let* ((path (concat eproj-tests/project-with-related-projects-as-subdirs
                       "/main-project"))
         (proj (eproj-get-project-for-path path)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))
    (should (not (null? (eproj-project/related-projects proj))))

    (let ((related-roots-norm
           (eproj-tests/normalize-file-list
            (-map #'eproj-project/root
                  (cons proj
                        (eproj-get-all-related-projects proj))))))
      (should (equal
               (sort (-map #'file-name-base related-roots-norm) #'string<)
               '("main-project"
                 "subproj1"
                 "subproj2")))

      (should (equal '("src/Bar.hs" "src/Foo.hs")
                     (--map (file-relative-name it path)
                            (eproj-tests/normalize-file-list
                             (eproj-get-project-files proj)))))

      (should (equal '("subproj1/Bar1.hs" "subproj2/src/Quux.hs")
                     (--map (file-relative-name it path)
                            (eproj-tests/normalize-file-list
                             (--mapcat (eproj-get-project-files (eproj-get-project-for-path it))
                                       (eproj-project/related-projects proj))))))

      (should (equal '("Foo.txt" "src/Bar.txt")
                     (--map (file-relative-name it path)
                            (eproj-tests/normalize-file-list
                             (eproj-project/aux-files proj))))))))

;;;; eproj/ctags-get-tags-from-buffer

(eproj-tests--define-tests
    "eproj-tests/%s/eproj/get-ctags-from-buffer"
  (let* ((test-root (fold-platform-os-type "/home/test/whatever"
                                           "c:/home/test/whatever"))
         (test-filename "foo.bar")
         (test-filename-abs (expand-file-name test-filename test-root)))
    (eproj-tests/test-ctags-get-tags-from-buffer
     (format
      "\
foo1	%s	100;\"	x foo:bar
foo2	%s	101 ;\"	y quux:frob
foo3	%s	102	;\"	z
"
      test-filename
      test-filename
      test-filename)
     tags-index
     (eproj/ctags-get-tags-from-buffer test-root (current-buffer))
     (should-not (= 0 (eproj-tag-index-size tags-index)))

     (let ((tag1 (car-safe (eproj-tag-index-get "foo1" tags-index))))
       (should tag1)
       (should (string=? test-filename-abs (eproj-tag/file tag1)))
       (should (= 100 (eproj-tag/line tag1)))
       (should (equal ?x (eproj-tag/type tag1))))

     (let ((tag2 (car-safe (eproj-tag-index-get "foo2" tags-index))))
       (should tag2)
       (should (string=? test-filename-abs (eproj-tag/file tag2)))
       (should (= 101 (eproj-tag/line tag2)))
       (should (equal ?y (eproj-tag/type tag2))))

     (let ((tag3 (car-safe (eproj-tag-index-get "foo3" tags-index))))
       (should tag3)
       (should (string=? test-filename-abs (eproj-tag/file tag3)))
       (should (= 102 (eproj-tag/line tag3)))
       (should (equal ?z (eproj-tag/type tag3)))))))

;;;; eproj/get-fast-tags-tags-from-buffer

(eproj-tests--define-tests
    "eproj-tests/%s/eproj/get-fast-tags-from-buffer"
  (let* ((test-root (fold-platform-os-type "/home/test/whatever"
                                           "c:/home/test/whatever"))
         (test-filename "foo.bar")
         (test-filename-abs (expand-file-name test-filename test-root)))
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
     tags-index
     (eproj/get-fast-tags-tags-from-buffer test-root (current-buffer))
     (should-not (= 0 (eproj-tag-index-size tags-index)))

     (let ((tag1 (car-safe (eproj-tag-index-get "foo1" tags-index))))
       (should tag1)
       (should (string=? test-filename-abs (eproj-tag/file tag1)))
       (should (= 100 (eproj-tag/line tag1)))
       (should (equal ?x (eproj-tag/type tag1))))

     (let ((tag2 (car-safe (eproj-tag-index-get "foo2" tags-index))))
       (should tag2)
       (should (string=? test-filename-abs (eproj-tag/file tag2)))
       (should (= 101 (eproj-tag/line tag2)))
       (should (equal ?y (eproj-tag/type tag2))))

     (let ((tag3 (car-safe (eproj-tag-index-get "foo3" tags-index))))
       (should tag3)
       (should (string=? test-filename-abs (eproj-tag/file tag3)))
       (should (= 102 (eproj-tag/line tag3)))
       (should (equal ?z (eproj-tag/type tag3)))))))

(eproj-tests--define-tests
    "eproj-tests/%s/eproj/get-fast-tags-from-buffer/filenames-with-spaces"
  (let ((test-filename (fold-platform-os-type
                        "/home/admin/my projects/test project/hello.c"
                        "c:/home/admin/my projects/test project/hello.c")))
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
     tags-index
     (eproj/get-fast-tags-tags-from-buffer nil (current-buffer))
     (should-not (= 0 (eproj-tag-index-size tags-index)))

     (let ((tag1 (car-safe (eproj-tag-index-get "foo1" tags-index))))
       (should tag1)
       (should (string=? test-filename (eproj-tag/file tag1)))
       (should (= 100 (eproj-tag/line tag1)))
       (should (equal ?x (eproj-tag/type tag1))))

     (let ((tag2 (car-safe (eproj-tag-index-get "foo2" tags-index))))
       (should tag2)
       (should (string=? test-filename (eproj-tag/file tag2)))
       (should (= 101 (eproj-tag/line tag2)))
       (should (equal ?y (eproj-tag/type tag2))))

     (let ((tag3 (car-safe (eproj-tag-index-get "foo3" tags-index))))
       (should tag3)
       (should (string=? test-filename (eproj-tag/file tag3)))
       (should (= 102 (eproj-tag/line tag3)))
       (should (equal ?z (eproj-tag/type tag3)))))))

(eproj-tests--define-tests
    "eproj-tests/%s/eproj/get-fast-tags-from-buffer/ignore-constructor-tags-that-repeat-type-tags"
  (let ((test-filename (fold-platform-os-type "/home/sergey/Test.hs"
                                              "c:/home/sergey/Test.hs")))
    (eproj-tests/test-ctags-get-tags-from-buffer
     (format
      "\
Identity	%s	100;\"	C
Identity	%s	101 ;\"	t
test	%s	102	;\"	f
"
      test-filename
      test-filename
      test-filename)
     tags-index
     (eproj/get-fast-tags-tags-from-buffer nil (current-buffer))
     (should-not (= 0 (eproj-tag-index-size tags-index)))

     (let ((type-tag (car-safe (eproj-tag-index-get "Identity" tags-index))))
       (should type-tag)
       (should (string=? test-filename (eproj-tag/file type-tag)))
       (should (= 101 (eproj-tag/line type-tag)))
       (should (equal ?t (eproj-tag/type type-tag))))

     (let ((function-tag (car-safe (eproj-tag-index-get "test" tags-index))))
       (should function-tag)
       (should (string=? test-filename (eproj-tag/file function-tag)))
       (should (= 102 (eproj-tag/line function-tag)))
       (should (equal ?f (eproj-tag/type function-tag)))))))



;; (let ((ert-debug-on-error nil))
;;   (eproj-reset-projects)
;;   (ert (join-lines (-map (lambda (x) (concat "^" (symbol->string x) "$"))
;;                         eproj-tests/tests)
;;                    "\\|")
;;        ;; "eproj-tests/.*"
;;        )
;;   nil)

;; (progn
;;   (ert "eproj-tests/.*")
;;   nil)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; eproj-tests.el ends here
