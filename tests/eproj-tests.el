;; eproj-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 14 May 2013
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'common)
(require 'dash)
(require 'eproj)
(require 'eproj-haskell)
(require 'find-files)

(require 'ert)
(require 'haskell-tests)

(defun eproj-tests/non-special-files (path)
  "Construct list of non-special files (i.e. that typically would be under version control)
under ROOT directory."
  (--filter (not (member (file-name-nondirectory t) '(".gitignore" ".eproj-info")))
            (--filter (not (file-directory? it))
                      (directory-files path t directory-files-no-dot-files-regexp))))

(defun eproj-tests/sort-file-list (items &optional root)
  (cl-assert (-all? #'stringp items)
             nil
             "Expected a list of strings but got: %s"
             items)
  (remove-duplicates-sorted!
   (sort (--map (strip-trailing-slash it) items) #'string<)
   #'string=))

(defun eproj-tests/normalize-file-list (items &optional root)
  (cl-assert (-all? #'stringp items)
             nil
             "Expected a list of strings but got: %s"
             items)
  (eproj-tests/sort-file-list
   (--map (expand-file-name it root) items)))

(defun eproj-tests/normalize-string-list (items)
  (sort (copy-list items) #'string<))

(defun eproj-tests/paths=? (path-a path-b)
  (string=? (expand-file-name (strip-trailing-slash path-a))
            (expand-file-name (strip-trailing-slash path-b))))

(defmacro eproj-tests--define-tests (test-name &rest body)
  (declare (indent 1))
  (let ((test-name-elisp-file-search
         (string->symbol (format "%s//elisp-file-search" test-name)))
        (test-name-executable-file-search
         (string->symbol (format "%s//executable-file-search" test-name)))
        (test-name-foreign-file-search
         (string->symbol (format "%s//foreign-file-search" test-name))))
    `(progn
       (ert-deftest ,test-name-elisp-file-search ()
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

(defconst eproj-tests/project-with-manual-default
  (expand-file-name (concat eproj-tests/project-dir "/haskell-project-with-manual-default")))

(defconst eproj-tests/implicit-haskell-project-archive
  (expand-file-name (concat eproj-tests/project-dir "/haskell-implicit-project.zip")))

(defconst eproj-tests/implicit-haskell-project2-archive
  (expand-file-name (concat eproj-tests/project-dir "/haskell-implicit-project-alt-cabal.project.zip")))

(defconst eproj-tests/implicit-haskell-project-with-local-archive
  (expand-file-name (concat eproj-tests/project-dir "/haskell-implicit-project-with-local.zip")))

(defconst eproj-tests/haskell-project-with-aux-files
  (expand-file-name (concat eproj-tests/project-dir "/haskell-project-with-aux-files/prefix-long")))

(defconst eproj-tests/haskell-project-authoritative
  (expand-file-name (concat eproj-tests/project-dir "/haskell-project-authoritative")))

(eproj-tests--define-tests
    "eproj-tests/eproj-get-all-related-projects"
  (let* ((path (concat eproj-tests/folder-with-related-projects "/project-main"))

         (expected-related-basenames
          '("project-main"
            "subproject1"
            "subproject2"
            "subsubproject1"
            "subsubproject2"))
         (expected-related-full
          (eproj-tests/normalize-file-list
           (-filter #'file-directory?
                    (directory-files
                     eproj-tests/folder-with-related-projects
                     t
                     directory-files-no-dot-files-regexp)))))

    (let ((run-check
           (lambda ()
             (let ((proj (eproj-get-project-for-path path)))

               (should (eproj-tests/paths=? path (eproj-project/root proj)))
               (should (not (null (eproj-project/related-projects proj))))

               (let ((related-roots-norm
                      (eproj-tests/normalize-file-list
                       (-map #'eproj-project/root
                             (eproj-get-all-related-projects proj)))))
                 (should (equal
                          (sort (-map #'file-name-base related-roots-norm) #'string<)
                          expected-related-basenames))

                 (should (equal related-roots-norm expected-related-full)))))))
      (funcall run-check)
      (eproj-update-projects)
      (funcall run-check)
      (eproj-reset-projects)
      (funcall run-check))))

(eproj-tests--define-tests
    "eproj-tests/aux-files"
  (let* ((path eproj-tests/project-with-aux-files)
         (proj (eproj-get-project-for-path path)))
    (should (not (null proj)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))
    (should (not (null (eproj--aux-files proj))))
    (should (equal (eproj-tests/normalize-file-list
                    (eproj--aux-files proj))
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

      (let ((navigation-files-basenames (-map #'file-name-nondirectory navigation-files)))
        (dolist (file '("README.md"
                        "foo.extra"
                        "bar.extra"
                        "file1.txt"
                        "1.quux"
                        "2.quux"))
          (should (member file navigation-files-basenames)))))))

(eproj-tests--define-tests
    "eproj-tests/tags-of-c-files"
  (progn
    (unless (executable-find eproj-ctags--exec)
      (ert-skip "ctags not available"))

    (let* ((path eproj-tests/project-with-c-files)
           (proj (eproj-get-project-for-path path))
           (tags-index-thunk (cdr (assq 'c-mode (eproj-project/tags proj))))
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
      (should-not (null proj))
      (should (eproj-tests/paths=? path (eproj-project/root proj)))
      (should-not (= 0 (length (eproj-project/tags proj))))
      (should (eproj-thunk-p tags-index-thunk))
      (let ((tags-index (eproj-thunk-get-value tags-index-thunk)))
        (should-not (null tags-index))
        (should (eproj-tag-index-p tags-index))
        (should-not (= 0 (eproj-tag-index-size tags-index)))
        (should-not (= 0 (length (eproj-tag-index-keys tags-index))))
        (should (equal (eproj-tests/normalize-string-list project-names)
                       (eproj-tests/normalize-string-list
                        (eproj-tag-index-keys tags-index))))))))

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
    "eproj-tests/project-with-eproj-file-and-tags-file"
  (let ((path eproj-tests/project-with-eproj-file-and-tags-file))
    (eproj-reset-projects)
    (should (not (null (eproj-get-initial-project-root path))))
    (eproj-reset-projects)
    (should (not (null (eproj-get-initial-project-root (concat path "/Foo")))))
    (eproj-reset-projects)
    (should (not (null (eproj-get-initial-project-root (concat path "/Foo/Bar")))))
    (eproj-reset-projects)
    (let ((proj (eproj-get-project-for-path path)))
      (should (not (null proj)))
      (should (eproj-tests/paths=? path (eproj-project/root proj)))

      (should (equal (eproj-tests/normalize-file-list
                      (find-rec path
                                :filep (lambda (path) (string-match-p "\\.hs$" path))))
                     (eproj-tests/normalize-file-list (eproj--get-project-files proj))))

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
                                                nil))))))))

(eproj-tests--define-tests
    "eproj-tests/project-with-file-list"
  (let* ((path eproj-tests/project-with-file-list)
         (proj (eproj-get-project-for-path path)))
    (should (not (null proj)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))

    (should (equal (eproj-tests/normalize-file-list
                    (list (concat path "/Foo/Bar/Test.hs")))
                   (eproj-tests/normalize-file-list (eproj--get-project-files proj))))))

(eproj-tests--define-tests
 "eproj-tests/project-with-ignored-files"
 (unless (cached-executable-find "fast-tags")
   (ert-skip "fast-tags not available"))
 (let* ((path eproj-tests/project-with-ignored-files)
        (proj (eproj-get-project-for-path path)))
   (should (not (null proj)))
   (should (eproj-tests/paths=? path (eproj-project/root proj)))

   (let ((distribution-test-path (concat path "/Foo/Distribution/Test.hs")))
     (should (equal (eproj-tests/normalize-file-list
                     (list
                      (concat path "/Foo/Bar/IdentityMonad.hs")
                      distribution-test-path))
                    (eproj-tests/normalize-file-list (eproj--get-project-files proj))))
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
 "eproj-tests/haskell-project-with-manually-configured-default-projects"
 (unless (cached-executable-find "fast-tags")
   (ert-skip "fast-tags not available"))
 (let* ((path eproj-tests/project-with-manual-default)
        (eproj/default-projects (alist->hash-table
                                 (list (cons 'haskell-mode
                                             (list (concat path "/default")))))))
   (let ((proj (eproj-get-project-for-path (concat path "/main"))))
     (should (not (null proj)))

     (should (memq 'haskell-mode (eproj-project/no-default-project-for proj)))

     (should (eproj-get-matching-tags proj 'haskell-mode "foo" nil))
     (should (eproj-get-matching-tags proj 'haskell-mode "bar" nil))
     (should-not (eproj-get-matching-tags proj 'haskell-mode "baz" nil))

     (let ((related-proj (eproj-get-project-for-path (concat path "/related"))))
       (should (not (null related-proj)))
       (should (null (eproj-project/no-default-project-for related-proj)))

       (should-not (eproj-get-matching-tags related-proj 'haskell-mode "foo" nil))
       (should (eproj-get-matching-tags related-proj 'haskell-mode "bar" nil))
       (should (eproj-get-matching-tags related-proj 'haskell-mode "baz" nil))))))

(eproj-tests--define-tests
    "eproj-tests/eproj-related-project-files-are-not-included-into-main-project"
  (let* ((path (concat eproj-tests/project-with-related-projects-as-subdirs
                       "/main-project"))
         (proj (eproj-get-project-for-path path)))
    (should (eproj-tests/paths=? path (eproj-project/root proj)))
    (should (not (null (eproj-project/related-projects proj))))

    (let ((related-roots-norm
           (eproj-tests/normalize-file-list
            (-map #'eproj-project/root
                  (eproj-get-all-related-projects proj)))))
      (should (equal
               (sort (-map #'file-name-base related-roots-norm) #'string<)
               '("main-project"
                 "subproj1"
                 "subproj2")))

      (should (equal '("src/Bar.hs" "src/Foo.hs")
                     (--map (file-relative-name it path)
                            (eproj-tests/normalize-file-list
                             (eproj--get-project-files proj)))))

      (should (equal '("subproj1/Bar1.hs" "subproj2/src/Quux.hs")
                     (--map (file-relative-name it path)
                            (eproj-tests/normalize-file-list
                             (--mapcat (eproj--get-project-files (eproj-get-project-for-path it))
                                       (eproj-project/related-projects proj))))))

      (should (equal '("Foo.txt" "src/Bar.txt")
                     (--map (file-relative-name it path)
                            (eproj-tests/normalize-file-list
                             (eproj--aux-files proj))))))))

(eproj-tests--define-tests
    "eproj-tests/implicit-haskell-project"

  (unless (cached-executable-find "fast-tags")
    (ert-skip "fast-tags not available"))
  (let ((tmp-dir (make-temp-file "temp" t))
        (unzip (cached-executable-find "unzip")))

    (unless unzip
      (ert-skip "unzip not available"))

    (unwind-protect
        (progn
          (call-process unzip nil nil nil
                        eproj-tests/implicit-haskell-project-archive
                        "-d" tmp-dir)

          (let ((path (concat tmp-dir "/haskell-implicit-project/main"))
                (related-roots
                 (list (concat tmp-dir "/haskell-implicit-project/dep1")
                       (concat tmp-dir "/haskell-implicit-project/dep2")
                       (concat tmp-dir "/haskell-implicit-project/main"))))

            (let ((run-check
                   (lambda ()
                     (let ((proj (eproj-get-project-for-path path)))

                       (should (not (null proj)))
                       (should (eproj-tests/paths=? path (eproj-project/root proj)))

                       (should (not (null (eproj-project/related-projects proj))))
                       (should (equal
                                (eproj-tests/normalize-file-list
                                 (-map #'eproj-project/root
                                       (eproj-get-all-related-projects proj)))
                                related-roots))

                       (dolist (name '("dep1" "dep2" "mainFunc" "subdepFoo"))
                         (should (eproj-get-matching-tags proj 'haskell-mode name nil)))))))

              (funcall run-check)
              (eproj-update-projects)
              (funcall run-check)
              (eproj-reset-projects)
              (funcall run-check))))
      (delete-directory tmp-dir t))))

(eproj-tests--define-tests
    "eproj-tests/implicit-haskell-project-alt-cabal.project"

  (unless (cached-executable-find "fast-tags")
    (ert-skip "fast-tags not available"))
  (let ((tmp-dir (make-temp-file "temp" t))
        (unzip (cached-executable-find "unzip")))

    (unless unzip
      (ert-skip "unzip not available"))

    (unwind-protect
        (progn
          (call-process unzip nil nil nil
                        eproj-tests/implicit-haskell-project2-archive
                        "-d" tmp-dir)

          (let ((path (concat tmp-dir "/haskell-implicit-project-alt-cabal.project/main"))
                (related-roots
                 (list (concat tmp-dir "/haskell-implicit-project-alt-cabal.project/dep1")
                       (concat tmp-dir "/haskell-implicit-project-alt-cabal.project/dep2")
                       (concat tmp-dir "/haskell-implicit-project-alt-cabal.project/main"))))

            (let ((run-check
                   (lambda ()
                     (let ((proj (eproj-get-project-for-path path)))

                       (should (not (null proj)))
                       (should (eproj-tests/paths=? path (eproj-project/root proj)))

                       (should (not (null (eproj-project/related-projects proj))))
                       (should (equal
                                (eproj-tests/normalize-file-list
                                 (-map #'eproj-project/root
                                       (eproj-get-all-related-projects proj)))
                                related-roots))

                       (dolist (name '("dep1" "dep2" "mainFunc" "subdepFoo"))
                         (should (eproj-get-matching-tags proj 'haskell-mode name nil)))))))

              (funcall run-check)
              (eproj-update-projects)
              (funcall run-check)
              (eproj-reset-projects)
              (funcall run-check))))
      (delete-directory tmp-dir t))))

(eproj-tests--define-tests
    "eproj-tests/implicit-haskell-project-with-local"

  (unless (cached-executable-find "fast-tags")
    (ert-skip "fast-tags not available"))
  (let ((tmp-dir (make-temp-file "temp" t))
        (unzip (cached-executable-find "unzip")))

    (unless unzip
      (ert-skip "unzip not available"))

    (unwind-protect
        (progn
          (call-process unzip nil nil nil
                        eproj-tests/implicit-haskell-project-with-local-archive
                        "-d" tmp-dir)

          (let ((path (concat tmp-dir "/haskell-implicit-project-with-local/main"))
                (related-roots
                 (list (concat tmp-dir "/haskell-implicit-project-with-local/dep1")
                       (concat tmp-dir "/haskell-implicit-project-with-local/dep2")
                       (concat tmp-dir "/haskell-implicit-project-with-local/main"))))

            (let ((run-check
                   (lambda ()
                     (let ((proj (eproj-get-project-for-path path)))

                       (should (not (null proj)))
                       (should (eproj-tests/paths=? path (eproj-project/root proj)))

                       (should (not (null (eproj-project/related-projects proj))))
                       (should (equal
                                (eproj-tests/normalize-file-list
                                 (-map #'eproj-project/root
                                       (eproj-get-all-related-projects proj)))
                                related-roots))

                       (dolist (name '("dep1" "dep2" "mainFunc" "subdepFoo"))
                         (should (eproj-get-matching-tags proj 'haskell-mode name nil)))))))

              (funcall run-check)
              (eproj-update-projects)
              (funcall run-check)
              (eproj-reset-projects)
              (funcall run-check))))
      (delete-directory tmp-dir t))))

(eproj-tests--define-tests
 "eproj-tests/haskell-project-with-aux-files"
 (unless (cached-executable-find "fast-tags")
   (ert-skip "fast-tags not available"))
 (let* ((path eproj-tests/haskell-project-with-aux-files)
        (proj (eproj-get-project-for-path path))
        (expected-navigation-files
         '("foo.cabal"
           "src/Foo.hs"
           "cabal.project"
           "dep/cabal.project"
           "dep/subdep1/subdep1.cabal"
           "dep/subdep1/src/Subdep1/Foo.hs"
           "dep/subdep1/test2.extra"
           "dep/subdep2/src/Subdep2/Bar.hs"
           "dep/subdep2/subdep2.cabal"
           "dep/subdep2/test3.foobar"
           "test.foobar"
           "test.extra")))
   (should (not (null proj)))
   (should (eproj-tests/paths=? path (eproj-project/root proj)))
   (should (not (null (eproj--aux-files proj))))

   (let ((actual-navigation-files nil))
     (eproj-with-all-project-files-for-navigation proj
                                                  (lambda (_abs-path rel-path)
                                                    (push rel-path actual-navigation-files)))
     (should (equal (eproj-tests/sort-file-list actual-navigation-files)
                    (eproj-tests/sort-file-list expected-navigation-files))))

   (dolist (name '("foobar" "subfoo" "subbar" "prefixFoo"))
     (should (eproj-get-matching-tags proj 'haskell-mode name nil)))))

(eproj-tests--define-tests
 "eproj-tests/haskell-project-authoritative"
  (unless (cached-executable-find "fast-tags")
    (ert-skip "fast-tags not available"))
 (let* ((path eproj-tests/haskell-project-authoritative)
        (authoritative-proj (eproj-get-project-for-path (concat path "/authoritative")))
        (non-authoritative-proj (eproj-get-project-for-path (concat path "/non-authoritative"))))

   (dolist (name '("foo" "bar"))
     (let ((authoritative-tags (eproj-get-matching-tags authoritative-proj 'haskell-mode name nil)))
       (should (equal (length authoritative-tags)
                      1))
       (should (--all? (member (eproj-tag/file (cadr it))
                               (list (concat path "/authoritative/src/Foo.hs")))
                       authoritative-tags)))

     (let ((non-authoritative-tags (eproj-get-matching-tags non-authoritative-proj 'haskell-mode name nil)))
       (should (equal (length non-authoritative-tags)
                      2))
       (should (--all? (member (eproj-tag/file (cadr it))
                               (list (concat path "/non-authoritative/src/Foo.hs")
                                     (concat path "/subproj/src/Foo.hs")))
                       non-authoritative-tags))))))

;;;; eproj/ctags-get-tags-from-buffer

(eproj-tests--define-tests
    "eproj-tests/eproj/get-ctags-from-buffer"
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
    "eproj-tests/eproj/get-fast-tags-from-buffer"
  (unless (cached-executable-find "fast-tags")
    (ert-skip "fast-tags not available"))
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
    "eproj-tests/eproj/get-fast-tags-from-buffer/filenames-with-spaces"
  (unless (cached-executable-find "fast-tags")
    (ert-skip "fast-tags not available"))
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
    "eproj-tests/eproj/get-fast-tags-from-buffer/ignore-constructor-tags-that-repeat-type-tags"
  (unless (cached-executable-find "fast-tags")
    (ert-skip "fast-tags not available"))
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

(haskell-tests--test-results
    eproj-tests/haskell-extract-block-1
  :actions-and-values
  (((progn
      (goto-line 2)
      (eproj/haskell-extract-block))
    "toText :: Builder -> T.Text")

   ((progn
      (goto-line 3)
      (eproj/haskell-extract-block))
    "toText (Builder size f) = T.Text arr 0 size\n  where\n    arr = TA.run $ do\n      marr <- TA.new size\n      f 0 marr\n      pure marr\n\n    foo = 1")

   ((progn
      (goto-line 4)
      (eproj/haskell-extract-block))
    "  where\n    arr = TA.run $ do\n      marr <- TA.new size\n      f 0 marr\n      pure marr\n\n    foo = 1")

   ((progn
      (goto-line 5)
      (eproj/haskell-extract-block))
    "    arr = TA.run $ do\n      marr <- TA.new size\n      f 0 marr\n      pure marr"))
  :contents
  "_|_
toText :: Builder -> T.Text
toText (Builder size f) = T.Text arr 0 size
  where
    arr = TA.run $ do
      marr <- TA.new size
      f 0 marr
      pure marr

    foo = 1")

(haskell-tests--test-result
    eproj-tests/parse-cabal-project-1
  :action
  (eproj-haskell--parse-cabal-projects (current-buffer))
  :expected-value
  '("cabal-benchmarks/"
    "Cabal-tests/"
    "Cabal-described"
    "quux with spaces/frob.cabal"
    "frobnicate.cabal"
    "Cabal-tree-diff/"
    "Cabal-QuickCheck/"

    "bar/baz.cabal"
    "quux with spaces/frob.cabal"
    "blob.cabal"
    "buzz.cabal"
    "decombobulator/decombobulate.cabal"

    "solver-benchmarks/"
    "cabal-install-solver/"
    "cabal-install/"
    "Cabal/"
    "cabal-testsuite/"
    "Cabal-syntax/")
  :contents
  "
packages: Cabal/ cabal-testsuite/ Cabal-syntax/
packages: cabal-install/
packages: cabal-install-solver/
packages: solver-benchmarks/

packages:
  -- foo
  bar/baz.cabal
  \"quux with spaces/frob.cabal\"
  blob.cabal
   -- fizz.cabal
  buzz.cabal

  decombobulator/decombobulate.cabal


tests: True

packages: Cabal-QuickCheck/
benchmarks: False
packages: Cabal-tree-diff/
packages: Cabal-described \"quux with spaces/frob.cabal\" frobnicate.cabal


packages: Cabal-tests/
packages: cabal-benchmarks/

optional-packages: ./vendored/*/*.cabal

_|_")

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

(provide 'eproj-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; eproj-tests.el ends here
