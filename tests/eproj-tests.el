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
  (-filter (comp #'not
                 (partial-first #'member '(".gitignore" ".eproj-info"))
                 #'file-name-nondirectory)
           (-filter (comp #'not #'file-directory?)
                    (directory-files path t directory-files-no-dot-files-regexp))))

(defun eproj-tests/normalize-file-list (items)
  (remove-duplicates-sorted
   (sort (-map (comp #'strip-trailing-slash #'expand-file-name) items) #'string<)
   #'string=))

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

(defmacro eproj-tests--set-up-environment (&rest body)
  (declare (indent 0))
  `(let ((eproj/default-projects (make-hash-table :test #'eq))
         ;; don't want verbose messages in the output
         (eproj-verbose-tag-loading nil))
     ,@body))


(defconst eproj-tests/project-dir
  (concat +emacs-config-path+ "/tests/eproj-sample-projects"))

(defconst eproj-tests/folder-with-related-projects
  (expand-file-name (concat eproj-tests/project-dir "/related-projects")))
(defconst eproj-tests/project-with-git-with-related
  (expand-file-name (concat eproj-tests/folder-with-related-projects "/project-main")))

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

(ert-deftest eproj-tests/eproj-get-all-related-projects ()
  (eproj-tests--set-up-environment
    (let* ((path eproj-tests/project-with-git-with-related)
           (proj (eproj-get-project-for-path path)))
      (should (eproj-tests/paths=? path (eproj-project/root proj)))
      (should (not (null? (eproj-project/related-projects proj))))
      (should (equal (eproj-tests/normalize-file-list
                      (-map #'eproj-project/root
                            (cons proj
                                  (eproj-get-all-related-projects proj nil))))
                     (eproj-tests/normalize-file-list
                      (-filter #'file-directory?
                               (directory-files
                                eproj-tests/folder-with-related-projects
                                t
                                directory-files-no-dot-files-regexp))))))))

(ert-deftest eproj-tests/aux-files ()
  (eproj-tests--set-up-environment
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
                                directory-files-no-dot-files-regexp))))))))

(ert-deftest eproj-tests/tags-of-c-files ()
  (eproj-tests--set-up-environment
    (let* ((path eproj-tests/project-with-c-files)
           (proj (eproj-get-project-for-path path))
           (tags-table
            (cdr-safe (assq 'c-mode
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
                      (eproj-tests/hash-table-keys tags-table)))))))

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

(ert-deftest eproj-tests/eproj/get-fast-tags-from-buffer ()
  (eproj-tests--set-up-environment
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
       (eproj/get-fast-tags-tags-from-buffer (current-buffer))
       (should-not (= 0 (hash-table-size tags-table)))

       (let ((tag1 (car-safe (gethash "foo1" tags-table))))
         (should tag1)
         (should (string=? test-filename (eproj-tag/file tag1)))
         (should (= 100 (eproj-tag/line tag1)))
         (should (equal (cons 'type "x") (assq 'type (eproj-tag/properties tag1)))))

       (let ((tag2 (car-safe (gethash "foo2" tags-table))))
         (should tag2)
         (should (string=? test-filename (eproj-tag/file tag2)))
         (should (= 101 (eproj-tag/line tag2)))
         (should (equal (cons 'type "y") (assq 'type (eproj-tag/properties tag2)))))

       (let ((tag3 (car-safe (gethash "foo3" tags-table))))
         (should tag3)
         (should (string=? test-filename (eproj-tag/file tag3)))
         (should (= 102 (eproj-tag/line tag3)))
         (should (equal (cons 'type "z") (assq 'type (eproj-tag/properties tag3)))))))))

(ert-deftest eproj-tests/eproj/get-fast-tags-from-buffer/filenames-with-spaces ()
  (eproj-tests--set-up-environment
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
       (eproj/get-fast-tags-tags-from-buffer (current-buffer))
       (should-not (= 0 (hash-table-size tags-table)))

       (let ((tag1 (car-safe (gethash "foo1" tags-table))))
         (should tag1)
         (should (string=? test-filename (eproj-tag/file tag1)))
         (should (= 100 (eproj-tag/line tag1)))
         (should (equal (cons 'type "x") (assq 'type (eproj-tag/properties tag1)))))

       (let ((tag2 (car-safe (gethash "foo2" tags-table))))
         (should tag2)
         (should (string=? test-filename (eproj-tag/file tag2)))
         (should (= 101 (eproj-tag/line tag2)))
         (should (equal (cons 'type "y") (assq 'type (eproj-tag/properties tag2)))))

       (let ((tag3 (car-safe (gethash "foo3" tags-table))))
         (should tag3)
         (should (string=? test-filename (eproj-tag/file tag3)))
         (should (= 102 (eproj-tag/line tag3)))
         (should (equal (cons 'type "z") (assq 'type (eproj-tag/properties tag3)))))))))


(ert-deftest eproj-tests/eproj/get-fast-tags-from-buffer/ignore-constructor-tags-that-repeat-type-tags ()
  (eproj-tests--set-up-environment
    (let ((test-filename "/home/sergey/Test.hs"))
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
       tags-table
       (eproj/get-fast-tags-tags-from-buffer (current-buffer))
       (should-not (= 0 (hash-table-size tags-table)))

       (let ((type-tag (car-safe (gethash "Identity" tags-table))))
         (should type-tag)
         (should (string=? test-filename (eproj-tag/file type-tag)))
         (should (= 101 (eproj-tag/line type-tag)))
         (should (equal (cons 'type "t") (assq 'type (eproj-tag/properties type-tag)))))

       (let ((function-tag (car-safe (gethash "test" tags-table))))
         (should function-tag)
         (should (string=? test-filename (eproj-tag/file function-tag)))
         (should (= 102 (eproj-tag/line function-tag)))
         (should (equal (cons 'type "f") (assq 'type (eproj-tag/properties function-tag)))))))))


(ert-deftest eproj-tests/project-with-eproj-file-and-tags-file ()
  (eproj-tests--set-up-environment
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
                     (eproj-tests/normalize-file-list (eproj-get-project-files proj)))))))

(ert-deftest eproj-tests/project-with-file-list ()
  (eproj-tests--set-up-environment
    (let* ((path eproj-tests/project-with-file-list)
           (proj (eproj-get-project-for-path path)))
      (should (not (null? proj)))
      (should (eproj-tests/paths=? path (eproj-project/root proj)))

      (should (equal (eproj-tests/normalize-file-list
                      (list (concat path "/Foo/Bar/Test.hs")))
                     (eproj-tests/normalize-file-list (eproj-get-project-files proj)))))))

(ert-deftest eproj-tests/project-with-ignored-files ()
  (eproj-tests--set-up-environment
    (let* ((path eproj-tests/project-with-ignored-files)
           (proj (eproj-get-project-for-path path)))
      (should (not (null? proj)))
      (should (eproj-tests/paths=? path (eproj-project/root proj)))

      (should (equal (eproj-tests/normalize-file-list
                      (list (concat path "/Foo/Bar/IdentityMonad.hs")))
                     (eproj-tests/normalize-file-list (eproj-get-project-files proj))))
      (should (eproj-get-matching-tags proj
                                       'haskell-mode
                                       "IdentityM"
                                       nil))
      (should-not (eproj-get-matching-tags proj
                                           'haskell-mode
                                           "IgnoredM"
                                           nil)))))

;; (setf eproj-tests/tests
;;       '(eproj-tests/eproj-get-all-related-projects
;;         eproj-tests/aux-files
;;         eproj-tests/tags-of-c-files
;;
;;         eproj-tests/eproj/ctags-get-tags-from-buffer
;;         eproj-tests/eproj/ctags-get-tags-from-buffer/filenames-with-spaces
;;
;;         eproj-tests/project-with-eproj-file-and-tags-file
;;         eproj-tests/project-with-file-list
;;         eproj-tests/project-with-ignored-files))

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
