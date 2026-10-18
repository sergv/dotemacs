;; find-files-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  8 October 2025
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'set-up-platform))

(require 'common)
(require 'dash)
(require 'ert)
(require 'find-files)
(require 'tests-utils)

(cl-defmacro find-files-tests--define-tests*
    (&key (backends '(elisp executable native emacs-native-helper))
          name
          body)
  (declare (indent nil))
  (let ((test-name-elisp
         (string->symbol (concat (symbol->string name) "//elisp-file-search")))
        (test-name-executable
         (string->symbol (concat (symbol->string name) "//executable-file-search")))
        (test-name-foreign
         (string->symbol (concat (symbol->string name) "//foreign-file-search")))
        (test-name-native-helper
         (string->symbol (concat (symbol->string name) "//emacs-native-helper"))))
    `(progn
       ,@(when (memq 'elisp backends)
           (list
            `(ert-deftest ,test-name-elisp ()
               (let ((find-rec-backend 'elisp))
                 ,body))))
       ,@(when (memq 'executable backends)
           (list
            `(ert-deftest ,test-name-executable ()
               (let ((find-rec-backend 'executable))
                 ,body))))
       ,@(when (memq 'native backends)
           (list
            `(when use-foreign-libraries?
               (ert-deftest ,test-name-foreign ()
                 (let ((find-rec-backend 'native))
                   ,body)))))
       ,@(when (memq 'emacs-native-helper backends)
           (list
            `(ert-deftest ,test-name-native-helper ()
               (when (null (foregin-setup-resolve-emacs-native-helper-path))
                 (ert-skip "emacs-native-helper not available"))
               (let ((find-rec-backend 'emacs-native-helper))
                 ,body)))))))

(defmacro find-files-tests--define-tests (test-name-template &rest body)
  (declare (indent 1))
  `(find-files-tests--define-tests*
    :name ,test-name-template
    :body (progn ,@body)))

(defconst find-file-tests-tests/project-dir
  (concat +test-utils--test-root+ "/test-data/egrep"))

(defconst find-file-tests-tests/dante-test-data
  (concat +test-utils--test-root+ "/test-data/dante"))

(find-files-tests--define-tests
    find-files-tests-1-default-use
  (should
   (equal (sort (-map #'test-utils--normalise-file-name
                      (find-rec* :root find-file-tests-tests/project-dir
                                 :globs-to-find '("*")))
                :lessp #'string<)
          (--map (test-utils--normalise-file-name (concat find-file-tests-tests/project-dir "/" it))
                 '("README.md"
                   "bar.c"
                   "bar.c.gz"
                   "src/foo.c")))))

(find-files-tests--define-tests
    find-files-tests-2-relative-paths
  (should
   (equal (sort (-map #'test-utils--normalise-file-name
                      (find-rec* :root find-file-tests-tests/project-dir
                                 :globs-to-find '("*")
                                 :relative-paths t))
                :lessp #'string<)
          (--map (test-utils--normalise-file-name it)
                 '("README.md"
                   "bar.c"
                   "bar.c.gz"
                   "src/foo.c")))))

(find-files-tests--define-tests
    find-files-tests-3-ignored-extensions-globs
  (should
   (equal (sort (-map #'test-utils--normalise-file-name
                      (find-rec* :root find-file-tests-tests/project-dir
                                 :globs-to-find '("*")
                                 :ignored-extensions-globs '("*.c")
                                 :relative-paths t))
                :lessp #'string<)
          (--map (test-utils--normalise-file-name it)
                 '("README.md"
                   "bar.c.gz")))))

(find-files-tests--define-tests
    find-files-tests-4-ignored-absolute-files-globs
  (should
   (equal (sort (-map #'test-utils--normalise-file-name
                      (find-rec* :root find-file-tests-tests/project-dir
                                 :globs-to-find '("*")
                                 :ignored-absolute-files-globs
                                 (list
                                  (concat find-file-tests-tests/project-dir
                                          "/bar*"))
                                 :relative-paths t))
                :lessp #'string<)
          (--map (test-utils--normalise-file-name it)
                 '("README.md"
                   "src/foo.c")))))

(find-files-tests--define-tests
    find-files-tests-5a-ignored-absolute-dirs-globs
  (should
   (equal (sort (-map #'test-utils--normalise-file-name
                      (find-rec* :root find-file-tests-tests/project-dir
                                 :globs-to-find '("*")
                                 :ignored-absolute-dirs-globs
                                 (list
                                  (concat find-file-tests-tests/project-dir
                                          "/*/"))
                                 :relative-paths t))
                :lessp #'string<)
          (--map (test-utils--normalise-file-name it)
                 '("README.md"
                   "bar.c"
                   "bar.c.gz")))))

(find-files-tests--define-tests
    find-files-tests-5b-ignored-absolute-dirs-globs
  (should
   (equal (sort (-map #'test-utils--normalise-file-name
                      (find-rec* :root find-file-tests-tests/project-dir
                                 :globs-to-find '("*")
                                 :ignored-absolute-dirs-globs
                                 (list
                                  (concat find-file-tests-tests/project-dir
                                          "/sr*"))
                                 :relative-paths t))
                :lessp #'string<)
          (--map (test-utils--normalise-file-name it)
                 '("README.md"
                   "bar.c"
                   "bar.c.gz")))))

(find-files-tests--define-tests
    find-files-tests-6-ignored-directories
  (should
   (equal (sort (-map #'test-utils--normalise-file-name
                      (find-rec* :root find-file-tests-tests/project-dir
                                 :globs-to-find '("*")
                                 :ignored-directories '("src")
                                 :relative-paths t))
                :lessp #'string<)
          (--map (test-utils--normalise-file-name it)
                 '("README.md"
                   "bar.c"
                   "bar.c.gz")))))

(find-files-tests--define-tests
    find-files-tests-6-ignored-directory-prefixes
  (should
   (equal (sort (-map #'test-utils--normalise-file-name
                      (find-rec* :root find-file-tests-tests/project-dir
                                 :globs-to-find '("*")
                                 :ignored-directory-prefixes '("s")
                                 :relative-paths t))
                :lessp #'string<)
          (--map (test-utils--normalise-file-name it)
                 '("README.md"
                   "bar.c"
                   "bar.c.gz")))))

(find-files-tests--define-tests*
 :name
 find-files-tests-7-find-rec-multi
 :backends (native emacs-native-helper)
 :body
 (let ((proj1-root (concat find-file-tests-tests/dante-test-data "/simple-check-project"))
       (proj2-root (concat find-file-tests-tests/dante-test-data "/simple-repl-project"))
       (proj3-root (concat find-file-tests-tests/dante-test-data "/simple-repl-project-with-hsc")))
   (should
    (equal (sort (-map #'test-utils--normalise-file-name
                       (find-rec-multi*
                        :roots (list proj1-root proj2-root proj3-root)
                        :globs-to-find '("*.cabal")))
                 :lessp #'string<)
           (sort
            (-map #'test-utils--normalise-file-name
                  (list (concat proj1-root "/emacs-dante-simple-check-test-project.cabal")
                        (concat proj2-root "/emacs-dante-simple-repl-test-project.cabal")
                        (concat proj3-root "/emacs-dante-simple-repl-test-project-with-hsc.cabal")))
            :lessp #'string<)))))

(provide 'find-files-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; find-files-tests.el ends here
