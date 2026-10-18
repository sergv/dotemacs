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

(defmacro find-files-tests--define-tests (test-name-template &rest body)
  (declare (indent 1))
  (let ((test-name-elisp
         (string->symbol (concat (symbol->string test-name-template) "//elisp-file-search")))
        (test-name-executable
         (string->symbol (concat (symbol->string test-name-template) "//executable-file-search")))
        (test-name-foreign
         (string->symbol (concat (symbol->string test-name-template) "//foreign-file-search")))
        (test-name-native-helper
         (string->symbol (concat (symbol->string test-name-template) "//emacs-native-helper"))))
    `(progn
       (ert-deftest ,test-name-elisp ()
         (let ((find-rec-backend 'elisp))
           ,@body))
       (ert-deftest ,test-name-executable ()
         (let ((find-rec-backend 'executable))
           ,@body))
       (when use-foreign-libraries?
         (ert-deftest ,test-name-foreign ()
           (let ((find-rec-backend 'native))
             ,@body)))
       (ert-deftest ,test-name-native-helper ()
         (when (null (foregin-setup-resolve-emacs-native-helper-path))
           (ert-skip "emacs-native-helper not available"))
         (let ((find-rec-backend 'emacs-native-helper))
           ,@body)))))

(defconst find-file-tests-tests/project-dir
  (concat +test-utils--test-root+ "/test-data/egrep"))

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

(provide 'find-files-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; find-files-tests.el ends here
