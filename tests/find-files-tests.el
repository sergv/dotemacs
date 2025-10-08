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
         (string->symbol (concat (symbol->string test-name-template) "//foreign-file-search"))))
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
             ,@body))))))

(defconst find-file-tests-tests/project-dir
  (concat +emacs-config-path+ "/tests/test-data/egrep"))

(find-files-tests--define-tests
    find-files-tests-1
  (should
   (equal (sort (-map #'test-utils--normalise-file-name
                      (find-rec* :root find-file-tests-tests/project-dir
                                 :globs-to-find '("*")))
                :lessp #'string<)
          (--map (test-utils--normalise-file-name (concat find-file-tests-tests/project-dir "/" it))
                 '("README.md"
                   "bar.c"
                   "src/foo.c")))))

(find-files-tests--define-tests
    find-files-tests-2
  (should
   (equal (sort (-map #'test-utils--normalise-file-name
                      (find-rec* :root find-file-tests-tests/project-dir
                                 :globs-to-find '("*")
                                 :relative-paths t))
                :lessp #'string<)
          (--map (test-utils--normalise-file-name it)
                 '("README.md"
                   "bar.c"
                   "src/foo.c")))))

(provide 'find-files-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; find-files-tests.el ends here
