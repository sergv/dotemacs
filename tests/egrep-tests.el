;; egrep-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 19 June 2018
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'egrep)
(require 'ert)

(defmacro grep-tests--define-tests (test-name-template regexp-var elisp-regexp foreign-regexp &rest body)
  (declare (indent 4))
  (let ((test-name-elisp
         (string->symbol (format test-name-template "elisp-engine")))
        (test-name-foreign
         (string->symbol (format test-name-template "foreign-engine"))))
    `(progn
       (ert-deftest ,test-name-elisp ()
         (let ((egrep-backend 'elisp)
               (,regexp-var ,elisp-regexp))
           ,@body))
       (when use-foreign-libraries?
         (ert-deftest ,test-name-foreign ()
           (let ((egrep-backend 'native)
                 (,regexp-var ,foreign-regexp))
             ,@body))))))

(defun egrep-tests--normalise-egrep-match (entry)
  (make-egrep-match
   (egrep-tests--normalise-file-name (egrep-match-file entry))
   (egrep-match-start-pos entry)
   (egrep-match-line entry)
   (egrep-match-column entry)
   (substring-no-properties (egrep-match-formatted-entry entry))))

(defun egrep-tests--normalise-expected-match (entry)
  (pcase egrep-backend
    (`elisp
     (make-egrep-match
      (egrep-tests--normalise-file-name (egrep-match-file entry))
      (egrep-match-start-pos entry)
      nil
      nil
      (substring-no-properties (egrep-match-formatted-entry entry))))
    (`native
     (make-egrep-match
      (egrep-tests--normalise-file-name (egrep-match-file entry))
      nil
      (egrep-match-line entry)
      (egrep-match-column entry)
      (substring-no-properties (egrep-match-formatted-entry entry))))))

(defconst egrep-tests/project-dir
  (concat +emacs-config-path+ "/tests/test-data/egrep"))

(defalias #'egrep-tests--normalise-file-name #'expand-file-name)

(grep-tests--define-tests "egrep-tests-1/%s"
    regexp
    "hello"
    "hello"
  (should
   (equal (cl-map 'vector
                  #'egrep-tests--normalise-egrep-match
                  (egrep--find-matches regexp '("*.c") nil egrep-tests/project-dir nil))
          (cl-map 'vector
                  #'egrep-tests--normalise-expected-match
                  (vector
                   (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 52 5 6 "bar.c:5:  int hello = x + y;\n")
                   (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 76 6 9 "bar.c:6:  return hello;\n")
                   (make-egrep-match (concat egrep-tests/project-dir "/src/foo.c") 26 3 5 "src/foo.c:3:void hello(char const * name)\n"))))))

(grep-tests--define-tests "egrep-tests-ignore-globs-override-search-globs/%s"
    regexp
    "hello"
    "hello"
  (should-error
   (egrep--find-matches regexp '("*.c") '("*.c") egrep-tests/project-dir nil)))

(grep-tests--define-tests "egrep-tests-ignore-file-globs-work/%s"
    regexp
    "HElLO"
    "HElLO"
  (should
   (equal (cl-map 'vector
                  #'egrep-tests--normalise-egrep-match
                  (egrep--find-matches regexp '("*.c") '("*src/*") egrep-tests/project-dir t))
          (cl-map 'vector
                  #'egrep-tests--normalise-expected-match
                  (vector
                   (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 52 5 6 "bar.c:5:  int hello = x + y;\n")
                   (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 76 6 9 "bar.c:6:  return hello;\n"))))))

(grep-tests--define-tests "egrep-tests-ignore-case-works/%s"
    regexp
    "HElLO"
    "HElLO"
  (should
   (equal (cl-map 'vector
                  #'egrep-tests--normalise-egrep-match
                  (egrep--find-matches regexp '("*.c") nil egrep-tests/project-dir t))
          (cl-map 'vector
                  #'egrep-tests--normalise-expected-match
                  (vector
                   (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 52 5 6 "bar.c:5:  int hello = x + y;\n")
                   (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 76 6 9 "bar.c:6:  return hello;\n")
                   (make-egrep-match (concat egrep-tests/project-dir "/src/foo.c") 26 3 5 "src/foo.c:3:void hello(char const * name)\n"))))))

(provide 'egrep-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; egrep-tests.el ends here
