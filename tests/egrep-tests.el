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

(defconst egrep-tests/project-dir
  (concat +emacs-config-path+ "/tests/test-data/egrep"))

(defalias #'egrep-tests--normalise-file-name #'expand-file-name)

(grep-tests--define-tests "egrep-tests-1/%s"
    regexp
    "hello"
    "hello"
  (should
   (equal (cl-map 'vector
                  (lambda (entry)
                    (make-egrep-match
                     (egrep-tests--normalise-file-name (egrep-match-file entry))
                     (egrep-match-start-pos entry)
                     (substring-no-properties (egrep-match-formatted-entry entry))))
                  (egrep--find-matches regexp '("*.c") nil egrep-tests/project-dir nil))
          (vector
           (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 52 "bar.c:5:  int hello = x + y;\n")
           (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 76 "bar.c:6:  return hello;\n")
           (make-egrep-match (concat egrep-tests/project-dir "/src/foo.c") 26 "src/foo.c:3:void hello(char const * name)\n")))))

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
                  (lambda (entry)
                    (make-egrep-match
                     (egrep-tests--normalise-file-name (egrep-match-file entry))
                     (egrep-match-start-pos entry)
                     (substring-no-properties (egrep-match-formatted-entry entry))))
                  (egrep--find-matches regexp '("*.c") '("*src/*") egrep-tests/project-dir t))
          (vector
           (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 52 "bar.c:5:  int hello = x + y;\n")
           (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 76 "bar.c:6:  return hello;\n")))))

(grep-tests--define-tests "egrep-tests-ignore-case-works/%s"
    regexp
    "HElLO"
    "HElLO"
  (should
   (equal (cl-map 'vector
                  (lambda (entry)
                    (make-egrep-match
                     (egrep-tests--normalise-file-name (egrep-match-file entry))
                     (egrep-match-start-pos entry)
                     (substring-no-properties (egrep-match-formatted-entry entry))))
                  (egrep--find-matches regexp '("*.c") nil egrep-tests/project-dir t))
          (vector
           (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 52 "bar.c:5:  int hello = x + y;\n")
           (make-egrep-match (concat egrep-tests/project-dir "/bar.c") 76 "bar.c:6:  return hello;\n")
           (make-egrep-match (concat egrep-tests/project-dir "/src/foo.c") 26 "src/foo.c:3:void hello(char const * name)\n")))))

(provide 'egrep-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; egrep-tests.el ends here
