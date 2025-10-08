;; egrep-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 19 June 2018
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'set-up-platform))

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
   (egrep-tests--normalise-file-name (egrep-match-file entry) t)
   (egrep-tests--normalise-file-name (egrep-match-short-file-name entry) nil)
   (egrep-match-line entry)
   (egrep-match-column entry)
   (substring-no-properties (egrep-match-matched-prefix entry))
   (substring-no-properties (egrep-match-matched-text entry))
   (substring-no-properties (egrep-match-matched-suffix entry))
   (egrep-match-offset entry)))

(defconst egrep-tests/project-dir
  (concat +emacs-config-path+ "/tests/test-data/egrep"))

(defun egrep-tests--normalise-file-name (x expand?)
  (funcall
   (fold-platform-os-type
    #'identity
    #'downcase)
   (funcall (if expand? #'expand-file-name #'identity) x)))

(defconst egrep-tests--bar-matches
  (vector
   (make-egrep-match (concat egrep-tests/project-dir "/bar.c")     "bar.c"     5 6 "  int " "hello" " = x + y;" 52)
   (make-egrep-match (concat egrep-tests/project-dir "/bar.c")     "bar.c"     6 9 "  return " "hello" ";" 76)))

(defconst egrep-tests--foo-matches
  (vector
   (make-egrep-match (concat egrep-tests/project-dir "/src/foo.c") "src/foo.c" 3 5 "void " "hello" "(char const * name)" 26)))

(grep-tests--define-tests "egrep-tests-1/%s"
    regexp
    "hello"
    "hello"
  (should
   (equal (cl-map 'vector
                  #'egrep-tests--normalise-egrep-match
                  (egrep--find-matches regexp '("*.c") nil egrep-tests/project-dir nil nil))
          (cl-map 'vector
                  #'egrep-tests--normalise-egrep-match
                  (vconcat egrep-tests--bar-matches egrep-tests--foo-matches)))))

(grep-tests--define-tests "egrep-tests-ignore-globs-override-search-globs/%s"
    regexp
    "hello"
    "hello"
  (should-error
   (egrep--find-matches regexp '("*.c") '("*.c") egrep-tests/project-dir nil nil)))

(grep-tests--define-tests "egrep-tests-ignore-file-globs-work/%s"
    regexp
    "HElLO"
    "HElLO"
  (should
   (equal (cl-map 'vector
                  #'egrep-tests--normalise-egrep-match
                  (egrep--find-matches regexp '("*.c") '("*src/*") egrep-tests/project-dir t nil))
          (cl-map 'vector
                  #'egrep-tests--normalise-egrep-match
                  egrep-tests--bar-matches))))

(grep-tests--define-tests "egrep-tests-ignore-case-works/%s"
    regexp
    "HElLO"
    "HElLO"
  (should
   (equal (cl-map 'vector
                  #'egrep-tests--normalise-egrep-match
                  (egrep--find-matches regexp '("*.c") nil egrep-tests/project-dir t nil))
          (cl-map 'vector
                  #'egrep-tests--normalise-egrep-match
                  (vconcat egrep-tests--bar-matches egrep-tests--foo-matches)))))

(provide 'egrep-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; egrep-tests.el ends here
