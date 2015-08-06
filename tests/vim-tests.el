;; vim-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  6 August 2015
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'vim-search)
(require 'ert)

(ert-deftest vim-tests/test-vim:parse-substitute-lowlevel ()
  (should (equal (vim:parse-substitute-lowlevel "/foo/bar")
                 '("foo" "bar" nil)))
  (should (equal (vim:parse-substitute-lowlevel ",foo,bar")
                 '("foo" "bar" nil)))
  (should (equal (vim:parse-substitute-lowlevel ",foo,bar,")
                 '("foo" "bar" nil)))
  (should (equal (vim:parse-substitute-lowlevel "/hello/world/gI")
                 '("hello" "world" "gI")))
  (should (equal (vim:parse-substitute-lowlevel "/\"/\"/")
                 '("\"" "\"" nil)))
  (should (equal (vim:parse-substitute-lowlevel "         /    a  ")
                 '("    a  " nil nil)))
  (should (equal (vim:parse-substitute-lowlevel "/xyz")
                 '("xyz" nil nil)))
  (should (equal (vim:parse-substitute-lowlevel ";xyz")
                 '("xyz" nil nil)))
  (should (equal (vim:parse-substitute-lowlevel "/x/y/gic   uuuu")
                 '("x" "y" "gic")))
  (should (equal (vim:parse-substitute-lowlevel "/abc/\\/\\/\\/\\//g")
                 '("abc" "\\/\\/\\/\\/" "g")))
  (should (equal (vim:parse-substitute-lowlevel "/^\\(.*\\)$/\\\"\\1\\\"\\n/g")
                 '("^\\(.*\\)$" "\\\"\\1\\\"\\n" "g")))
  (should (equal (vim:parse-substitute-lowlevel ",hello ,/world ! world;/,g")
                 '("hello " "/world ! world;/" "g")))
  (should (equal (vim:parse-substitute-lowlevel "/")
                 '("" nil nil)))
  (should (equal (vim:parse-substitute-lowlevel "/[A-Z]/foo/g")
                 '("[A-Z]" "foo" "g")))
  (should (equal (vim:parse-substitute-lowlevel "/hello/world\\/g")
                 '("hello" "world\\/g" nil))))

(ert "vim-tests/.*")

(provide 'vim-tests)

;; Local Variables:
;; End:

;; vim-tests.el ends here
