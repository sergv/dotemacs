;; s-extras-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  6 February 2022
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 's-extras)

(require 'ert)

(ert-deftest s-extras-tests/count-chars-in-string-1 ()
  (should (equal (s-extras-count-chars-in-string ?\n "foobar")
                 0)))

(ert-deftest s-extras-tests/count-chars-in-string-2 ()
  (should (equal (s-extras-count-chars-in-string ?o "foobar")
                 2)))

(ert-deftest s-extras-tests/count-chars-in-string-3 ()
  (should (equal (s-extras-count-chars-in-string ?r "foobar")
                 1)))

(ert-deftest s-extras-tests/count-chars-in-string-4 ()
  (should (equal (s-extras-count-chars-in-string ?f "foobar")
                 1)))

(ert-deftest s-extras-tests/count-chars-in-string-5 ()
  (should (equal (s-extras-count-chars-in-string ?в "приветв")
                 2)))

(ert-deftest s-extras-tests/s-extras-replace-char!-1 ()
  (should (equal (s-extras-replace-char! ?o ?a (copy-sequence "foo"))
                 "faa")))

(ert-deftest s-extras-tests/s-extras-replace-char!-2 ()
  (should (equal (s-extras-replace-char! ?a ?b (copy-sequence "foo"))
                 "foo")))

(ert-deftest s-extras-tests/s-extras-replace-char!-3 ()
  (should (equal (s-extras-replace-char! ?a ?b (copy-sequence ""))
                 "")))

(ert-deftest s-extras-tests/s-extras-replace-char!-4 ()
  (should (equal (s-extras-replace-char! ?a ?b (copy-sequence "a"))
                 "b")))

(ert-deftest s-extras-tests/s-extras-replace-char!-5 ()
  (should (equal (s-extras-replace-char! ?a ?b (copy-sequence "aa"))
                 "bb")))

(ert-deftest s-extras-tests/s-extras-replace-char!-6 ()
  (should (equal (s-extras-replace-char! ?т
                                         ?в
                                         (s-extras-replace-char! ?в
                                                                 ?п
                                                                 (copy-sequence "привет")))
                 "припев")))

(provide 's-extras-tests)

;; Local Variables:
;; End:

;; s-extras-tests.el ends here
