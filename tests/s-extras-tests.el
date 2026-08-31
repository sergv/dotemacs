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

(ert-deftest s-extras-tests/s-extras-replace-char-1 ()
  (should (equal (s-extras-replace-char ?o ?a "foo")
                 "faa")))

(ert-deftest s-extras-tests/s-extras-replace-char-2 ()
  (should (equal (s-extras-replace-char ?a ?b "foo")
                 "foo")))

(ert-deftest s-extras-tests/s-extras-replace-char-3 ()
  (should (equal (s-extras-replace-char ?a ?b "")
                 "")))

(ert-deftest s-extras-tests/s-extras-replace-char-4 ()
  (should (equal (s-extras-replace-char ?a ?b "a")
                 "b")))

(ert-deftest s-extras-tests/s-extras-replace-char-5 ()
  (should (equal (s-extras-replace-char ?a ?b "aa")
                 "bb")))

(ert-deftest s-extras-tests/s-extras-replace-char-6 ()
  (should (equal (s-extras-replace-char ?т
                                         ?в
                                         (s-extras-replace-char ?в
                                                                 ?п
                                                                 (copy-sequence "привет")))
                 "припев")))

(ert-deftest s-extras-tests/strip-terminal-save-restore-cursor-escape-sequences-1 ()

  (should (equal (s-extras--strip-terminal-save-restore-cursor-escape-sequences "")
                 ""))

  (should (equal (s-extras--strip-terminal-save-restore-cursor-escape-sequences "x")
                 "x"))

  (should (equal (s-extras--strip-terminal-save-restore-cursor-escape-sequences "foo")
                 "foo"))

  (should (equal (s-extras--strip-terminal-save-restore-cursor-escape-sequences "foo7bar89")
                 "foobar9")))

(ert-deftest s-extras-tests/strip-terminal-save-restore-cursor-escape-sequences!-1 ()

  (should (equal (s-extras--strip-terminal-save-restore-cursor-escape-sequences! (copy-sequence ""))
                 ""))

  (should (equal (s-extras--strip-terminal-save-restore-cursor-escape-sequences! (copy-sequence "x"))
                 "x"))

  (should (equal (s-extras--strip-terminal-save-restore-cursor-escape-sequences! (copy-sequence "foo"))
                 "foo"))

  (should (equal (s-extras--strip-terminal-save-restore-cursor-escape-sequences! (copy-sequence "foo7bar89"))
                 "foobar9")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-1a ()
  (should (equal (s-extras-expand-escape-sequences "abc")
                 "abc")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-1b ()
  (should (equal (s-extras-expand-escape-sequences "a")
                 "a")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-1c ()
  (should (equal (s-extras-expand-escape-sequences "")
                 "")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-2 ()
  (should (equal (s-extras-expand-escape-sequences "ab\\nc")
                 "ab\nc")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-3 ()
  (should (equal (s-extras-expand-escape-sequences "abc\\n")
                 "abc\n")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-4 ()
  (should (equal (s-extras-expand-escape-sequences "abc\\n\\n")
                 "abc\n\n")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-5 ()
  (should (equal (s-extras-expand-escape-sequences "\\nabc")
                 "\nabc")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-6 ()
  (should (equal (s-extras-expand-escape-sequences "\\n\\nabc")
                 "\n\nabc")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-7 ()
  (should (equal (s-extras-expand-escape-sequences "ab\\\\c")
                 "ab\\c")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-8 ()
  (should (equal (s-extras-expand-escape-sequences "ab\\tc")
                 "ab\tc")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-9 ()
  (should (equal (s-extras-expand-escape-sequences "ab\\rc")
                 "ab\rc")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-10 ()
  (should (equal (s-extras-expand-escape-sequences "\\(foo\\|bar\\)")
                 "\\(foo\\|bar\\)")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-11 ()
  (should (equal (s-extras-expand-escape-sequences "abc\\")
                 "abc\\")))

(ert-deftest s-extras-tests/s-extras-expand-escape-sequences-12 ()
  ;; Test that we don’t do anything too funny with Unicode that Emacs won’t like.
  ;; E.g. Emacs 31 started disallowing assigning multibyte characters into stings
  ;; so previous implemenation that was reassinging character-by-character
  ;; stopped working.
  (should (equal (s-extras-expand-escape-sequences "foo’bar")
                 "foo’bar")))

(provide 's-extras-tests)

;; Local Variables:
;; End:

;; s-extras-tests.el ends here
