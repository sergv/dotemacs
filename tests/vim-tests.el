;; vim-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  6 August 2015
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'tests-utils)

(require 'vim)
(require 'vim-search)
(require 'ert)

(defmacro vim-tests--test-fresh-buffer-contents (action contents expected-value)
  (declare (indent 1))
  `(tests-utils--test-buffer-contents
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    :initialisation (text-mode)
    ;; Don’t reuse buffer to start out in fresh environment each time and don’t
    ;; share things like last cmd events, etc.
    :buffer-id nil))

(ert-deftest vim-tests/test-vim--parse-substitute-pattern-repl-flags ()
  (should (equal (vim--parse-substitute-pattern-repl-flags "/foo/bar")
                 '("foo" "bar" nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags ",foo,bar")
                 '("foo" "bar" nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags ",foo,bar,")
                 '("foo" "bar" nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/hello/world/gI")
                 '("hello" "world" "gI")))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/\"/\"/")
                 '("\"" "\"" nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags "         /    a  ")
                 '("    a  " nil nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/xyz")
                 '("xyz" nil nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags ";xyz")
                 '("xyz" nil nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/x/y/gic   uuuu")
                 '("x" "y" "gic")))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/abc/\\/\\/\\/\\//g")
                 '("abc" "\\/\\/\\/\\/" "g")))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/^\\(.*\\)$/\\\"\\1\\\"\\n/g")
                 '("^\\(.*\\)$" "\\\"\\1\\\"\\n" "g")))
  (should (equal (vim--parse-substitute-pattern-repl-flags ",hello ,/world ! world;/,g")
                 '("hello " "/world ! world;/" "g")))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/")
                 '("" nil nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/[A-Z]/foo/g")
                 '("[A-Z]" "foo" "g")))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/hello/world\\/g")
                 '("hello" "world\\/g" nil))))

(ert-deftest vim-tests/vim:regex-without-case ()
  (should (equal (vim:regex-without-case "foobar")
                 "foobar"))
  (should (equal (vim:regex-without-case "foo\\bar")
                 "foo\\bar"))
  (should (equal (vim:regex-without-case "foo\\.bar")
                 "foo\\.bar"))
  (should (equal (vim:regex-without-case "foobar\\")
                 "foobar\\"))
  (should (equal (vim:regex-without-case "foobar\\x")
                 "foobar\\x"))
  (should (equal (vim:regex-without-case "foobar\\c")
                 "foobar"))
  (should (equal (vim:regex-without-case "\\Cfoobar\\c")
                 "foobar")))

(ert-deftest vim-tests/dd-1 ()
  (vim-tests--test-fresh-buffer-contents
      (execute-kbd-macro (kbd ",,"))
    (tests-utils--multiline
     ""
     ""
     "(foo"
     "   (bar (baz "
     "         _|_  quux  )))"
     "")
    (tests-utils--multiline
     ""
     ""
     "(foo"
     "   (bar (baz "
     "_|_")))

(ert-deftest vim-tests/repeat-vim:sp-splice-sexp-killing-backward-1 ()
  (vim-tests--test-fresh-buffer-contents
      (execute-kbd-macro (kbd "j ( d"))
    (tests-utils--multiline
     ""
     ""
     "(foo"
     "   (bar (baz "
     "         _|_  quux  )))"
     "")
    (tests-utils--multiline
     ""
     ""
     "(foo"
     "(bar _|_quux))"
     "")))

(ert-deftest vim-tests/repeat-vim:sp-splice-sexp-killing-backward-2 ()
  (vim-tests--test-fresh-buffer-contents
      (execute-kbd-macro (kbd "j ( d ."))
    (tests-utils--multiline
     ""
     ""
     "(foo"
     "   (bar (baz "
     "         _|_  quux  )))"
     "")
    (tests-utils--multiline
     ""
     ""
     "(foo"
     "_|_quux)"
     "")))

(ert-deftest vim-tests/repeat-vim:sp-splice-sexp-killing-backward-3 ()
  (vim-tests--test-fresh-buffer-contents
      (execute-kbd-macro (kbd "j ( d 2 ."))
    (tests-utils--multiline
     ""
     ""
     "(foo"
     "   (bar (baz "
     "         _|_  quux  )))"
     "")
    (tests-utils--multiline
     ""
     ""
     "_|_quux"
     "")))

(ert-deftest vim-tests/rectangular-insert-1 ()
  (vim-tests--test-fresh-buffer-contents
      (execute-kbd-macro (kbd "d d C-v h h h I 1 2 3 <escape>"))
    (tests-utils--multiline
     "fo_|_o"
     "bar"
     "baz"
     "quux"
     "fizz"
     "frobnicate")
    (tests-utils--multiline
     "12_|_3foo"
     "123bar"
     "123baz"
     "123quux"
     "fizz"
     "frobnicate")))

(ert-deftest vim-tests/rectangular-insert-undo-1 ()
  (vim-tests--test-fresh-buffer-contents
      ;; Enable undo tracking.
      (let ((buffer-undo-list nil))
        (execute-kbd-macro (kbd "d d C-v h h h I 1 2 3 <escape> k")))
    (tests-utils--multiline
     "fo_|_o"
     "bar"
     "baz"
     "quux"
     "fizz"
     "frobnicate")
    (tests-utils--multiline
     "_|_foo"
     "bar"
     "baz"
     "quux"
     "fizz"
     "frobnicate")))

(ert-deftest vim-tests/rectangular-insert-undo-redo-1 ()
  (vim-tests--test-fresh-buffer-contents
      ;; Enable undo tracking.
      (let ((buffer-undo-list nil))
        (execute-kbd-macro (kbd "d d C-v h h h I 1 2 3 <escape> k K")))
    (tests-utils--multiline
     "fo_|_o"
     "bar"
     "baz"
     "quux"
     "fizz"
     "frobnicate")
    (tests-utils--multiline
     "123foo"
     "123bar"
     "123baz"
     "_|_123quux"
     "fizz"
     "frobnicate")))

;; (ert "vim-tests/.*")

(provide 'vim-tests)

;; Local Variables:
;; End:

;; vim-tests.el ends here
