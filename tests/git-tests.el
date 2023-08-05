;; git-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Saturday, 31 May 2014
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'ert)

(ert-deftest git-tests/patch-whitespace-only-change? ()
  "Test `patch-whitespace-only-change?' on different patches."
  (should (patch-whitespace-only-change?
           "\
 hello
- foo
+foo
 world!"))
  (should (patch-whitespace-only-change?
           "\
 hello
-\tfoo
+foo
 world!"))
  (should (patch-whitespace-only-change?
           "\
 hello
-foo\t
+foo
 world!"))
  (should (patch-whitespace-only-change?
           "\
 hello
-foo\tbar
+foo bar
 world!"))
  (should (patch-whitespace-only-change?
           "\
 hello
-foo  bar
+foo bar
 world!"))

  (should-not (patch-whitespace-only-change?
               "\
 hello
-foo
+bar
 world!")))

;; (setf git-tests/tests
;;       '(git-tests/patch-whitespace-only-change?))

;; (let ((ert-debug-on-error nil))
;;   (ert (join-lines (map (comp #'regexp-quote #'symbol->string)
;;                         git-tests/tests)
;;                    "\\|")
;;        ;; "haskell-tests/.*"
;;        )
;;   nil)

(provide 'git-tests)

;; Local Variables:
;; End:

;; git-tests.el ends here
