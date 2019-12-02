;; rust-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 29 November 2019
;; Description:

(require 'common)
(require 'ert)
(require 'tests-utils)

(require 'rust-setup)
(require 'rust-smart-operators)
(require 'smartparens-setup)

(defmacro rust-tests--test-buffer-contents (action contents expected-value)
  (declare (indent 1))
  `(tests-utils--test-buffer-contents
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    :initialisation (rust-mode)
    :buffer-id rust))

(ert-deftest rust-tests/sp-newline--expand-braced-block-1 ()
  (rust-tests--test-buffer-contents
   (progn
     (sp-newline))
   (tests-utils--multiline
    ""
    "fn foo() {_|_}"
    "")
   (tests-utils--multiline
    ""
    "fn foo() {"
    "    _|_"
    "}"
    "")))

(ert-deftest rust-tests/sp-newline--expand-braced-block-2 ()
  (rust-tests--test-buffer-contents
   (progn
     (sp-newline))
   (tests-utils--multiline
    ""
    "fn foo() {_|_     }"
    "")
   (tests-utils--multiline
    ""
    "fn foo() {"
    "    _|_"
    "}"
    "")))

(ert-deftest rust-tests/sp-newline--expand-braced-block-3 ()
  (rust-tests--test-buffer-contents
   (progn
     (sp-newline))
   (tests-utils--multiline
    ""
    "fn foo() {             _|_}"
    "")
   (tests-utils--multiline
    ""
    "fn foo() {"
    "    _|_"
    "}"
    "")))

(ert-deftest rust-tests/sp-newline--expand-braced-block-4 ()
  (rust-tests--test-buffer-contents
   (progn
     (sp-newline))
   (tests-utils--multiline
    ""
    "fn foo() {             _|_           }"
    "")
   (tests-utils--multiline
    ""
    "fn foo() {"
    "    _|_"
    "}"
    "")))


(ert-deftest rust-tests/sp-newline--duplication-of-commented-line-1 ()
  (rust-tests--test-buffer-contents
   (progn
     (sp-newline))
   (tests-utils--multiline
    ""
    "// foobar _|_"
    ""
    "fn foo() {}"
    "")
   (tests-utils--multiline
    ""
    "// foobar"
    "// _|_"
    ""
    "fn foo() {}"
    "")))

(ert-deftest rust-tests/sp-newline--duplication-of-commented-line-3 ()
  (rust-tests--test-buffer-contents
   (progn
     (sp-newline))
   (tests-utils--multiline
    ""
    "// foobar _|_ quux"
    ""
    "fn foo() {}"
    "")
   (tests-utils--multiline
    ""
    "// foobar"
    "// _|_quux"
    ""
    "fn foo() {}"
    "")))


(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-1 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 +_|_2"
    "x = 1 ++ _|_2"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-2 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 +           _|_ 2"
    "x = 1 ++_|_ 2"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-3 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?*)
    "x = 1 +           _|_"
    "x = 1 +* _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-4 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1_|_"
    "x = 1 + _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-5 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 _|_"
    "x = 1 + _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-6 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1  _|_"
    "x = 1  + _|_"))

(ert-deftest rust-tests/rust-smart-operators--prepend-to-prev-operator-7 ()
  (rust-tests--test-buffer-contents
      (rust-smart-operators--insert-char-surrounding-with-spaces ?=)
    "x = 1 ! _|_"
    "x = 1 != _|_"))

(ert-deftest rust-tests/sp-splice-sexp-killing-backward-1 ()
  (rust-tests--test-buffer-contents
      (sp-splice-sexp-killing-backward)
    "foo = ({ foo: _|_ a, b})"
    "foo = (_|_a, b)"))

(provide 'rust-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; rust-tests.el ends here
