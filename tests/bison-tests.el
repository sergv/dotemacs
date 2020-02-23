;; bison-mode-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 23 February 2020
;; Description:

(require 'common)
(require 'ert)
(require 'tests-utils)

(require 'bison-mode)

(defmacro bison-tests--test-buffer-contents (action contents expected-value)
  (declare (indent 1))
  `(tests-utils--test-buffer-contents
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    :initialisation (bison-mode)
    :buffer-id bison))

(ert-deftest rust-tests/rust-smart-operators--arrow-2 ()
  (rust-tests--test-buffer-contents
      (progn
        (rust-smart-operators--insert-char-surrounding-with-spaces ?\-)
        (rust-smart-operators--insert-char-surrounding-with-spaces ?\>))
    "foo(x: i32)_|_"
    "foo(x: i32) -> _|_"))

(provide 'bison-mode-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; bison-mode-tests.el ends here
