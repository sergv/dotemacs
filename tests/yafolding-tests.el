;; yafolding-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  3 December 2024
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'dash)
(require 'tests-utils)

(require 'vim)
(require 'vim-search)
(require 'ert)

(defun yafolding-tests--mk-buffer-id (mode)
  (cl-assert (symbolp mode) nil "Invalid mode: %s" mode)
  (string->symbol (concat "yafolding-tests-" (symbol->string mode))))

(cl-defmacro yafolding-tests--region-test-for-modes (&key modes name action contents expected-value)
  `(tests-utils--test-buffer-contents-init-only-modes
    :select-modes ,modes
    :name ,name
    :action
    (let ((bounds (yafolding-get-element-region)))
      (save-excursion
        (goto-char (cdr bounds))
        (insert "YYY")
        (goto-char (car bounds))
        (insert "XXX")))
    :contents ,contents
    :expected-value ,expected-value
    :buffer-id #'yafolding-tests--mk-buffer-id))

(yafolding-tests--region-test-for-modes
 :modes (text-mode haskell-mode haskell-ts-mode)
 :name yafolding-tests/bounds-1
 :contents
 (tests-utils--multiline
  ""
  "foo x = bar (x * x)"
  "  _|_where"
  "    bar y = y + y"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo x = bar (x * x)"
  "  _|_whereXXX"
  "    bar y = y + yYYY"
  ""))

(yafolding-tests--region-test-for-modes
 :modes (text-mode haskell-mode haskell-ts-mode)
 :name yafolding-tests/bounds-2aa
 :contents
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_where"
  "    bar y = y + y"
  "    baz z = z + z + z"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_whereXXX"
  "    bar y = y + y"
  "    baz z = z + z + zYYY"
  ""))

(yafolding-tests--region-test-for-modes
 :modes (text-mode haskell-mode haskell-ts-mode)
 :name yafolding-tests/bounds-2ab
 :contents
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_where"
  "    bar y = y + y"
  "    baz z = z + z + z")
 :expected-value
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_whereXXX"
  "    bar y = y + y"
  "    baz z = z + z + zYYY"))

(yafolding-tests--region-test-for-modes
 :modes (text-mode haskell-mode haskell-ts-mode)
 :name yafolding-tests/bounds-2ba
 :contents
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_where"
  "    bar y = y + y"
  ""
  "    baz z = z + z + z"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_whereXXX"
  "    bar y = y + y"
  ""
  "    baz z = z + z + zYYY"
  ""))

(yafolding-tests--region-test-for-modes
 :modes (text-mode haskell-mode haskell-ts-mode)
 :name yafolding-tests/bounds-2bb
 :contents
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_where"
  "    bar y = y + y"
  ""
  "    baz z = z + z + z")
 :expected-value
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_whereXXX"
  "    bar y = y + y"
  ""
  "    baz z = z + z + zYYY"))

(yafolding-tests--region-test-for-modes
 :modes (text-mode haskell-mode haskell-ts-mode)
 :name yafolding-tests/bounds-2ca
 :contents
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_where"
  "    bar y = y + y"
  "    "
  "    baz z = z + z + z"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_whereXXX"
  "    bar y = y + y"
  "    "
  "    baz z = z + z + zYYY"
  ""))

(yafolding-tests--region-test-for-modes
 :modes (text-mode haskell-mode haskell-ts-mode)
 :name yafolding-tests/bounds-2cb
 :contents
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_where"
  "    bar y = y + y"
  "    "
  "    baz z = z + z + z")
 :expected-value
 (tests-utils--multiline
  ""
  "foo x = bar (x * baz x)"
  "  _|_whereXXX"
  "    bar y = y + y"
  "    "
  "    baz z = z + z + zYYY"))

(provide 'yafolding-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; yafolding-tests.el ends here
