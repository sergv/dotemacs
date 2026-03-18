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

(cl-defmacro yafolding-tests--region-test-for-modes (&key modes name contents expected-value)
  (declare (indent nil))
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

(cl-defmacro yafolding-tests--test-buffer-contents (&key modes name action contents expected-value)
  (declare (indent nil))
  `(tests-utils--test-buffer-contents-init-only-modes
    :select-modes ,modes
    :name ,name
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    :buffer-id #'yafolding-tests--mk-buffer-id))

(yafolding-tests--region-test-for-modes
 :modes (text-mode haskell-mode haskell-ts-mode haskell-hsc-mode)
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
 :modes (text-mode haskell-mode haskell-ts-mode haskell-hsc-mode)
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
 :modes (text-mode haskell-mode haskell-ts-mode haskell-hsc-mode)
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
 :modes (text-mode haskell-mode haskell-ts-mode haskell-hsc-mode)
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
 :modes (text-mode haskell-mode haskell-ts-mode haskell-hsc-mode)
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
 :modes (text-mode haskell-mode haskell-ts-mode haskell-hsc-mode)
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
 :modes (text-mode haskell-mode haskell-ts-mode haskell-hsc-mode)
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

(yafolding-tests--test-buffer-contents
 :modes (text-mode)
 :name yafolding-tests/yafolding-go-parent-element-1a
 :action (yafolding-go-parent-element)
 :contents
 (tests-utils--multiline
  ""
  "executable alex"
  "  hs-source-dirs: src"
  "  main-is: Main.hs"
  ""
  "  build-depends:"
  "      base >= 4.9 && < 5"
  "        -- Data.List.NonEmpty _|_enters `base` at 4.9"
  "    , array"
  "    , containers"
  "    , directory"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "executable alex"
  "  hs-source-dirs: src"
  "  main-is: Main.hs"
  ""
  "  build-depends:"
  "      _|_base >= 4.9 && < 5"
  "        -- Data.List.NonEmpty enters `base` at 4.9"
  "    , array"
  "    , containers"
  "    , directory"
  ""))

(yafolding-tests--test-buffer-contents
 :modes (text-mode)
 :name yafolding-tests/yafolding-go-parent-element-1b
 :action (yafolding-go-parent-element)
 :contents
 (tests-utils--multiline
  ""
  "executable alex"
  "  hs-source-dirs: src"
  "  main-is: Main.hs"
  ""
  "  build-depends:"
  "      base >= 4.9 && < 5"
  "        _|_-- Data.List.NonEmpty enters `base` at 4.9"
  "    , array"
  "    , containers"
  "    , directory"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "executable alex"
  "  hs-source-dirs: src"
  "  main-is: Main.hs"
  ""
  "  build-depends:"
  "      _|_base >= 4.9 && < 5"
  "        -- Data.List.NonEmpty enters `base` at 4.9"
  "    , array"
  "    , containers"
  "    , directory"
  ""))

(yafolding-tests--test-buffer-contents
 :modes (text-mode)
 :name yafolding-tests/yafolding-go-parent-element-1c
 :action (yafolding-go-parent-element)
 :contents
 (tests-utils--multiline
  ""
  "executable alex"
  "  hs-source-dirs: src"
  "  main-is: Main.hs"
  ""
  "  build-depends:"
  "      _|_base >= 4.9 && < 5"
  "        -- Data.List.NonEmpty enters `base` at 4.9"
  "    , array"
  "    , containers"
  "    , directory"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "executable alex"
  "  hs-source-dirs: src"
  "  main-is: Main.hs"
  ""
  "  _|_build-depends:"
  "      base >= 4.9 && < 5"
  "        -- Data.List.NonEmpty enters `base` at 4.9"
  "    , array"
  "    , containers"
  "    , directory"
  ""))

(yafolding-tests--test-buffer-contents
 :modes (text-mode)
 :name yafolding-tests/yafolding-go-parent-element-1d
 :action (yafolding-go-parent-element)
 :contents
 (tests-utils--multiline
  ""
  "executable alex"
  "  hs-source-dirs: src"
  "  main-is: Main.hs"
  ""
  "  _|_build-depends:"
  "      base >= 4.9 && < 5"
  "        -- Data.List.NonEmpty enters `base` at 4.9"
  "    , array"
  "    , containers"
  "    , directory"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "_|_executable alex"
  "  hs-source-dirs: src"
  "  main-is: Main.hs"
  ""
  "  build-depends:"
  "      base >= 4.9 && < 5"
  "        -- Data.List.NonEmpty enters `base` at 4.9"
  "    , array"
  "    , containers"
  "    , directory"
  ""))

(yafolding-tests--test-buffer-contents
 :modes (text-mode)
 :name yafolding-tests/yafolding-go-parent-element-1e
 :action (yafolding-go-parent-element)
 :contents
 (tests-utils--multiline
  ""
  "-- foo"
  ""
  "_|_executable alex"
  "  hs-source-dirs: src"
  "  main-is: Main.hs"
  ""
  "  build-depends:"
  "      base >= 4.9 && < 5"
  "        -- Data.List.NonEmpty enters `base` at 4.9"
  "    , array"
  "    , containers"
  "    , directory"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "_|_-- foo"
  ""
  "executable alex"
  "  hs-source-dirs: src"
  "  main-is: Main.hs"
  ""
  "  build-depends:"
  "      base >= 4.9 && < 5"
  "        -- Data.List.NonEmpty enters `base` at 4.9"
  "    , array"
  "    , containers"
  "    , directory"
  ""))

(provide 'yafolding-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; yafolding-tests.el ends here
