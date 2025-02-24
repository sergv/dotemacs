;; abbrev-plus-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 17 January 2022
;; Description:

(require 'ert)

(require 'abbrev+)

(defun abbrev+-tests/normalize-string-list (xs)
  (-sort #'string< xs))

(ert-deftest abbrev+-tests/make-abbrev+-prefixes-1 ()
  (should (equal (abbrev+-tests/normalize-string-list
                  (make-abbrev+-prefixes "foobar" 2))
                 (abbrev+-tests/normalize-string-list
                  '("fo"
                    "foo"
                    "foob"
                    "fooba"
                    "foobar")))))

(defconst make-abbrev+-triggers-for-func-name-1--expected
  '("aaa-bc-x-quuz"
    "aaabc-x-quuz"
    "aaa-b-x-quuz"
    "aaab-x-quuz"
    "aaa-bcx-quuz"
    "aaabcx-quuz"
    "aaa-bx-quuz"
    "aaabx-quuz"
    "aaa-bc--quuz"
    "aaabc--quuz"
    "aaa-b--quuz"
    "aaab--quuz"
    "aaa-bc-quuz"
    "aaabc-quuz"
    "aaa-b-quuz"
    "aaab-quuz"
    "aaa-bc-xquuz"
    "aaabc-xquuz"
    "aaa-b-xquuz"
    "aaab-xquuz"
    "aaa-bcxquuz"
    "aaabcxquuz"
    "aaa-bxquuz"
    "aaabxquuz"
    "aaa-bc-quuz"
    "aaabc-quuz"
    "aaa-b-quuz"
    "aaab-quuz"
    "aaa-bcquuz"
    "aaabcquuz"
    "aaa-bquuz"
    "aaabquuz"
    "aaa-bc-x-quu"
    "aaabc-x-quu"
    "aaa-b-x-quu"
    "aaab-x-quu"
    "aaa-bcx-quu"
    "aaabcx-quu"
    "aaa-bx-quu"
    "aaabx-quu"
    "aaa-bc--quu"
    "aaabc--quu"
    "aaa-b--quu"
    "aaab--quu"
    "aaa-bc-quu"
    "aaabc-quu"
    "aaa-b-quu"
    "aaab-quu"
    "aaa-bc-xquu"
    "aaabc-xquu"
    "aaa-b-xquu"
    "aaab-xquu"
    "aaa-bcxquu"
    "aaabcxquu"
    "aaa-bxquu"
    "aaabxquu"
    "aaa-bc-quu"
    "aaabc-quu"
    "aaa-b-quu"
    "aaab-quu"
    "aaa-bcquu"
    "aaabcquu"
    "aaa-bquu"
    "aaabquu"))

(ert-deftest abbrev+-tests/make-abbrev+-triggers-for-func-name-1 ()
  (should (equal (abbrev+-tests/normalize-string-list
                  (make-abbrev+-triggers-for-func-name
                   '("-" "")
                   '(("aaa")
                     ("bcdef" 1 2)
                     ("xxx" 0 1)
                     ("quuz" 3))))
                 (abbrev+-tests/normalize-string-list
                  make-abbrev+-triggers-for-func-name-1--expected))))

(provide 'abbrev-plus-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; abbrev-plus-tests.el ends here
