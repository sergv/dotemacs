;; text-property-utils-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 24 February 2025
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'text-property-utils)

(require 'ert)

(ert-deftest text-property-utils-tests/position-ranges--add-range ()
  (let ((ranges (text-property-utils--empty-position-ranges)))
    (text-property-utils--position-ranges--add-range! 1 5 ranges)
    (text-property-utils--position-ranges--add-range! 5 10 ranges)
    (text-property-utils--position-ranges--add-range! 15 20 ranges)
    (text-property-utils--position-ranges--add-range! 25 27 ranges)
    (text-property-utils--position-ranges--add-range! 27 29 ranges)
    (should (equal (text-property-ranges/ranges ranges)
                   '((25 . 29) (15 . 20) (1 . 10))))))

(ert-deftest persistent-sessions-tests/text-property-utils-get-all-text-properties-in-string ()
  (let ((test-string "foo bar"))
    (put-text-property 0 1 'foo 'bar test-string)
    (put-text-property 4 6 'quux 'baz test-string)
    (let ((props (text-property-utils-get-all-text-properties-in-string test-string nil)))
      (should (listp props))
      (should (= 2 (length props)))
      (should (equal (car props) '(quux baz ((4 . 6)))))
      (should (equal (cadr props) '(foo bar ((0 . 1))))))))

(ert-deftest persistent-sessions-tests/text-property-utils-get-all-text-properties-in-string-2 ()
  (let ((test-string "foo bar"))
    (put-text-property 0 7 'foo 'bar test-string)
    (put-text-property 4 7 'quux 'baz test-string)
    (let ((props (text-property-utils-get-all-text-properties-in-string test-string nil)))
      (should (listp props))
      (should (= 2 (length props)))
      (should (equal (car props) '(quux baz ((4 . 7)))))
      (should (equal (cadr props) '(foo bar ((0 . 7))))))))

(ert-deftest persistent-sessions-tests/text-property-utils-get-all-text-properties-in-string-3 ()
  (let ((test-string "foo bar"))
    (put-text-property 0 7 'foo 'bar test-string)
    (put-text-property 4 7 'quux 'baz test-string)
    (let ((props (text-property-utils-get-all-text-properties-in-string test-string '(foo))))
      (should (listp props))
      (should (= 1 (length props)))
      (should (equal (car props) '(quux baz ((4 . 7))))))))

(provide 'text-property-utils-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; text-property-utils-tests.el ends here
