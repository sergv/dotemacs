;; v-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 21 February 2020
;; Description:

(require 'v)

(ert-deftest v/v--find ()
  (let ((xs [1 2 3 4 5]))
    (should (equal 3
                   (v--find (< 2 it) xs)))
    (should (equal 5
                   (v--find (= 5 it) xs)))
    (should (equal nil
                   (v--find (= 6 it) xs)))))

(ert-deftest v/v--assq ()
  (let ((xs (vector (cons 'x 1) (cons 'y "foo") (cons 'z nil))))
    (should (equal (cons 'x 1)
                   (v-assq 'x xs)))
    (should (equal (cons 'y "foo")
                   (v-assq 'y xs)))
    (should (equal (cons 'z nil)
                   (v-assq 'z xs)))
    (should (equal nil
                   (v-assq 'w xs)))))

(ert "v/.*")

;; Local Variables:
;; End:

;; v-tests.el ends here