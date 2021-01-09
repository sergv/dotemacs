;; v-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <sergey@debian>
;; Created: 21 February 2020
;; Description:

(require 'v)

(ert-deftest v/v-find ()
  (let ((xs [1 2 3 4 5]))
    (should (equal 3
                   (v--find (< 2 it) xs)))
    (should (equal 5
                   (v--find (= 5 it) xs)))
    (should (equal nil
                   (v--find (= 6 it) xs)))))

(ert-deftest v/v-assq ()
  (let ((xs (vector (cons 'x 1) (cons 'y "foo") (cons 'z nil))))
    (should (equal (cons 'x 1)
                   (v-assq 'x xs)))
    (should (equal (cons 'y "foo")
                   (v-assq 'y xs)))
    (should (equal (cons 'z nil)
                   (v-assq 'z xs)))
    (should (equal nil
                   (v-assq 'w xs)))))

(ert-deftest v/v-member ()
  (let ((xs (vector (cons 'x 1) (cons 'y "foo") (cons 'z nil) "foo" "bar" "baz")))
    (should-not (v-member 'x xs))
    (should-not (v-member 'y xs))
    (should-not (v-member 'z xs))
    (should-not (v-member 'w xs))

    (should (v-member "foo" xs))
    (should (v-member "bar" xs))
    (should (v-member "baz" xs))
    (should (v-member (cons 'x 1) xs))))

(ert-deftest v/v-memq ()
  (let ((xs (vector (cons 'x 1) (cons 'y "foo") (cons 'z nil) "foo" "bar" "baz" 1 2 'a 'b)))
    (should-not (v-memq 'x xs))
    (should-not (v-memq 'y xs))
    (should-not (v-memq 'z xs))
    (should-not (v-memq 'w xs))

    (should-not (v-memq "foo" xs))
    (should-not (v-memq "bar" xs))
    (should-not (v-memq "baz" xs))
    (should-not (v-memq (cons 'x 1) xs))

    (should (v-memq 1 xs))
    (should (v-memq 2 xs))
    (should (v-memq 'a xs))
    (should (v-memq 'b xs))
    (should-not (v-memq 'c xs))))

(ert "v/.*")

;; Local Variables:
;; End:

;; v-tests.el ends here
