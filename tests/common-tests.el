;; common-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 23 April 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'ert)

(ert-deftest common-tests/test-bisect1 ()
  (let ((items [0 1 2 4 5 6]))
    (dotimes (i (length items))
      (let ((res (bisect (aref items i)
                         items
                         0
                         (length items)
                         #'=
                         #'<)))
        (should-not (null? res))
        (should (= i res))))))

(ert-deftest common-tests/test-bisect2 ()
  (let ((items [0 1 2 4 5 6]))
    (let ((res (bisect 3
                       items
                       0
                       (length items)
                       #'=
                       #'<)))
      (should-not (null? res))
      (should (= res 3)))))

(ert-deftest common-tests/test-bisect-leftmost1 ()
  (let ((items [(1 . b) (1 . c) (1 . a) (2 . x) (2 . y)]))
    (let ((res (bisect-leftmost (cons 2 'x)
                                items
                                0
                                (length items)
                                ;; note: use of #'equal? as equality predicate is
                                ;; inconsistent with lesser-than predicate
                                (lambda (a b) (= (first a) (first b)))
                                (lambda (a b) (< (first a) (first b))))))
      (should-not (null? res))
      (should (= res 3)))))

(ert-deftest common-tests/test-bisect-leftmost2 ()
  (let ((items [(1 . b) (1 . c) (1 . a) (2 . x) (2 . y)]))
    (dotimes (i (length items))
      (let ((res (bisect-leftmost (aref items i)
                                  items
                                  0
                                  (length items)
                                  ;; note: use of #'equal? as equality predicate is
                                  ;; inconsistent with lesser-than predicate
                                  (lambda (a b) (= (first a) (first b)))
                                  (lambda (a b) (< (first a) (first b))))))
        (should-not (null? res))
        (if (= (first (aref items i)) 1)
          (should (= res 0))
          (should (= res 3)))))))

(ert "common-tests/\\(?:test-bisect\\(?:-leftmost\\)?\\)[12]")

(provide 'common-tests)

;; Local Variables:
;; End:

;; common-tests.el ends here
