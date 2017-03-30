;; common-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 23 April 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'ert)
(require 'common)
(require 'common-heavy)

;; note: use of #'equal? as equality predicate is
;; inconsistent with lesser-than predicate (i.e. common-tests/pair<)
(defun common-tests/pair= (a b)
  (= (first a) (first b)))

(defun common-tests/pair< (a b)
  (< (first a) (first b)))

(defconst common-tests/pairs-test-array
  [(1 . b) (1 . c) (1 . a) (2 . x) (2 . y)])

(ert-deftest common-tests/test-bisect-exhaustive ()
  (let* ((count 80)
         (items
          (list->vector
           (loop
             for i from 0 below count
             collect (* i i)))))
    (loop
      for i from 0 below count do
      (loop
        for j from (+ i 1) below count do
        (loop
          for k from i to j do
          (let ((res (bisect (aref items k)
                             items
                             i
                             j
                             #'=
                             #'<)))
            (should-not (null? res))
            (should (= res k))
            (should (= (aref items k) (aref items res)))))))))


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
        (should (= i res))
        (should (= (aref items i) (aref items res)))))))

(ert-deftest common-tests/test-bisect2 ()
  (let ((items [0 1 2 4 5 6]))
    (let ((res (bisect 3
                       items
                       0
                       (length items)
                       #'=
                       #'<)))
      (should-not (null? res))
      (should (= res 3))
      (should (= (aref items 3) (aref items res))))))

(ert-deftest common-tests/test-bisect-leftmost1 ()
  (let ((items common-tests/pairs-test-array)
        (target-value (cons 2 'x)))
    (let ((res (bisect-leftmost target-value
                                items
                                0
                                (length items)
                                #'common-tests/pair=
                                #'common-tests/pair<)))
      (should-not (null? res))
      (should (= res 3))
      (should (common-tests/pair= target-value (aref items res))))))

(ert-deftest common-tests/test-bisect-leftmost2 ()
  (let ((items common-tests/pairs-test-array))
    (dotimes (i (length items))
      (let* ((target-value (aref items i))
             (res (bisect-leftmost target-value
                                   items
                                   0
                                   (length items)
                                   #'common-tests/pair=
                                   #'common-tests/pair<)))
        (should-not (null? res))
        (if (= (first target-value) 1)
          (should (= res 0))
          (should (= res 3)))
        (should (common-tests/pair= target-value (aref items res)))))))

(ert-deftest common-tests/test-bisect-rightmost1 ()
  (let ((items common-tests/pairs-test-array)
        (target-value (cons 2 'x)))
    (let ((res (bisect-rightmost target-value
                                 items
                                 0
                                 (length items)
                                 #'common-tests/pair=
                                 #'common-tests/pair<)))
      (should-not (null? res))
      (should (= res 4))
      (should (common-tests/pair= target-value (aref items res))))))

(ert-deftest common-tests/test-bisect-rightmost2 ()
  (let ((items common-tests/pairs-test-array))
    (dotimes (i (length items))
      (let* ((target-value (aref items i))
             (res (bisect-rightmost target-value
                                    items
                                    0
                                    (length items)
                                    #'common-tests/pair=
                                    #'common-tests/pair<)))
        (should-not (null? res))
        (if (= (first (aref items i)) 1)
          (should (= res 2))
          (should (= res 4)))
        (should (common-tests/pair= target-value (aref items res)))))))

(ert-deftest common-tests/test-delete-if-with-action! ()
  (let ((make-list
         (lambda (n)
           (loop
             for i from 0 to n
             collecting i))))
    (should (equal nil
                   (delete-if-with-action!
                    (lambda (x) (error "should not be called"))
                    nil
                    #'ignore)))

    (should (equal '(0)
                   (delete-if-with-action!
                    (lambda (n) nil)
                    (funcall make-list 0)
                    #'ignore)))
    (should (equal '()
                   (delete-if-with-action!
                    (lambda (n) t)
                    (funcall make-list 0)
                    #'ignore)))

    (should (equal '(0 1)
                   (delete-if-with-action!
                    (lambda (n) nil)
                    (funcall make-list 1)
                    #'ignore)))
    (should (equal '(1)
                   (delete-if-with-action!
                    (lambda (n) (= n 0))
                    (funcall make-list 1)
                    #'ignore)))
    (should (equal '(0)
                   (delete-if-with-action!
                    (lambda (n) (= n 1))
                    (funcall make-list 1)
                    #'ignore)))
    (should (equal '()
                   (delete-if-with-action!
                    (lambda (n) t)
                    (funcall make-list 1)
                    #'ignore)))

    (should (equal '(0 1 2 3 4 5)
                   (delete-if-with-action!
                    (lambda (n) nil)
                    (funcall make-list 5)
                    #'ignore)))
    (should (equal '(1 2 3 4 5)
                   (delete-if-with-action!
                    (lambda (n) (= n 0))
                    (funcall make-list 5)
                    #'ignore)))
    (should (equal '(0 2 3 4 5)
                   (delete-if-with-action!
                    (lambda (n) (= n 1))
                    (funcall make-list 5)
                    #'ignore)))
    (should (equal '(0 1 2 3 5)
                   (delete-if-with-action!
                    (lambda (n) (= n 4))
                    (funcall make-list 5)
                    #'ignore)))
    (should (equal '(0 1 2 3 4)
                   (delete-if-with-action!
                    (lambda (n) (= n 5))
                    (funcall make-list 5)
                    #'ignore)))
    (should (equal '(0 1 2 3 4 5)
                   (delete-if-with-action!
                    (lambda (n) (= n 6))
                    (funcall make-list 5)
                    #'ignore)))

    (should (equal '(4 5)
                   (delete-if-with-action!
                    (lambda (n) (<= n 3))
                    (funcall make-list 5)
                    #'ignore)))
    (should (equal '(0 1 2)
                   (delete-if-with-action!
                    (lambda (n) (<= 3 n))
                    (funcall make-list 5)
                    #'ignore)))))

(ert-deftest common-tests/remove-duplicates-sorted ()
  (should (equal
           (remove-duplicates-sorted nil #'string=)
           nil))
  (should (equal
           (remove-duplicates-sorted '("a") #'string=)
           '("a")))
  (should (equal
           (remove-duplicates-sorted '("a" "a") #'string=)
           '("a")))
  (should (equal
           (remove-duplicates-sorted '("a" "b") #'string=)
           '("a" "b")))
  (should (equal
           (remove-duplicates-sorted '("a" "a" "b") #'string=)
           '("a" "b")))
  (should (equal
           (remove-duplicates-sorted '("a" "b" "a") #'string=)
           '("a" "b" "a")))

  (should (equal
           (remove-duplicates-sorted '("a" "b" "c") #'string=)
           '("a" "b" "c")))
  (should (equal
           (remove-duplicates-sorted '("b" "c" "a") #'string=)
           '("b" "c" "a"))))

(ert-deftest common-tests/remove-duplicates-sorting ()
  (should (equal
           (remove-duplicates-sorting (copy-list '("a" "b" "c"))
                                      #'string=
                                      #'string<)
           '("a" "b" "c")))
  (should (equal
           (remove-duplicates-sorting (copy-list '("b" "c" "a"))
                                      #'string=
                                      #'string<)
           '("a" "b" "c")))
  (should (equal
           (remove-duplicates-sorting (copy-list '("b" "c" "a" "b" "c"))
                                      #'string=
                                      #'string<)
           '("a" "b" "c"))))

(ert-deftest common-tests/remove-duplicates-hashing ()
  (should (equal
           (remove-duplicates-hashing (copy-list '("a" "b" "c"))
                                      #'equal)
           '("a" "b" "c")))
  (should (equal
           (remove-duplicates-hashing (copy-list '("b" "c" "a"))
                                      #'equal)
           '("b" "c" "a")))
  (should (equal
           (remove-duplicates-hashing (copy-list '("b" "c" "a" "b" "c"))
                                      #'equal)
           '("b" "c" "a")))
  (should (equal
           (remove-duplicates-hashing (copy-list '("a" "a" "b" "b" "c" "c"))
                                      #'equal)
           '("a" "b" "c"))))

(ert-deftest common-tests/nested-hash-tables ()
  (let ((tables
         (mk-nested-hash-tables
          (list
           (list #'cadr #'equal)
           (list #'car #'equal)))))
    (nested-hash-tables/add! '(foo bar baz)
                             tables)
    (nested-hash-tables/add! '(quux bar baz)
                             tables)
    (should
     (equal
      (nested-hash-tables->alist tables)
      '((quux quux bar baz)
        (foo  foo bar baz))))))

(ert-deftest common-tests/nested-hash-tables-2 ()
  (let ((tables
         (mk-nested-hash-tables
          (list
           (list #'cadr #'equal)
           (list #'car #'equal)
           (list #'identity #'equal)))))
    (nested-hash-tables/add! '(foo bar baz)
                             tables)
    (nested-hash-tables/add! '(quux bar baz)
                             tables)
    (should
     (equal
      (nested-hash-tables->alist tables)
      '(((quux bar baz) quux bar baz)
        ((foo bar baz)  foo bar baz))))))

(ert-deftest common-tests/split-shell-command-into-arguments-1 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo bar baz")
    '("foo" "bar" "baz"))))

(ert-deftest common-tests/split-shell-command-into-arguments-2 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo    bar     baz   ")
    '("foo" "bar" "baz"))))

(ert-deftest common-tests/split-shell-command-into-arguments-3 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo \"bar\" baz")
    '("foo" "bar" "baz"))))

(ert-deftest common-tests/split-shell-command-into-arguments-4 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo 'bar quux' baz")
    '("foo" "bar quux" "baz"))))

(ert-deftest common-tests/split-shell-command-into-arguments-5 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo \"bar quux\" baz")
    '("foo" "bar quux" "baz"))))

(ert-deftest common-tests/split-shell-command-into-arguments-6 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo \"bar 'quux'\" baz")
    '("foo" "bar 'quux'" "baz"))))

(ert-deftest common-tests/split-shell-command-into-arguments-7 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo \"bar \\\"quux\\\"\" baz")
    '("foo" "bar \"quux\"" "baz"))))

(ert-deftest common-tests/split-shell-command-into-arguments-8 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo 'bar '\"'\"'quux' baz")
    '("foo" "bar 'quux" "baz"))))

(ert-deftest common-tests/split-shell-command-into-arguments-9 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo \"bar \"\"quux\" baz")
    '("foo" "bar quux" "baz"))))

(ert-deftest common-tests/split-shell-command-into-arguments-10 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo \"bar \"'quux' baz")
    '("foo" "bar quux" "baz"))))

;; (progn
;;   (ert "common-tests/.*")
;;   nil)

(provide 'common-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; common-tests.el ends here
