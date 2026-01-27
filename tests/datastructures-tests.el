;; datastructures-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  5 January 2014
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'common)
(require 'common-heavy)
(require 'ert)

(require 'append-list)
(require 'bimap)
(require 'bisect)
(require 'interval-with-margins)
(require 'nested-hash-tables)
(require 'sorted-set)
(require 'trie)

(defconst datastructures-tests/pairs-test-array
  [(1 . b) (1 . c) (1 . a) (2 . x) (2 . y)])

;; note: use of #'equal as equality predicate is
;; inconsistent with lesser-than predicate (i.e. datastructures-tests/pair<)
(defun datastructures-tests/pair= (a b)
  (= (car a) (car b)))

(defun datastructures-tests/pair< (a b)
  (< (car a) (car b)))

(ert-deftest datastructures-tests/sorted-set-creation ()
  (let ((lt #'<))
    (should (= 2
               (sorted-set/length
                (sorted-set/add (sorted-set/add (sorted-set/empty lt)
                                                1)
                                2))))
    (should (= 1
               (sorted-set/length
                (sorted-set/add (sorted-set/add (sorted-set/empty lt)
                                                1)
                                1))))

    (should (= 1
               (sorted-set/length
                (sorted-set/from-list '(1 1 1 1 1 1 1 1) lt))))
    (should (= 3
               (sorted-set/length
                (sorted-set/from-list '(1 1 1 2 1 1 1 5 1 1) lt))))))

(ert-deftest datastructures-tests/sorted-set-union ()
  (let ((lt #'<))
    (should (equal '(1 2 3 4 5)
                   (sorted-set/items
                    (sorted-set/union (sorted-set/from-list '(5 4 3) lt)
                                      (sorted-set/from-list '(3 2 1) lt)))))
    (should (equal 5
                   (sorted-set/length
                    (sorted-set/union (sorted-set/from-list '(5 4 3) lt)
                                      (sorted-set/from-list '(3 2 1) lt)))))
    (should (equal '(0 1 2 3 4 5)
                   (sorted-set/items
                    (sorted-set/union (sorted-set/from-list '(5 4 3) lt)
                                      (sorted-set/from-list '(3 2 1 0) lt)))))
    (should (equal '(1 2 3 4 5 6)
                   (sorted-set/items
                    (sorted-set/union (sorted-set/from-list '(6 5 4 3) lt)
                                      (sorted-set/from-list '(3 2 1) lt)))))
    (should (equal '(1 2 3)
                   (sorted-set/items
                    (sorted-set/union (sorted-set/from-list '(1 2 3) lt)
                                      (sorted-set/from-list '(3 2 1) lt)))))
    (should (equal 3
                   (sorted-set/length
                    (sorted-set/union (sorted-set/from-list '(1 2 3) lt)
                                      (sorted-set/from-list '(3 2 1) lt)))))))

(ert-deftest datastructures-tests/sorted-set-intersection ()
  (let ((lt #'<))
    (should (equal '(3 4)
                   (sorted-set/items
                    (sorted-set/intersection (sorted-set/from-list '(5 4 3)
                                                                   lt)
                                             (sorted-set/from-list '(3 2 1 4)
                                                                   lt)))))
    (should (equal 2
                   (sorted-set/length
                    (sorted-set/intersection (sorted-set/from-list '(5 4 3)
                                                                   lt)
                                             (sorted-set/from-list '(3 2 1 4)
                                                                   lt)))))
    (should (equal '(1 2 3)
                   (sorted-set/items
                    (sorted-set/intersection (sorted-set/from-list '(1 2 3)
                                                                   lt)
                                             (sorted-set/from-list '(3 2 1)
                                                                   lt)))))

    (should (sorted-set/empty?
             (sorted-set/intersection (sorted-set/from-list '(1 2 3)
                                                            lt)
                                      (sorted-set/from-list '(4 5 6)
                                                            lt))))))

(defun datastructures-tests/bimap-consistency-properties (bm)
  (let ((fwd (bimap/forward-map bm))
        (bwd (bimap/backward-map bm)))
    (should (= (hash-table-size fwd)
               (hash-table-size bwd)))
    (dolist (fwd-entry (hash-table->alist fwd))
      (let* ((k (car fwd-entry))
             (v (cdr fwd-entry))
             (v-preimage (gethash v bwd)))
        (should v-preimage)
        (should (equal k v-preimage))))
    (dolist (bwd-entry (hash-table->alist bwd))
      (let* ((v (car bwd-entry))
             (k (cdr bwd-entry))
             (k-preimage (gethash k fwd)))
        (should k-preimage)
        (should (equal v k-preimage))))))

(ert-deftest datastructures-tests/bimap-1 ()
  (let* ((items '(("foo" . 1) ("bar" . 2) ("baz" . 3) ("quux" . 4)))
         (bm (alist->bimap items)))
    (datastructures-tests/bimap-consistency-properties bm)
    (should (equal 4 (bimap-size bm)))
    (dolist (entry items)
      (should (equal (bimap-lookup (car entry) bm)
                     (cdr entry)))
      (should (equal (bimap-lookup-reverse (cdr entry) bm)
                     (car entry))))))

(ert-deftest datastructures-tests/bimap-2 ()
  (let* ((items '(("foo" . 1) ("bar" . 2) ("baz" . 3) ("quux" . 4) ("foo" . 5) ("bar" . 3)))
         (bm (alist->bimap items)))
    (datastructures-tests/bimap-consistency-properties bm)
    (should (equal 3 (bimap-size bm)))
    (dolist (entry '(("foo" . 5) ("bar" . 3) ("quux" . 4)))
      (should (equal (bimap-lookup (car entry) bm)
                     (cdr entry)))
      (should (equal (bimap-lookup-reverse (cdr entry) bm)
                     (car entry))))))

(ert-deftest datastructures-tests/test-bisect-exhaustive ()
  (let* ((count 70)
         (items
          (list->vector
           (cl-loop
             for i from 0 below count
             collect (* i i)))))
    (cl-loop
      for i from 0 below count do
      (cl-loop
        for j from (+ i 1) below count do
        (cl-loop
          for k from i to j do
          (let ((res (bisect (aref items k)
                             items
                             i
                             j
                             #'=
                             #'<)))
            (should res)
            (should (= res k))
            (should (= (aref items k) (aref items res)))))))))

(ert-deftest datastructures-tests/test-bisect-exhaustive-1 ()
  (let ((items (vector (cons 0 'foo)
                       (cons 1 'bar)
                       (cons 2 'baz)
                       (cons 4 'quux)
                       (cons 5 'wat)
                       (cons 6 'ok))))
    (dotimes (i (length items))
      (let ((res (bisect (aref items i)
                         items
                         0
                         (length items)
                         #'datastructures-tests/pair=
                         #'datastructures-tests/pair<)))
        (should res)
        (should (= i res))
        (should (equal (aref items i) (aref items res)))))))

(ert-deftest datastructures-tests/test-bisect-singular-1 ()
  (let ((items (vector (cons 0 'foo)
                       (cons 1 'bar)
                       (cons 2 'baz)
                       (cons 4 'quux)
                       (cons 5 'wat)
                       (cons 6 'ok))))
    (let ((res (bisect (cons 3 'whatever)
                       items
                       0
                       (length items)
                       #'datastructures-tests/pair=
                       #'datastructures-tests/pair<)))
      (should res)
      (should (= res 3))
      (should (equal (aref items 3) (aref items res))))))


(ert-deftest datastructures-tests/test-bisect-find-exhaustive ()
  (let* ((count 50)
         (items
          (list->vector
           (cl-loop
            for i from 0 below count
            collect (* i i)))))
    (cl-loop
     for i from 0 below count do
     (cl-loop
      for j from (+ i 1) below count do
      (cl-loop
       for k from i to j do
       (let* ((target-item (aref items k))
              (res (bisect-find items
                                i
                                j
                                (lambda (x) (= target-item x))
                                (lambda (x) (< target-item x)))))
         (should res)
         (should (= res k))
         (should (= (aref items k) (aref items res)))))))))

(ert-deftest datastructures-tests/test-bisect-find-exhaustive-1 ()
  (let ((items (vector (cons 0 'foo)
                       (cons 1 'bar)
                       (cons 2 'baz)
                       (cons 4 'quux)
                       (cons 5 'wat)
                       (cons 6 'ok))))
    (dotimes (i (length items))
      (let* ((target-item (aref items i))
             (res (bisect-find items
                               0
                               (length items)
                               (lambda (x)
                                 (datastructures-tests/pair= target-item x))
                               (lambda (x)
                                 (datastructures-tests/pair< target-item x)))))
        (should res)
        (should (= i res))
        (should (equal (aref items i) (aref items res)))))))

(ert-deftest datastructures-tests/test-bisect-find-singular-1 ()
  (let ((items (vector (cons 0 'foo)
                       (cons 1 'bar)
                       (cons 2 'baz)
                       (cons 4 'quux)
                       (cons 5 'wat)
                       (cons 6 'ok))))
    (let* ((target-item (cons 3 'whatever))
           (res (bisect-find items
                             0
                             (length items)
                             (lambda (x)
                               (datastructures-tests/pair= target-item x))
                             (lambda (x)
                               (datastructures-tests/pair< target-item x)))))
      (should res)
      (should (= res 3))
      (should (equal (aref items 3) (aref items res))))))


(ert-deftest datastructures-tests/test-bisect-fixnum-exhaustive-1 ()
  (let ((items [0 1 2 4 5 6]))
    (dotimes (i (length items))
      (let ((res (bisect-fixnum (aref items i)
                                items
                                0
                                (length items))))
        (should res)
        (should (= i res))
        (should (= (aref items i) (aref items res)))))))

(ert-deftest datastructures-tests/test-bisect-fixnum-singular-1 ()
  (let ((items [0 1 2 4 5 6]))
    (let ((res (bisect-fixnum 3
                              items
                              0
                              (length items))))
      (should res)
      (should (= res 3))
      (should (= (aref items 3) (aref items res))))))



(ert-deftest datastructures-tests/test-bisect-leftmost1 ()
  (let ((items datastructures-tests/pairs-test-array)
        (target-value (cons 2 'x)))
    (let ((res (bisect-leftmost target-value
                                items
                                0
                                (length items)
                                #'datastructures-tests/pair=
                                #'datastructures-tests/pair<)))
      (should res)
      (should (= res 3))
      (should (datastructures-tests/pair= target-value (aref items res))))))

(ert-deftest datastructures-tests/test-bisect-leftmost2 ()
  (let ((items datastructures-tests/pairs-test-array))
    (dotimes (i (length items))
      (let* ((target-value (aref items i))
             (res (bisect-leftmost target-value
                                   items
                                   0
                                   (length items)
                                   #'datastructures-tests/pair=
                                   #'datastructures-tests/pair<)))
        (should res)
        (if (= (first target-value) 1)
          (should (= res 0))
          (should (= res 3)))
        (should (datastructures-tests/pair= target-value (aref items res)))))))

(ert-deftest datastructures-tests/test-bisect-rightmost1 ()
  (let ((items datastructures-tests/pairs-test-array)
        (target-value (cons 2 'x)))
    (let ((res (bisect-rightmost target-value
                                 items
                                 0
                                 (length items)
                                 #'datastructures-tests/pair=
                                 #'datastructures-tests/pair<)))
      (should res)
      (should (= res 4))
      (should (datastructures-tests/pair= target-value (aref items res))))))

(ert-deftest datastructures-tests/test-bisect-rightmost2 ()
  (let ((items datastructures-tests/pairs-test-array))
    (dotimes (i (length items))
      (let* ((target-value (aref items i))
             (res (bisect-rightmost target-value
                                    items
                                    0
                                    (length items)
                                    #'datastructures-tests/pair=
                                    #'datastructures-tests/pair<)))
        (should res)
        (if (= (first (aref items i)) 1)
          (should (= res 2))
          (should (= res 4)))
        (should (datastructures-tests/pair= target-value (aref items res)))))))

(ert-deftest datastructures-tests/trie-1 ()
  (let ((items '(("foo" . 1)
                 ("bar" . 2)
                 ("quux" . 3)))
        (trie (make-empty-trie)))
    (dolist (x items)
      (trie-insert! (car x) (cdr x) trie))
    (should (trie-lookup-node-char ?f trie))
    (should (trie-lookup-node-char ?b trie))
    (should (trie-lookup-node-char ?q trie))
    (should-not (trie-lookup-node-char ?a trie))

    (dolist (x items)
      (should (equal (trie-lookup (car x) trie)
                     (cdr x))))

    (should (eq (trie-lookup "" trie 'not-found)
                'not-found))
    (should (eq (trie-lookup "f" trie 'not-found)
                'not-found))
    (should (eq (trie-lookup "oo" trie 'not-found)
                'not-found))
    (should (eq (trie-lookup "foobar" trie 'not-found)
                'not-found))
    (should (eq (trie-lookup "baz" trie 'not-found)
                'not-found))))

(ert-deftest datastructures-tests/trie-2 ()
  (let ((items '(("foo" . 1)
                 ("bar" . 2)
                 ("quux" . 3)
                 ("foobar" . 4)
                 ("foobaz" . 5)
                 ("foabar" . 6)
                 ("quzx" . 7)
                 ("qzux" . 8)))
        (trie (make-empty-trie)))
    (dolist (x items)
      (trie-insert! (car x) (cdr x) trie))
    (should (trie-lookup-node-char ?f trie))
    (should (trie-lookup-node-char ?b trie))
    (should (trie-lookup-node-char ?q trie))
    (should-not (trie-lookup-node-char ?a trie))

    (dolist (x items)
      (should (equal (trie-lookup (car x) trie)
                     (cdr x))))

    (should (eq (trie-lookup "" trie 'not-found)
                'not-found))
    (should (eq (trie-lookup "f" trie 'not-found)
                'not-found))
    (should (eq (trie-lookup "oo" trie 'not-found)
                'not-found))
    (should-not (eq (trie-lookup "foobar" trie 'not-found)
                    'not-found))
    (should (eq (trie-lookup "baz" trie 'not-found)
                'not-found))))

(ert-deftest datastructures-tests/trie-normalization-1 ()
  (let ((items '(("foo" . 1)
                 ("bar" . 2)
                 ("quux" . 3)
                 ("foobar" . 4)
                 ("foobaz" . 5)
                 ("foabar" . 6)
                 ("quzx" . 7)
                 ("qzux" . 8)))
        (trie (make-empty-trie)))
    (dolist (x items)
      (trie-insert! (car x) (cdr x) trie))
    (trie-opt-normalize-subtrees! trie)
    (should (trie-lookup-node-char ?f trie))
    (should (trie-lookup-node-char ?b trie))
    (should (trie-lookup-node-char ?q trie))
    (should-not (trie-lookup-node-char ?a trie))

    (dolist (x items)
      (should (equal (trie-lookup (car x) trie)
                     (cdr x))))

    (should (eq (trie-lookup "" trie 'not-found)
                'not-found))
    (should (eq (trie-lookup "f" trie 'not-found)
                'not-found))
    (should (eq (trie-lookup "oo" trie 'not-found)
                'not-found))
    (should-not (eq (trie-lookup "foobar" trie 'not-found)
                    'not-found))
    (should (eq (trie-lookup "baz" trie 'not-found)
                'not-found))))

(ert-deftest datastructures-tests/trie-sharing-1 ()
  (let ((items '(("foo" . 1)
                 ("bar" . 2)
                 ("quux" . 3)
                 ("foobar" . 4)
                 ("foobaz" . 5)
                 ("foabar" . 4)
                 ("quzx" . 7)
                 ("qzux" . 8)))
        (trie (make-empty-trie)))
    (dolist (x items)
      (trie-insert! (car x) (cdr x) trie))

    (setf trie (trie-opt-recover-sharing! trie))

    (should (trie-lookup-node-char ?f trie))
    (should (trie-lookup-node-char ?b trie))
    (should (trie-lookup-node-char ?q trie))
    (should-not (trie-lookup-node-char ?a trie))

    (dolist (x items)
      (should (equal (trie-lookup (car x) trie)
                     (cdr x))))

    (should (eq (trie-lookup "" trie 'not-found)
                'not-found))
    (should (eq (trie-lookup "f" trie 'not-found)
                'not-found))
    (should (eq (trie-lookup "oo" trie 'not-found)
                'not-found))
    (should-not (eq (trie-lookup "foobar" trie 'not-found)
                    'not-found))
    (should (eq (trie-lookup "baz" trie 'not-found)
                'not-found))))

(ert-deftest datastructures-tests/trie-insert-with-1 ()
  (let ((items '(("foo" . 1)
                 ("bar" . 2)
                 ("quux" . 3)
                 ("foobar" . 4)
                 ("foobaz" . 5)
                 ("foabar" . 6)
                 ("quzx" . 7)
                 ("qzux" . 8)))
        (trie (make-empty-trie)))
    (dolist (x items)
      (trie-insert! (car x) (cdr x) trie))

    (should (eq (trie-lookup "foobar" trie 'not-found)
                4))

    (trie-insert! "foobar" 100 trie)

    (should (eq (trie-lookup "foobar" trie 'not-found)
                100))

    (trie-insert-with! "foobar" 200 trie #'+)

    (should (eq (trie-lookup "foobar" trie 'not-found)
                300))))

(ert-deftest datastructures-tests/trie-matches-stringh-suffix-1 ()
  (let ((items '(("foo" . 1)
                 ("bar" . 2)
                 ("quux" . 3)))
        (trie (make-empty-trie)))
    (dolist (x items)
      (trie-insert! (reverse (car x)) (cdr x) trie))

    (should (equal (trie-matches-string-suffix? trie "foo") 1))
    (should (equal (trie-matches-string-suffix? trie "bar") 2))
    (should (equal (trie-matches-string-suffix? trie "quux") 3))

    (should (equal (trie-matches-string-suffix? trie "xfoo") 1))
    (should (equal (trie-matches-string-suffix? trie "xbar") 2))
    (should (equal (trie-matches-string-suffix? trie "xquux") 3))

    (should (equal (trie-matches-string-suffix? trie "fooxfoo") 1))
    (should (equal (trie-matches-string-suffix? trie "barxbar") 2))
    (should (equal (trie-matches-string-suffix? trie "quuxxquux") 3))

    (should (equal (trie-matches-string-suffix? trie "fooxfo" 'not-found) 'not-found))
    (should (equal (trie-matches-string-suffix? trie "barxba" 'not-found) 'not-found))
    (should (equal (trie-matches-string-suffix? trie "quuxxquu" 'not-found) 'not-found))))

(ert-deftest datastructures-tests/append-list-null-1 ()
  (should (append-list-null (append-list-empty))))

(ert-deftest datastructures-tests/append-list-null-2 ()
  (should-not (append-list-null (append-list-singleton 1))))

(ert-deftest datastructures-tests/append-list-null-3 ()
  (let ((lst (append-list-empty)))
    (append-list-append! lst 'a)
    (should-not (append-list-null (append-list-singleton 1)))))

(ert-deftest datastructures-tests/append-list-null-4 ()
  (let ((lst (append-list-empty)))
    (append-list-prepend! lst 'a)
    (should-not (append-list-null (append-list-singleton 1)))
    (append-list-append! lst 'b)
    (should-not (append-list-null (append-list-singleton 1)))))

(ert-deftest datastructures-tests/append-list-1 ()
  (let ((lst (append-list-empty)))
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '()))))

(ert-deftest datastructures-tests/append-list-2 ()
  (let ((lst (append-list-empty)))
    (append-list-append! lst 'bar)
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(bar)))))

(ert-deftest datastructures-tests/append-list-3 ()
  (let ((lst (append-list-empty)))
    (append-list-prepend! lst 'bar)
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(bar)))))

(ert-deftest datastructures-tests/append-list-4a ()
  (let ((lst (append-list-empty)))
    (append-list-prepend! lst 'bar)
    (should (append-list-p lst))
    (append-list-append! lst 'baz)
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(bar baz)))))

(ert-deftest datastructures-tests/append-list-4b ()
  (let ((lst (append-list-empty)))
    (append-list-append! lst 'baz)
    (should (append-list-p lst))
    (append-list-prepend! lst 'bar)
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(bar baz)))))

(ert-deftest datastructures-tests/append-list-5 ()
  (let ((lst (append-list-empty)))
    (append-list-append! lst 'baz)
    (should (append-list-p lst))
    (append-list-prepend! lst 'bar)
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(bar baz)))))


(ert-deftest datastructures-tests/append-list-6 ()
  (let ((lst (append-list-singleton 'foo)))
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(foo)))))

(ert-deftest datastructures-tests/append-list-7 ()
  (let ((lst (append-list-singleton 'foo)))
    (append-list-append! lst 'bar)
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(foo bar)))))

(ert-deftest datastructures-tests/append-list-8 ()
  (let ((lst (append-list-singleton 'foo)))
    (append-list-prepend! lst 'bar)
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(bar foo)))))

(ert-deftest datastructures-tests/append-list-9a ()
  (let ((lst (append-list-singleton 'foo)))
    (append-list-prepend! lst 'bar)
    (should (append-list-p lst))
    (append-list-append! lst 'baz)
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(bar foo baz)))))

(ert-deftest datastructures-tests/append-list-9b ()
  (let ((lst (append-list-singleton 'foo)))
    (append-list-append! lst 'baz)
    (should (append-list-p lst))
    (append-list-prepend! lst 'bar)
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(bar foo baz)))))

(ert-deftest datastructures-tests/append-list-10 ()
  (let ((lst (append-list-singleton 'foo)))
    (append-list-append! lst 'baz)
    (should (append-list-p lst))
    (append-list-prepend! lst 'bar)
    (should (append-list-p lst))
    (should (equal (append-list-reify lst)
                   '(bar foo baz)))))

(ert-deftest datastructures-tests/append-list-11 ()
  (let ((lst (append-list-empty)))
    (append-list-append! lst 'foo)
    (should (append-list-p lst))
    (append-list-append! lst 'bar)
    (should (append-list-p lst))

    (let ((tl (append-list-get-last-cons lst)))

      (should (equal (append-list-reify lst)
                     '(foo bar)))
      (should (equal tl
                     '(bar)))

      (append-list-prepend! lst 'baz)
      (should (append-list-p lst))

      (should (equal (append-list-reify lst)
                     '(baz foo bar)))
      (should (equal tl
                     '(bar)))

      (append-list-append! lst 'quux)
      (should (append-list-p lst))

      (should (equal (append-list-reify lst)
                     '(baz foo bar quux)))
      (should (equal tl
                     '(bar quux)))

      (should (eq (cddr (append-list-reify lst))
                  tl)))))

(ert-deftest datastructures-tests/append-list-12 ()
  (should-error (append-list-get-last-cons (append-list-empty))))

(ert-deftest datastructures-tests/append-list-connect-1 ()
  (let* ((lst (append-list-singleton 'foo))
         (lst2 (append-list-singleton 'bar)))
    (should (append-list-p lst))
    (should (append-list-p lst2))

    (append-list-extend-with! lst lst2)
    (should (append-list-p lst))
    (should (append-list-p lst2))

    (should (equal (append-list-reify lst)
                   '(foo bar)))
    (should (equal (append-list-reify lst2)
                   '(bar)))

    (append-list-append! lst 'baz)
    (should (append-list-p lst))
    (should (append-list-p lst2))

    (should (equal (append-list-reify lst)
                   '(foo bar baz)))
    (should (equal (append-list-reify lst2)
                   '(bar baz)))

    (append-list-append! lst2 'quux)
    (should (append-list-p lst))
    (should (append-list-p lst2))))

(ert-deftest datastructures-tests/append-list-connect-2 ()
  (let* ((lst (append-list-empty))
         (lst2 (append-list-singleton 'bar)))
    (should (append-list-p lst))
    (should (append-list-p lst2))

    (append-list-extend-with! lst lst2)
    (should (append-list-p lst))
    (should (append-list-p lst2))

    (should (equal (append-list-reify lst)
                   '(bar)))
    (should (equal (append-list-reify lst2)
                   '(bar)))

    (append-list-append! lst 'baz)
    (should (append-list-p lst))
    (should (append-list-p lst2))

    (should (equal (append-list-reify lst)
                   '(bar baz)))
    (should (equal (append-list-reify lst2)
                   '(bar baz)))))

;; Test for connecting non-empty list with empty list which
;; we don’t support since it’s an edge case we don’t really exercise.
;; (ert-deftest datastructures-tests/append-list-connect-3 ()
;;   (let* ((lst (append-list-singleton 'foo))
;;          (lst2 (append-list-empty)))
;;     (should (append-list-p lst))
;;     (should (append-list-p lst2))
;;
;;     (append-list-extend-with! lst lst2)
;;     (should (append-list-p lst))
;;     (should (append-list-p lst2))
;;
;;     (should (equal (append-list-reify lst)
;;                    '(foo)))
;;     (should (equal (append-list-reify lst2)
;;                    '()))
;;
;;     (append-list-append! lst 'baz)
;;     (should (append-list-p lst))
;;     (should (append-list-p lst2))
;;
;;     (should (equal (append-list-reify lst)
;;                    '(foo baz)))
;;     (should (equal (append-list-reify lst2)
;;                    '(baz)))
;;
;;     (append-list-append! lst2 'quux)
;;     (should (append-list-p lst))
;;     (should (append-list-p lst2))
;;
;;     (should (equal (append-list-reify lst)
;;                    '(foo baz quux)))
;;     (should (equal (append-list-reify lst2)
;;                    '(baz quux)))
;;
;;     (append-list-append! lst 'a)
;;     (should (append-list-p lst))
;;     (should (append-list-p lst2))
;;
;;     (should (equal (append-list-reify lst)
;;                    '(foo baz quux a)))
;;     (should (equal (append-list-reify lst2)
;;                    '(baz quux a)))
;;
;;     (append-list-append! lst2 'b)
;;     (should (append-list-p lst))
;;     (should (append-list-p lst2))
;;
;;     (should (equal (append-list-reify lst)
;;                    '(foo baz quux a b)))
;;     (should (equal (append-list-reify lst2)
;;                    '(baz quux a b)))))



;; (and (ert "datastructures-tests/.*") t)

;; (let ((ert-debug-on-error nil))
;;   (eproj-reset-projects)
;;   (ert (join-lines (map #'symbol->string datastructures-tests/tests) "\\|")
;;        ;; "haskell-tests/.*"
;;        )
;;   nil)

(ert-deftest datastructures-tests/inplace-delete-if!-1 ()
  (let ((xs '(1 2 3 4 5 1 2 3 4 5)))
    (dolist (x (cons 0 (cons 6 xs)))
      (should (equal (inplace-delete-if! (lambda (y) (= y x))
                                         (copy-list xs))
                     (remove x xs))))))

(defun datastructures-tests/string-contains?-1 ()
  (should (string-contains? ?x "foox")))

(defun datastructures-tests/string-contains?-2 ()
  (should-not (string-contains? ?x "foo")))

(defun datastructures-tests/string-contains?-3 ()
  (should-not (string-contains? ?x "")))

(defun datastructures-tests/string-contains?-4 ()
  (should (string-contains? ?x "x")))

(defun datastructures-tests/string-contains?-5 ()
  (should (string-contains? ?x "xxx")))

(defun datastructures-tests/string-contains?-6 ()
  (should (string-contains? ?x "xfoo")))

(defun datastructures-tests/string-contains?-7 ()
  (should (string-contains? ?x "fooxfoo")))

(defun datastructures-tests/string-contains?-8 ()
  (let ((case-fold-search t))
    (should (string-contains? ?X "fooxfoo"))))

(defun datastructures-tests/string-contains?-9 ()
  (let ((case-fold-search t))
    (should (string-contains? ?x "fooXfoo"))))

(ert-deftest datastructures-tests/interval-with-margins-merge-overlapping! ()
  (cl-macrolet ((i (start end &optional x y)
                  `(mk-interval-with-margins ,start ,end ,x ,y nil nil)))
    (should (equal (interval-with-margins-merge-overlapping! (copy-list (list (i 1 2) (i 3 4))))
                   (list (i 1 2) (i 3 4))))
    (should (equal (interval-with-margins-merge-overlapping! (copy-list (list (i 3 4) (i 1 2))))
                   (list (i 1 2) (i 3 4))))
    (should (equal (interval-with-margins-merge-overlapping! (copy-list (list (i 2 4) (i 1 2))))
                   (list (i 1 4))))
    (should (equal (interval-with-margins-merge-overlapping! (copy-list (list (i 2 4) (i 1 2) (i 3 5))))
                   (list (i 1 5))))
    (should (equal (interval-with-margins-merge-overlapping! (copy-list (list (i 3 4) (i 1 2) (i 3 5))))
                   (list (i 1 2) (i 3 5))))
    (should (equal (interval-with-margins-merge-overlapping! (copy-list (list (i 1 10) (i 2 4))))
                   (list (i 1 10))))
    (should (equal (interval-with-margins-merge-overlapping! (copy-list (list (i 1 10) (i 2 4) (i 3 15))))
                   (list (i 1 15))))
    (should (equal (interval-with-margins-merge-overlapping! (copy-list (list (i 1 3) (i 2 4) (i 3 15))))
                   (list (i 1 15))))
    (should (equal (interval-with-margins-merge-overlapping! (copy-list (list (i 1 5 nil 3) (i 7 10))))
                   (list (i 1 10))))
    (should (equal (interval-with-margins-merge-overlapping! (copy-list (list (i 1 5) (i 7 10 2))))
                   (list (i 1 10))))))

(ert-deftest datastructures-tests/nested-hash-tables-1 ()
  (let ((tables
         (mk-nested-hash-tables
          (list
           (cons #'cadr #'equal)
           (cons #'car #'equal)))))
    (nested-hash-tables/add! '(foo bar baz) tables)
    (nested-hash-tables/add! '(quux bar baz) tables)

    (should
     (equal
      (nested-hash-tables/gethash '(bar bar bar) tables)
      nil))

    (should
     (equal
      (nested-hash-tables/gethash '(bar bar bar) tables 'undefined)
      'undefined))

    (should
     (equal
      (nested-hash-tables/gethash '(foo bar baz) tables)
      '(foo bar baz)))

    (should
     (equal
      (nested-hash-tables/gethash '(quux bar baz) tables)
      '(quux bar baz)))

    (should
     (equal
      (nested-hash-tables->alist tables)
      '((quux quux bar baz)
        (foo  foo bar baz))))))

(ert-deftest datastructures-tests/nested-hash-tables-2 ()
  (let ((tables
         (mk-nested-hash-tables
          (list
           (cons #'cadr #'equal)
           (cons #'car #'equal)
           (cons #'identity #'equal)))))
    (nested-hash-tables/add! '(foo bar baz) tables)
    (nested-hash-tables/add! '(quux bar baz) tables)

    (should
     (equal
      (nested-hash-tables/gethash '(bar bar bar) tables)
      nil))

    (should
     (equal
      (nested-hash-tables/gethash '(bar bar bar) tables 'undefined)
      'undefined))

    (should
     (equal
      (nested-hash-tables/gethash '(foo bar baz) tables)
      '(foo bar baz)))

    (should
     (equal
      (nested-hash-tables/gethash '(quux bar baz) tables)
      '(quux bar baz)))

    (should
     (equal
      (nested-hash-tables->alist tables)
      '(((quux bar baz) quux bar baz)
        ((foo bar baz)  foo bar baz))))))


(provide 'datastructures-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; datastructures-tests.el ends here
