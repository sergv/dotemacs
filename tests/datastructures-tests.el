;; datastructures-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  5 January 2014
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'common)
(require 'ert)

(require 'bimap)
(require 'bisect)
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
  (let* ((count 80)
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

;; (ert "datastructures-tests/.*")

;; (let ((ert-debug-on-error nil))
;;   (eproj-reset-projects)
;;   (ert (join-lines (map #'symbol->string datastructures-tests/tests) "\\|")
;;        ;; "haskell-tests/.*"
;;        )
;;   nil)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; datastructures-tests.el ends here
