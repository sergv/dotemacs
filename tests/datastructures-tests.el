;; datastructures-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday,  5 January 2014
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'common)
(require 'ert)
(require 'datastructures)

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

;; (ert "datastructures-tests/.*")

;; (let ((ert-debug-on-error nil))
;;   (eproj-reset-projects)
;;   (ert (join-lines (map #'symbol->string datastructures-tests/tests) "\\|")
;;        ;; "haskell-tests/.*"
;;        )
;;   nil)

;; Local Variables:
;; End:

;; datastructures-tests.el ends here
