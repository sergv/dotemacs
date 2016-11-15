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


(setf datastructures-tests/tests
      '(datastructures-tests/sorted-set-creation
        datastructures-tests/sorted-set-union
        datastructures-tests/sorted-set-intersection))

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
