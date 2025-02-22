;; packing-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 25 December 2021
;; Description:

(require 'packing)

(ert-deftest packing-tests/pack-unpack-pair ()
  (dolist (item (list (cons 1 2)
                      (cons 10 20)
                      (cons 100 200)
                      (cons 1000 2000)
                      (cons 10000 20000)
                      (cons 100000 200000)
                      (cons 1000000 2000000)
                      (cons 10000000 20000000)
                      (cons -1 0)
                      (cons 0 -1)
                      (cons -1 -1)
                      (cons -1 -2)
                      (cons -10 -20)
                      (cons 10 -20)
                      (cons -10 20)))
    (let ((unpacked (packing-unpack-pair (packing-pack-pair (car item) (cdr item)))))
      (should (fixnump (car unpacked)))
      (should (fixnump (cdr unpacked)))
      (should (equal unpacked
                     item)))))

(ert-deftest packing-tests/pack-unpack-pair32 ()
  (dolist (item (list (cons 1 2)
                      (cons 10 20)
                      (cons 100 200)
                      (cons 1000 2000)
                      (cons 8191 8191)
                      (cons -8191 8191)
                      (cons 8191 -8191)
                      (cons -1 0)
                      (cons 0 -1)
                      (cons -1 -1)
                      (cons -1 -2)
                      (cons -10 -20)
                      (cons 10 -20)
                      (cons -10 20)))
    (let ((unpacked (packing32-unpack-pair (packing32-pack-pair (car item) (cdr item)))))
      (should (fixnump (car unpacked)))
      (should (fixnump (cdr unpacked)))
      (should (equal unpacked
                     item)))))

(ert-deftest packing-tests/split-join-path ()
  (dolist (item (list "bar/baz"
                      "/foo/bar/baz"
                      "C:/foo/bar/baz"))
    (should (equal (packing--join-path (packing--split-path item))
                   item))))

(ert-deftest packing-tests/pack-unpack-vec ()
  (dolist (item (list []
                      [1]
                      [1 2]
                      [1 2 3]
                      [1 2 3 4]
                      [1 2 3 4 5]
                      [1 2 3 4 5 6]))
    (should (equal (packing-unpack-vec (packing-pack-vec item))
                   item))))

(ert-deftest packing-tests/pack-unpack-list ()
  (dolist (item '(()
                  (1)
                  (1 2)
                  (1 2 3)
                  (1 2 3 4)
                  (1 2 3 4 5)
                  (1 2 3 4 5 6)))
    (should (equal (packing-unpack-list (packing-pack-list item))
                   item))))

(ert-deftest packing-tests/pack-unpack-file-path ()
  (dolist (item (list "bar/baz"
                      "/foo/bar/baz"
                      "C:/foo/bar/baz"))
    (packing--reset-caches)
    (should (equal (packing-unpack-file-path (packing-pack-file-path item))
                   item))))

(ert-deftest packing-tests/cached-pack-unpack-file-path ()
  (dolist (item (list "bar/baz"
                      "/foo/bar/baz"
                      "C:/foo/bar/baz"))
    (packing--reset-caches)
    (should (equal (packing-unpack-file-path (packing-pack-file-path-cached item))
                   item))))

(provide 'packing-tests)

;; Local Variables:
;; End:

;; packing-tests.el ends here
