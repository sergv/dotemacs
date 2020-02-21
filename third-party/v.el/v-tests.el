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


(ert "v/.*")

;; Local Variables:
;; End:

;; v-tests.el ends here
