;; persistent-store-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday, 17 August 2015
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'ert)

(ert-deftest persistent-store-tests/test-merging-1 ()
  (should (equal (persistent-store-try-merging-contents () ())
                 ())))

(ert-deftest persistent-store-tests/test-merging-2 ()
  (should (equal (persistent-store-try-merging-contents () '((x . t)))
                 '((x . t)))))

(ert-deftest persistent-store-tests/test-merging-3 ()
  (should (equal (persistent-store-try-merging-contents '((x . t)) '())
                 '((x . t)))))

(ert-deftest persistent-store-tests/test-merging-4 ()
  (should (equal (persistent-store-try-merging-contents '((x . t)) '((y . 2)))
                 '((x . t) (y . 2)))))

(ert-deftest persistent-store-tests/test-merging-5 ()
  (should (equal (persistent-store-try-merging-contents '((x . t) (z . 3)) '((y . 2)))
                 '((x . t) (y . 2) (z . 3)))))

(ert-deftest persistent-store-tests/test-merging-6 ()
  (should (equal (persistent-store-try-merging-contents '((x . t) (z . 3)) '((w . '(foo bar)) (y . 2)))
                 '((w . '(foo bar)) (x . t) (y . 2) (z . 3)))))

(ert-deftest persistent-store-tests/test-merging-7 ()
  (let ((persistent-store-merge-handlers
         `((x . ,(lambda (old new)
                   (let ((shortest (if (< (length old) (length new)) old new)))
                     shortest))))))
    (should (equal (persistent-store-try-merging-contents '((x . '(foo bar baz))) '((x . '(a b))))
                   '((x . '(a b)))))))

(ert-deftest persistent-store-tests/test-merging-8 ()
  (should (equal (persistent-store-try-merging-contents '((x . t) (y . 1)) '((x . nil)))
                 nil)))

;; (unintern 'persistent-store-tests/test-merging)

;; (progn
;;   (ert "persistent-store-tests/.*")
;;   nil)

(provide 'persistent-store-tests)

;; Local Variables:
;; End:

;; persistent-store-tests.el ends here
