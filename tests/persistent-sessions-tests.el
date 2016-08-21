;; persistent-sessions-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 21 August 2016
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'ert)

(ert-deftest persistent-sessions-tests/sessions/get-all-text-properties-in-string ()
  (let ((test-string "foo bar"))
    (put-text-property 0 1 'foo 'bar test-string)
    (put-text-property 4 6 'quux 'baz test-string)
    (let ((props (sessions/get-all-text-properties-in-string test-string)))
      (should (listp props))
      (should (= 2 (length props)))
      (should (equal (car props) '(quux baz ((4 . 5)))))
      (should (equal (cadr props) '(foo bar (0)))))))

(ert-deftest persistent-sessions-tests/store-and-restore-string ()
  (let ((test-string "foo bar"))
    (put-text-property 0 1 'foo 'bar test-string)
    (put-text-property 4 6 'quux 'baz test-string)
    (should (equal (text-properties-at 0 test-string) '(foo bar)))
    (should (equal (text-properties-at 1 test-string) nil))
    (should (equal (text-properties-at 2 test-string) nil))
    (should (equal (text-properties-at 3 test-string) nil))
    (should (equal (text-properties-at 4 test-string) '(quux baz)))
    (should (equal (text-properties-at 5 test-string) '(quux baz)))
    (should (equal (text-properties-at 6 test-string) nil))
    (let* ((encoded-string
            (sessions/store-string test-string))
           (converted-string
            (sessions/restore-string encoded-string)))
      (should (listp encoded-string))
      (should (equal 3 (length encoded-string)))
      (should (eq 'string (car encoded-string)))
      (should (listp (cadr encoded-string)))
      (should (listp (caddr encoded-string)))
      (should (stringp converted-string))
      (should (equal (get-text-property 0 'foo converted-string) 'bar))
      (should (null (get-text-property 1 'foo converted-string)))
      (should (null (get-text-property 2 'foo converted-string)))
      (should (null (get-text-property 3 'foo converted-string)))
      (should (equal (get-text-property 4 'quux converted-string) 'baz))
      (should (equal (get-text-property 5 'quux converted-string) 'baz))
      (should (null (get-text-property 6 'quux converted-string))))))

(ert-deftest persistent-sessions-tests/store-and-restore-ring ()
  (let* ((size 10)
         (test-ring (make-ring size)))
    (dotimes (n size)
      (let ((start (mod n 3))
            (new-string (format "foo%d" n)))
        (put-text-property start (+ start 1) 'foo n new-string)
        (ring-insert test-ring new-string)))
    (let ((restored-ring
           (sessions/restore-ring
            (sessions/store-ring
             test-ring))))
      (should (equal size (ring-size restored-ring)))
      (should (equal test-ring restored-ring))
      (should (equal (prin1-to-string test-ring)
                     (prin1-to-string restored-ring))))))

(progn
  (ert "persistent-sessions-tests/.*")
  ;; (ert #'persistent-sessions-tests/store-string-1)
  ;; (ert #'persistent-sessions-tests/store-and-restore-ring)
  nil)

(provide 'persistent-sessions-tests)

;; Local Variables:
;; End:

;; persistent-sessions-tests.el ends here
