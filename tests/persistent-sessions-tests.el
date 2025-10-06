;; persistent-sessions-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Sunday, 21 August 2016
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'persistent-sessions)
(require 'persistent-sessions-error-reporting)
(require 'persistent-sessions-serializers)

(require 'ert)

(ert-deftest persistent-sessions-tests/store-and-restore-string-1 ()
  (let ((test-string "foo bar"))
    (put-text-property 0 1 'foo 'bar test-string)
    (put-text-property 4 6 'quux 'baz test-string)
    (progn
      (should (equal (text-properties-at 0 test-string) '(foo bar)))
      (should (equal (text-properties-at 1 test-string) nil))
      (should (equal (text-properties-at 2 test-string) nil))
      (should (equal (text-properties-at 3 test-string) nil))
      (should (equal (text-properties-at 4 test-string) '(quux baz)))
      (should (equal (text-properties-at 5 test-string) '(quux baz)))
      (should (equal (text-properties-at 6 test-string) nil)))
    (let* ((encoded-string
            (sessions/store-value test-string))
           (converted-string
            (sessions/versioned/restore-value nil encoded-string)))
      (should (listp encoded-string))
      (should (equal 4 (length encoded-string)))
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
      (should (null (get-text-property 6 'quux converted-string)))

      (should (string= converted-string test-string)))))

(ert-deftest persistent-sessions-tests/store-and-restore-string-2 ()
  (let ((test-string ""))
    (let* ((encoded-string
            (sessions/store-value test-string))
           (converted-string
            (sessions/versioned/restore-value nil encoded-string)))
      (should (listp encoded-string))
      (should (equal 4 (length encoded-string)))
      (should (eq 'string (car encoded-string)))

      (should (stringp converted-string))
      (should (string= converted-string test-string)))))

(ert-deftest persistent-sessions-tests/store-and-restore-ring ()
  (let* ((size 10)
         (test-ring (make-ring (+ size 1))))
    (dotimes (n size)
      (let ((start (mod n 3))
            (new-string (format "foo%d" n)))
        (put-text-property start (+ start 1) 'foo n new-string)
        (ring-insert test-ring new-string)))
    (ring-insert test-ring "")
    (let ((restored-ring
           (sessions/versioned/restore-value
            nil
            (sessions/store-value test-ring))))
      (should (equal (+ size 1) (ring-size restored-ring)))
      (should (equal test-ring restored-ring))
      (should (equal (prin1-to-string test-ring)
                     (prin1-to-string restored-ring))))))

(ert-deftest persistent-sessions-tests/store-and-restore-proper-list ()
  (let ((test-list (list 'a 'b 'c
                         1 2 3
                         "foo"
                         (propertize "bar" 'foo 'bar 'baz 15)
                         "baz")))
    (let ((restored-list
           (sessions/versioned/restore-value
            nil
            (sessions/store-value test-list))))
      (should (equal test-list restored-list))
      (should (equal (prin1-to-string test-list)
                     (prin1-to-string restored-list))))))

(ert-deftest persistent-sessions-tests/store-and-restore-vector ()
  (let ((test-vector (vector 'a 'b 'c
                             1 2 3
                             nil
                             "foo"
                             (propertize "bar" 'foo 'bar 'baz 15)
                             "baz")))
    (let ((restored-vector
           (sessions/versioned/restore-value
            nil
            (sessions/store-value test-vector))))
      (should (equal test-vector restored-vector))
      (should (equal (prin1-to-string test-vector)
                     (prin1-to-string restored-vector))))))

(ert-deftest persistent-sessions-tests/store-and-restore-hash-table ()
  (let ((test-hash-table (alist->hash-table
                          (list (cons 'foo 'bar)
                                (cons 'baz 5)
                                (cons 'quux (propertize "quux" 'foo 'bar 'baz 15))
                                (cons 5 "frob"))
                          #'eql)))
    (let ((restored-hash-table
           (sessions/versioned/restore-value
            nil
            (sessions/store-value test-hash-table))))
      (should (equal (hash-table-test test-hash-table)
                     (hash-table-test restored-hash-table)))
      (should (equal (persistent-sessions-tests/hash-table-contents test-hash-table)
                     (persistent-sessions-tests/hash-table-contents restored-hash-table))))))

(defun persistent-sessions-tests/hash-table-contents (tbl)
  "Normalize contents of hash table TBL and return it as an alist of key-value
pairs."
  (cl-assert (hash-table-p tbl))
  (sort (hash-table->alist tbl)
        (lambda (a b)
          (let ((x (car a))
                (y (car b)))
            (cond
              ((and (numberp x)
                    (numberp y))
               (< x y))
              ((and (symbolp x)
                    (symbolp y))
               (string< (symbol->string x)
                        (symbol->string y)))
              ((and (numberp x)
                    (symbolp y))
               t)
              ((and (symbolp x)
                    (numberp y))
               nil)
              (t
               (error "persistent-sessions-tests/hash-table-contents: unhandled comparison keys: a = %s, b = %s"
                      x
                      y)))))))

;; (progn
;;   (ert "persistent-sessions-tests/.*")
;;   ;; (ert #'persistent-sessions-tests/sessions/get-all-text-properties-in-string-2)
;;   nil)

(provide 'persistent-sessions-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; persistent-sessions-tests.el ends here
