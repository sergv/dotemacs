;; common-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 23 April 2013
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'common)
(require 'common-heavy)
(require 'macro-util)

(require 'ert)

(ert-deftest common-tests/test-delete-if-with-action! ()
  (let ((make-list
         (lambda (n)
           (cl-loop
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

(ert-deftest common-tests/remove-duplicates-sorted! ()
  (should (equal
           (remove-duplicates-sorted! nil #'string=)
           nil))
  (should (equal
           (remove-duplicates-sorted! '("a") #'string=)
           '("a")))
  (should (equal
           (remove-duplicates-sorted! '("a" "a") #'string=)
           '("a")))
  (should (equal
           (remove-duplicates-sorted! '("a" "b") #'string=)
           '("a" "b")))
  (should (equal
           (remove-duplicates-sorted! '("a" "a" "b") #'string=)
           '("a" "b")))
  (should (equal
           (remove-duplicates-sorted! '("a" "b" "a") #'string=)
           '("a" "b" "a")))

  (should (equal
           (remove-duplicates-sorted! '("a" "b" "c") #'string=)
           '("a" "b" "c")))
  (should (equal
           (remove-duplicates-sorted! '("b" "c" "a") #'string=)
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

(ert-deftest common-tests/remove-duplicates-by-hashing-projections ()
  (should (equal
           (remove-duplicates-by-hashing-projections #'identity
                                                     #'equal
                                                     (copy-list '("a" "b" "c")))
           '("a" "b" "c")))
  (should (equal
           (remove-duplicates-by-hashing-projections #'identity
                                                     #'equal
                                                     (copy-list '("b" "c" "a")))
           '("b" "c" "a")))
  (should (equal
           (remove-duplicates-by-hashing-projections #'identity
                                                     #'equal
                                                     (copy-list '("b" "c" "a" "b" "c")))
           '("b" "c" "a")))
  (should (equal
           (remove-duplicates-by-hashing-projections #'identity
                                                     #'equal
                                                     (copy-list '("a" "a" "b" "b" "c" "c")))
           '("a" "b" "c"))))


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

(ert-deftest common-tests/split-shell-command-into-arguments-11 ()
  (should
   (equal
    (split-shell-command-into-arguments "foo \"\" baz")
    '("foo" "" "baz"))))

(ert-deftest common-tests/pp-macro-1 ()
  (let ((macro "[,('["))
    (should (equal macro (list->string (read (pp-macro macro)))))))

(ert-deftest common-tests/pp-macro-2 ()
  (let ((macro ""))
    (should (equal macro (list->string (read (pp-macro macro)))))))

;; (ert-deftest common-tests/pp-macro-3 ()
;;   (let ((macro "\375\375\375\375\375\373\373\373\373\373"))
;;     (should (equal macro (list->string (read (pp-macro macro)))))))


(ert-deftest common-tests/is-uppercase?-1 ()
  (should (is-uppercase? ?X)))

(ert-deftest common-tests/is-uppercase?-2 ()
  (should-not (is-uppercase? ?x)))

(ert-deftest common-tests/is-uppercase?-3 ()
  (let ((case-fold-search t))
    (should (is-uppercase? ?X))))

(ert-deftest common-tests/is-uppercase?-4 ()
  (let ((case-fold-search t))
    (should-not (is-uppercase? ?x))))

(ert-deftest common-tests/is-lowercase?-1 ()
  (should-not (is-lowercase? ?X)))

(ert-deftest common-tests/is-lowercase?-2 ()
  (should (is-lowercase? ?x)))

(ert-deftest common-tests/is-lowercase?-3 ()
  (let ((case-fold-search t))
    (should-not (is-lowercase? ?X))))

(ert-deftest common-tests/is-lowercase?-4 ()
  (let ((case-fold-search t))
    (should (is-lowercase? ?x))))

(ert-deftest common-tests/extended<-1 ()
  (should (extended< 3 4)))

(ert-deftest common-tests/extended<-2 ()
  (should (extended< nil 4)))

(ert-deftest common-tests/extended<-3 ()
  (should-not (extended< 4 nil)))

(ert-deftest common-tests/extended<-4 ()
  (should-not (extended< nil nil)))

(dolist (entry
         '((1  ""       ""      ""     "")
           (2  " "      ""      ""     "")
           (3  "  "     ""      ""     "")
           (4  "x"      "x"     "x"    "x")
           (5  " x"     "x"     "x"    " x")
           (6  "x "     "x "    "x"    "x")
           (7  " x "    "x "    "x"    " x")
           (8  " x  x"  "x  x"  "x  x" " x  x")
           (9  "x  x "  "x  x " "x  x" "x  x")
           (10 " x  x " "x  x " "x  x" " x  x")
           (11 "\rx  x\r" "x  x\r" "x  x" "\rx  x")))
  (cl-destructuring-bind (n input expected-left expected-both expected-right)
      entry
    (eval
     `(progn
        (ert-deftest ,(string->symbol (format "common-tests/trim-whitespace-left-%d" n)) ()
          (should (equal (trim-whitespace-left ,input) ,expected-left)))
        (ert-deftest ,(string->symbol (format "common-tests/trim-whitespace-%d" n)) ()
          (should (equal (trim-whitespace ,input) ,expected-both)))
        (ert-deftest ,(string->symbol (format "common-tests/trim-whitespace-right-%d" n)) ()
          (should (equal (trim-whitespace-right ,input) ,expected-right)))))))

(ert-deftest common-tests/string-contains-only-whitespace?-1 ()
  (should (equal (string-contains-only-whitespace? "")
                 t)))

(ert-deftest common-tests/string-contains-only-whitespace?-2 ()
  (should (equal (string-contains-only-whitespace? "abc")
                 nil)))

(ert-deftest common-tests/string-contains-only-whitespace?-3 ()
  (should (equal (string-contains-only-whitespace? "  abc")
                 nil)))

(ert-deftest common-tests/string-contains-only-whitespace?-4 ()
  (should (equal (string-contains-only-whitespace? "  abc       ")
                 nil)))

(ert-deftest common-tests/string-contains-only-whitespace?-5 ()
  (should (equal (string-contains-only-whitespace? "  \t \t    \n \r  ")
                 t)))

(ert-deftest common-tests/expand-escape-sequences-1 ()
  (should (equal (expand-escape-sequences "abc")
                 "abc")))

(ert-deftest common-tests/expand-escape-sequences-2 ()
  (should (equal (expand-escape-sequences "ab\\nc")
                 "ab\nc")))

(ert-deftest common-tests/expand-escape-sequences-3 ()
  (should (equal (expand-escape-sequences "abc\\n")
                 "abc\n")))

(ert-deftest common-tests/expand-escape-sequences-4 ()
  (should (equal (expand-escape-sequences "abc\\n\\n")
                 "abc\n\n")))

(ert-deftest common-tests/expand-escape-sequences-5 ()
  (should (equal (expand-escape-sequences "\\nabc")
                 "\nabc")))

(ert-deftest common-tests/expand-escape-sequences-6 ()
  (should (equal (expand-escape-sequences "\\n\\nabc")
                 "\n\nabc")))

(ert-deftest common-tests/expand-escape-sequences-7 ()
  (should (equal (expand-escape-sequences "ab\\\\c")
                 "ab\\c")))

(ert-deftest common-tests/expand-escape-sequences-8 ()
  (should (equal (expand-escape-sequences "ab\\tc")
                 "ab\tc")))

(ert-deftest common-tests/expand-escape-sequences-9 ()
  (should (equal (expand-escape-sequences "ab\\rc")
                 "ab\rc")))

(ert-deftest common-tests/expand-escape-sequences-10 ()
  (should (equal (expand-escape-sequences "\\(foo\\|bar\\)")
                 "\\(foo\\|bar\\)")))

(ert-deftest common-tests/expand-escape-sequences-11 ()
  (should (equal (expand-escape-sequences "abc\\")
                 "abc\\")))

;;;

(ert-deftest common-tests/call-n-1 ()
  (should (equal (call-n 0 (lambda (x) (+ x 1)) 0)
                 0)))

(ert-deftest common-tests/call-n-2 ()
  (should (equal (call-n 1 (lambda (x) (+ x 1)) 0)
                 1)))

(ert-deftest common-tests/call-n-3 ()
  (should (equal (call-n 10 (lambda (x) (+ x 1)) 0)
                 10)))

(ert-deftest common-tests/text-before-matches?-1 ()
  (tests-utils--with-temp-buffer
   :action (should (text-before-matches? "foobar"))
   :contents
   (tests-utils--multiline
    ""
    "abc foobar_|_"
    "")
   :initialisation (text-mode)
   :buffer-id common))

(ert-deftest common-tests/text-before-matches?-2 ()
  (tests-utils--with-temp-buffer
   :action (should (text-before-matches? "foobar"))
   :contents
   (tests-utils--multiline
    "foobar_|_")
   :initialisation (text-mode)
   :buffer-id common))

(ert-deftest common-tests/text-before-matches?-3 ()
  (tests-utils--with-temp-buffer
   :action (should-not (text-before-matches? "foobar"))
   :contents
   (tests-utils--multiline
    ""
    "abc  oobar_|_"
    "")
   :initialisation (text-mode)
   :buffer-id common))

(ert-deftest common-tests/text-before-matches?-4 ()
  (tests-utils--with-temp-buffer
   :action (should-not (text-before-matches? "foobar"))
   :contents
   (tests-utils--multiline
    "fooba_|_r")
   :initialisation (text-mode)
   :buffer-id common))

(ert-deftest common-tests/text-after-matches?-1 ()
  (tests-utils--with-temp-buffer
   :action (should (text-after-matches? "foobar"))
   :contents
   (tests-utils--multiline
    ""
    "abc _|_foobar"
    "")
   :initialisation (text-mode)
   :buffer-id common))

(ert-deftest common-tests/text-after-matches?-2 ()
  (tests-utils--with-temp-buffer
   :action (should (text-after-matches? "foobar"))
   :contents
   (tests-utils--multiline
    "_|_foobar")
   :initialisation (text-mode)
   :buffer-id common))

(ert-deftest common-tests/text-after-matches?-3 ()
  (tests-utils--with-temp-buffer
   :action (should-not (text-after-matches? "foobar"))
   :contents
   (tests-utils--multiline
    ""
    "abc  _|_fooba"
    "")
   :initialisation (text-mode)
   :buffer-id common))

(ert-deftest common-tests/text-after-matches?-4 ()
  (tests-utils--with-temp-buffer
   :action (should-not (text-after-matches? "foobar"))
   :contents
   (tests-utils--multiline
    "f_|_oobar")
   :initialisation (text-mode)
   :buffer-id common))

(ert-deftest common-tests/file-name-all-parents-1 ()
  (should (equal (file-name-all-parents "/foo/bar/baz/quux")
                 (list "/foo/bar/baz/quux"
                       "/foo/bar/baz/"
                       "/foo/bar/"
                       "/foo/"
                       "/"))))

(ert-deftest common-tests/file-name-all-parents-2 ()
  (should (equal (file-name-all-parents "c:/foo/bar/baz/quux")
                 (list "c:/foo/bar/baz/quux"
                       "c:/foo/bar/baz/"
                       "c:/foo/bar/"
                       "c:/foo/"
                       "c:/"))))

(ert-deftest common-tests/file-name-all-parents-3 ()
  (should (equal (file-name-all-parents "/foo/bar/baz/quux/")
                 (list "/foo/bar/baz/quux/"
                       "/foo/bar/baz/"
                       "/foo/bar/"
                       "/foo/"
                       "/"))))

(ert-deftest common-tests/file-name-all-parents-4 ()
  (should (equal (file-name-all-parents "c:/foo/bar/baz/quux/")
                 (list "c:/foo/bar/baz/quux/"
                       "c:/foo/bar/baz/"
                       "c:/foo/bar/"
                       "c:/foo/"
                       "c:/"))))

(ert-deftest common-tests/remove-duplicates-from-sorted-list-by-1 ()
  (should (equal (remove-duplicates-from-sorted-list-by nil #'equal)
                 nil)))

(ert-deftest common-tests/remove-duplicates-from-sorted-list-by-2 ()
  (should (equal (remove-duplicates-from-sorted-list-by '(1 1 2 3 4 4 5 5 5) #'equal)
                 '(1 2 3 4 5))))

(ert-deftest common-tests/remove-duplicates-from-sorted-list-by-3 ()
  (should (equal (remove-duplicates-from-sorted-list-by '(1 2 3 4 5) #'equal)
                 '(1 2 3 4 5))))

(ert-deftest common-tests/remove-duplicates-from-sorted-list-by-4 ()
  (should (equal (remove-duplicates-from-sorted-list-by '(1 1 2 3 4 5) #'equal)
                 '(1 2 3 4 5))))

(ert-deftest common-tests/remove-duplicates-from-sorted-list-by-5 ()
  (should (equal (remove-duplicates-from-sorted-list-by '(1 2 3 3 4 5) #'equal)
                 '(1 2 3 4 5))))

(ert-deftest common-tests/remove-duplicates-from-sorted-list-by-6 ()
  (should (equal (remove-duplicates-from-sorted-list-by '(1 2 3 4 5 5) #'equal)
                 '(1 2 3 4 5))))

(ert-deftest common-tests/dovector-1 ()
  (let ((sum 0))
    (dovector (x [1 2 3])
      (setf sum (+ sum x)))
    (should (equal sum 6))))

(ert-deftest common-tests/dovector-2 ()
  (let ((sum 0))
    (dovector ((x idx) [1 2 3])
      (setf sum (+ sum (* idx idx x))))
    (should (equal sum 14))))

(ert-deftest common-tests/parse-regexp-groups-1 ()
  (should (equal (parse-regexp-groups "") nil)))

(ert-deftest common-tests/parse-regexp-groups-2 ()
  (should (equal (parse-regexp-groups "a") nil)))

(ert-deftest common-tests/parse-regexp-groups-3 ()
  (should (equal (parse-regexp-groups "a\\(\\)") '(1))))

(ert-deftest common-tests/parse-regexp-groups-4 ()
  (should (equal (parse-regexp-groups "\\(a\\)") '(1))))

(ert-deftest common-tests/parse-regexp-groups-5 ()
  (should (equal (parse-regexp-groups "\\(a\\(b\\)?\\)") '(2 1))))

(ert-deftest common-tests/parse-regexp-groups-6 ()
  (should (equal (parse-regexp-groups "\\(?:a\\(?:b\\)?\\)") nil)))

(ert-deftest common-tests/parse-regexp-groups-7 ()
  (should (equal (parse-regexp-groups "\\(?:a\\(?7:b\\)?\\)") '(7))))

(ert-deftest common-tests/parse-regexp-groups-8 ()
  (should (equal (parse-regexp-groups "\\(a[\\(]b\\)?\\)") '(1))))

(ert-deftest common-tests/parse-regexp-groups-9 ()
  (should (equal (parse-regexp-groups "\\(?:a\\([?7]:b)?\\)") '(1))))

(ert-deftest common-tests/parse-regexp-groups-10 ()
  (should (equal (parse-regexp-groups "\\(?:a\\[(]?7:b)?\\)") nil)))

(ert-deftest common-tests/parse-regexp-groups-11 ()
  (should (equal (parse-regexp-groups "\\(?:a[\\](?7:b)?\\)") nil)))

(ert-deftest common-tests/parse-regexp-groups-12 ()
  (should (equal (parse-regexp-groups "\\(?:a[\\(]?7:b)?\\)") nil)))

(ert-deftest common-tests/parse-regexp-groups-13 ()
  (should (equal (parse-regexp-groups "\\(?:a[\]\\(]?7:b)?\\)") nil)))

(ert-deftest common-tests/parse-regexp-groups-14 ()
  (should (equal (parse-regexp-groups "\\(?:a\\(?75:b\\)?\\)") '(75))))

(ert-deftest common-tests/parse-regexp-groups-15 ()
  (should (equal (parse-regexp-groups "\\(\\(a\\)\\)") '(2 1))))

(ert-deftest common-tests/parse-regexp-groups-16 ()
  (should (equal (parse-regexp-groups "\\(?5:\\(?2:a\\)\\)") '(2 5))))


(ert-deftest common-tests/common-string-prefix-1 ()
  (should (equal nil (common-string-prefix "foo" "bar" nil)))
  (should (equal nil (common-string-prefix "foo" "bar" t))))

(ert-deftest common-tests/common-string-prefix-2 ()
  (should (equal "foo" (common-string-prefix "foo" "foobar" nil)))
  (should (equal "foo" (common-string-prefix "foo" "foobar" t))))

(ert-deftest common-tests/common-string-prefix-3 ()
  (should (equal "foo" (common-string-prefix "foodud" "foobar" nil)))
  (should (equal "foo" (common-string-prefix "foodud" "foobar" t))))

(ert-deftest common-tests/common-string-prefix-4 ()
  (should (equal nil (common-string-prefix "foodud" "FOObar" nil)))
  (should (equal "foo" (common-string-prefix "foodud" "FOObar" t))))

(ert-deftest common-tests/common-string-prefix-5 ()
  (should (equal nil (common-string-prefix "FOOdud" "foobar" nil)))
  (should (equal "FOO" (common-string-prefix "FOOdud" "foobar" t))))

(ert-deftest common-tests/common-string-prefix-6 ()
  (should (equal "f" (common-string-prefix "fOo" "foobar" nil)))
  (should (equal "fOo" (common-string-prefix "fOo" "foobar" t))))


(ert-deftest common-tests/list<-1 ()
  (should-not (list< nil nil)))

(ert-deftest common-tests/list<-2 ()
  (should (list< nil '(1))))

(ert-deftest common-tests/list<-3 ()
  (should (list< '(1) '(2))))

(ert-deftest common-tests/list<-4 ()
  (should (list< '(1) '(2 3))))

(ert-deftest common-tests/list<-5 ()
  (should-not (list< '(2) '(1))))


(ert-deftest common-tests/string-list<-1 ()
  (should-not (string-list< nil nil)))

(ert-deftest common-tests/string-list<-2 ()
  (should (string-list< nil '("a"))))

(ert-deftest common-tests/string-list<-3 ()
  (should (string-list< '("a") '("b"))))

(ert-deftest common-tests/string-list<-4 ()
  (should (string-list< '("a") '("b" "c"))))

(ert-deftest common-tests/string-list<-5 ()
  (should-not (string-list< '("b") '("a"))))


(ert-deftest common-tests/append-plists-uniq-1a ()
  (let* ((xs '(a 1 b 2 c 3))
         (ys '())
         (xs-copy (copy-list xs)))
    (should (equal (append-plists-uniq xs ys)
                   xs-copy))
    ;; Must not mutate anything
    (should (equal xs xs-copy))))

(ert-deftest common-tests/append-plists-uniq-1b ()
  (let* ((xs '(a 1 b 2 c 3))
         (ys '())
         (xs-copy (copy-list xs)))
    (should (equal (append-plists-uniq ys xs)
                   xs-copy))
    ;; Must not mutate anything
    (should (equal xs xs-copy))))

(ert-deftest common-tests/append-plists-uniq-2a ()
  (let* ((xs '(a 1 b 2 c 3))
         (ys '(a 4))
         (xs-copy (copy-list xs))
         (ys-copy (copy-list ys)))
    (should (equal (append-plists-uniq xs ys)
                   '(a 1 b 2 c 3)))
    ;; Must not mutate anything
    (should (equal xs xs-copy))
    (should (equal ys ys-copy))))

(ert-deftest common-tests/append-plists-uniq-2b ()
  (let* ((xs '(a 1 b 2 c 3))
         (ys '(a 4))
         (xs-copy (copy-list xs))
         (ys-copy (copy-list ys)))
    (should (equal (append-plists-uniq ys xs)
                   '(b 2 c 3 a 4)))
    ;; Must not mutate anything
    (should (equal xs xs-copy))
    (should (equal ys ys-copy))))

(ert-deftest common-tests/append-plists-uniq-3a ()
  (let* ((xs '(a 1 b 2 c 3))
         (ys '(a 4 x 5 c 10))
         (xs-copy (copy-list xs))
         (ys-copy (copy-list ys)))
    (should (equal (append-plists-uniq xs ys)
                   '(x 5 a 1 b 2 c 3)))
    ;; Must not mutate anything
    (should (equal xs xs-copy))
    (should (equal ys ys-copy))))

(ert-deftest common-tests/append-plists-uniq-3b ()
  (let* ((xs '(a 1 b 2 c 3))
         (ys '(a 4 x 5 c 10))
         (xs-copy (copy-list xs))
         (ys-copy (copy-list ys)))
    (should (equal (append-plists-uniq ys xs)
                   '(b 2 a 4 x 5 c 10)))
    ;; Must not mutate anything
    (should (equal xs xs-copy))
    (should (equal ys ys-copy))))

;; (progn
;;   (ert "common-tests/.*")
;;   nil)

(provide 'common-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; common-tests.el ends here
