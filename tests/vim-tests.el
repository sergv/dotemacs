;; vim-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  6 August 2015
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'dash)
(require 'tests-utils)

(require 'vim)
(require 'vim-search)
(require 'ert)

(defconst vim-tests--all-known-modes-and-init
  (cons '(haskell-hsc-mode (haskell-hsc-mode))
        tests-utils--modes-and-init))

(defmacro vim-tests--enable-undo (&rest body)
  `(let ((buffer-undo-list nil))
     ,@body))

(defmacro vim-tests--test-fresh-buffer-contents-init (init action contents expected-value)
  (declare (indent 2))
  `(tests-utils--test-buffer-contents
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    :initialisation ,init
    ;; Don’t reuse buffer to start out in fresh environment each time and don’t
    ;; share things like last cmd events, etc.
    :buffer-id nil))

(defmacro vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands-all-known-inits (name-prefix actions contentss expected-value)
  (declare (indent 1))
  `(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
       ,name-prefix
       ,vim-tests--all-known-modes-and-init
     ,actions
     ,contentss
     ,expected-value))

(defmacro vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    (name-prefix inits actions contentss expected-value)
  (declare (indent 2))
  (cl-assert (symbolp name-prefix))
  (cl-assert (listp contentss))
  (cl-assert (listp actions))
  (cl-assert (listp inits))
  (cl-assert (--all? (or (symbolp (car it)) (numberp (car it))) contentss))
  (cl-assert (--all? (or (symbolp (car it)) (numberp (car it))) actions))
  (cl-assert (--all? (symbolp (car it)) inits))
  `(progn
     ,@(cl-loop
        for action in actions
        append
        (cl-loop
         for init-spec in inits
         append
         (cl-loop
          for contents in contentss
          collect
          `(ert-deftest ,(string->symbol (format "%s-%s-%s-%s"
                                                 name-prefix
                                                 (car init-spec)
                                                 (car action)
                                                 (car contents)))
               ()
             (vim-tests--test-fresh-buffer-contents-init
                 (progn ,@(cdr init-spec))
                 (progn ,@(cdr action))
               ,(cadr contents)

               ,expected-value)))))))

(defmacro vim-tests--test-fresh-buffer-contents-init-all (name inits action contents expected-value)
  (declare (indent 3))
  `(tests-utils--test-buffer-contents-for-inits
    :name ,name
    :inits ,inits
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    ;; Don’t reuse buffer to start out in fresh environment each time and don’t
    ;; share things like last cmd events, etc.
    :buffer-id nil))

;; Text mode has surprising bindings for <tab>. It doesn’t really matter
;; day to day but breaks tests significantly without much benefit testingwise.
(defmacro vim-tests--test-fresh-buffer-contents-init-standard-modes-except (skip-modes name action contents expected-value)
  (declare (indent 3))
  (cl-assert (listp skip-modes))
  (cl-assert (cl-every #'symbolp skip-modes))
  `(vim-tests--test-fresh-buffer-contents-init-all
       ,name
       ,(--remove (memq (car it) skip-modes) tests-utils--modes-and-init)
       ,action
     ,contents
     ,expected-value))

(defmacro vim-tests--test-fresh-buffer-contents-init-standard-modes-only (keep-modes name action contents expected-value)
  (declare (indent 3))
  (cl-assert (listp keep-modes))
  (cl-assert (cl-every #'symbolp keep-modes))
  `(vim-tests--test-fresh-buffer-contents-init-all
       ,name
       ,(--filter (memq (car it) keep-modes) vim-tests--all-known-modes-and-init)
       ,action
     ,contents
     ,expected-value))

(defmacro vim-tests--test-fresh-buffer-contents-init-standard-modes (name action contents expected-value)
  (declare (indent 2))
  `(vim-tests--test-fresh-buffer-contents-init-all
       ,name
       ,tests-utils--modes-and-init
       ,action
     ,contents
     ,expected-value))

(defmacro vim-tests--test-fresh-buffer-contents-init-standard-modes-equivalent-commands (names-and-actions contents expected-value)
  (declare (indent 1))
  `(progn
     ,@(cl-loop
        for entry in names-and-actions
        collect
        (let ((name (car entry))
              (action (cadr entry)))
          `(vim-tests--test-fresh-buffer-contents-init-all
            ,name
            ,tests-utils--modes-and-init
            ,action
            ,contents
            ,expected-value)))))

(defmacro vim-tests--test-fresh-buffer-contents (action contents expected-value)
  (declare (indent 1))
  `(vim-tests--test-fresh-buffer-contents-init
       (text-mode)
       ,action
     ,contents
     ,expected-value))

(cl-defmacro vim-tests--test-fresh-buffer-contents-init-standard-modes-pair-only
    (keep-modes name contents (name1 action1 expected1) (name2 action2 expected2))
  (declare (indent 3))
  (cl-assert (listp keep-modes))
  (cl-assert (cl-every #'symbolp keep-modes))
  (cl-assert (symbolp name1))
  (cl-assert (symbolp name2))
  `(progn
     (vim-tests--test-fresh-buffer-contents-init-all
         ,(string->symbol (format "%s-%s" name name1))
         ,(--filter (memq (car it) keep-modes) vim-tests--all-known-modes-and-init)
         ,action1
       ,contents
       ,expected1)
     (vim-tests--test-fresh-buffer-contents-init-all
         ,(string->symbol (format "%s-%s" name name2))
         ,(--filter (memq (car it) keep-modes) vim-tests--all-known-modes-and-init)
         ,action2
       ,contents
       ,expected2)))

(ert-deftest vim-tests/test-vim--parse-substitute-pattern-repl-flags ()
  (should (equal (vim--parse-substitute-pattern-repl-flags "/foo/bar")
                 '("foo" "bar" nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags ",foo,bar")
                 '("foo" "bar" nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags ",foo,bar,")
                 '("foo" "bar" nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/hello/world/gI")
                 '("hello" "world" "gI")))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/\"/\"/")
                 '("\"" "\"" nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags "         /    a  ")
                 '("    a  " nil nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/xyz")
                 '("xyz" nil nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags ";xyz")
                 '("xyz" nil nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/x/y/gic   uuuu")
                 '("x" "y" "gic")))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/abc/\\/\\/\\/\\//g")
                 '("abc" "\\/\\/\\/\\/" "g")))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/^\\(.*\\)$/\\\"\\1\\\"\\n/g")
                 '("^\\(.*\\)$" "\\\"\\1\\\"\\n" "g")))
  (should (equal (vim--parse-substitute-pattern-repl-flags ",hello ,/world ! world;/,g")
                 '("hello " "/world ! world;/" "g")))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/")
                 '("" nil nil)))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/[A-Z]/foo/g")
                 '("[A-Z]" "foo" "g")))
  (should (equal (vim--parse-substitute-pattern-repl-flags "/hello/world\\/g")
                 '("hello" "world\\/g" nil)))
  ;; (should (equal (vim--parse-substitute-pattern-repl-flags "(rx (or \"a\" \"bb\"))world\\/g")
  ;;                '("\\(?:a\\|bb\\)" "world\\/g" nil)))
  )

(ert-deftest vim-tests/test-vim--parse-align-pattern-flags ()
  (should (equal (vim--parse-align-pattern-flags "/foo")
                 (list "foo" nil)))
  (should (equal (vim--parse-align-pattern-flags "/foo/")
                 (list "foo" nil)))
  (should (equal (vim--parse-align-pattern-flags "/foo/xy")
                 (list "foo" nil)))
  (should (equal (vim--parse-align-pattern-flags "/fo\\no/xy")
                 (list "fo\no" nil)))
  (should (equal (vim--parse-align-pattern-flags "/fo\\/no/xy")
                 (list "fo\\/no" nil)))
  (should (equal (vim--parse-align-pattern-flags "/foo/n")
                 (list "foo" '(?n))))
  (should (equal (vim--parse-align-pattern-flags "/fo\\no/n")
                 (list "fo\no" '(?n))))
  (should (equal (vim--parse-align-pattern-flags "/fo\\/no/nn")
                 (list "fo\\/no" '(?n ?n)))))

(ert-deftest vim-tests/vim:regex-without-case ()
  (should (equal (vim--regex-without-case "foobar")
                 "foobar"))
  (should (equal (vim--regex-without-case "foo\\bar")
                 "foo\\bar"))
  (should (equal (vim--regex-without-case "foo\\.bar")
                 "foo\\.bar"))
  (should (equal (vim--regex-without-case "foobar\\")
                 "foobar\\"))
  (should (equal (vim--regex-without-case "foobar\\x")
                 "foobar\\x"))
  (should (equal (vim--regex-without-case "foobar\\c")
                 "foobar"))
  (should (equal (vim--regex-without-case "\\Cfoobar\\c")
                 "foobar")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/dd-1
    (execute-kbd-macro (kbd ", ,"))
  (tests-utils--multiline
   ""
   ""
   "(foo"
   "   (bar (baz "
   "         _|_  quux  )))"
   "")
  (tests-utils--multiline
   ""
   ""
   "\(foo"
   "   \(bar \(baz "
   "_|_"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (text-mode haskell-mode haskell-ts-mode)
    vim-tests/repeat-vim:splice-sexp-killing-backward-1
    (execute-kbd-macro (kbd "j \( d"))
  (tests-utils--multiline
   ""
   ""
   "(foo"
   "   (bar (baz "
   "         _|_  quux  )))"
   "")
  (tests-utils--multiline
   ""
   ""
   "(foo"
   "   (bar _|_quux  ))"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (text-mode haskell-mode haskell-ts-mode)
    vim-tests/repeat-vim:splice-sexp-killing-backward-2
    (execute-kbd-macro (kbd "j \( d ."))
  (tests-utils--multiline
   ""
   ""
   "(foo"
   "   (bar (baz "
   "         _|_  quux  )))"
   "")
  (tests-utils--multiline
   ""
   ""
   "(foo"
   "   _|_quux  )"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/repeat-vim:splice-sexp-killing-backward-2
    (execute-kbd-macro (kbd "j \( d"))
  (tests-utils--multiline
   ""
   ""
   "(foo"
   "   (bar (baz "
   "         _|_  quux  )))"
   "")
  (tests-utils--multiline
   ""
   ""
   "(foo"
   "   (bar _|_quux  ))"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/repeat-vim:splice-sexp-killing-backward-3
    (execute-kbd-macro (kbd "j \( d ."))
  (tests-utils--multiline
   ""
   ""
   "(foo"
   "   (bar (baz "
   "         _|_  quux  )))"
   "")
  (tests-utils--multiline
   ""
   ""
   "(foo"
   "   _|_quux  )"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/repeat-vim:splice-sexp-killing-backward-4
    (execute-kbd-macro (kbd "j \( d 2 ."))
  (tests-utils--multiline
   ""
   ""
   "(foo"
   "   (bar (baz "
   "         _|_  quux  )))"
   "")
  (tests-utils--multiline
   ""
   ""
   "_|_quux  "
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1
    (execute-kbd-macro (kbd "d d C-v h h h I 1 2 3 <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1a
    (execute-kbd-macro (kbd "d d C-v h h h n I 1 2 3 <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1aa
    (execute-kbd-macro (kbd "d d C-v h h h n n I 1 2 3 <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1aaa
    (execute-kbd-macro (kbd "d d C-v h h h n n n I 1 2 3 <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1aaaa
    (execute-kbd-macro (kbd "d d C-v h h h n n n n I 1 2 3 <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1aaaaa
    (execute-kbd-macro (kbd "d d C-v h h h n n n n n I 1 2 3 <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1b
    (execute-kbd-macro (kbd "C-v h h h d d I 1 2 3 <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1bb
    (execute-kbd-macro (kbd "C-v h h h d d d I 1 2 3 <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1ba
    (execute-kbd-macro (kbd "C-v t t t I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "_|_quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1bba
    (execute-kbd-macro (kbd "C-v t t t $ I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "_|_quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1bbb
    (execute-kbd-macro (kbd "C-v t t t $ n I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "_|_quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1c
    (execute-kbd-macro (kbd "$ C-v t t t 0 I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "_|_quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1cc
    (execute-kbd-macro (kbd "$ C-v t t t 0 d I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "_|_quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1ca
    (execute-kbd-macro (kbd "$ C-v 3 t ^ I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "_|_quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1cb
    (execute-kbd-macro (kbd "$ C-v 3 t ^ d I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "_|_quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1d
    (execute-kbd-macro (kbd "C-v t t t 0 I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "quu_|_x"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1dd
    (execute-kbd-macro (kbd "C-v t t t 0 d I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "quu_|_x"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1da
    (execute-kbd-macro (kbd "C-v 3 t ^ I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "quu_|_x"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-1dda
    (execute-kbd-macro (kbd "C-v 3 t ^ d I 1 2 3 <escape>"))
  (tests-utils--multiline
   "foo"
   "bar"
   "baz"
   "quu_|_x"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3foo"
   "123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))


(vim-tests--test-fresh-buffer-contents-init-all
    vim-tests/block-insert-2
    ((emacs-lisp-mode (emacs-lisp-mode))
     (text-mode (text-mode)))
    (execute-kbd-macro (kbd "d d C-v h h h I f o o - b a r C-w b a z <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "foo-ba_|_zfoo"
   "foo-bazbar"
   "foo-bazbaz"
   "foo-bazquux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-undo-1
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "d d C-v h h h I 1 2 3 <escape> k")))
  (tests-utils--multiline
   ""
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   ""
   "_|_foo"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-undo-2
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "d d C-v h h h I f o o - b a r C-w b a z <escape> k")))
  (tests-utils--multiline
   ""
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   ""
   "_|_foo"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-insert-undo-redo-1
    ;; Enable undo tracking.
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "d d C-v h h h I 1 2 3 <escape> k K")))
  (tests-utils--multiline
   ""
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   ""
   "123foo"
   "_|_123bar"
   "123baz"
   "123quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (rust-mode)
    vim-tests/block-insert-newline-1
    (execute-kbd-macro (kbd "d d C-v h h h I 1 2 3 <return> <escape>"))
  (tests-utils--multiline
   ""
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   ""
   "123"
   "_|_foo"
   "123"
   "bar"
   "123"
   "baz"
   "123"
   "quux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-change-1
    (execute-kbd-macro (kbd "d d C-v h h h n c 1 2 3 <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3o"
   "123r"
   "123z"
   "123ux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-change-char-1
    (execute-kbd-macro (kbd "d d C-v h h h n l 1 2 3 <escape>"))
  (tests-utils--multiline
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   "12_|_3o"
   "123r"
   "123z"
   "123ux"
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-change-2
    (execute-kbd-macro (kbd "C-v b h h h c f o o . <escape>"))
  (tests-utils--multiline
   "        devShell = project (with haskellPackages; [ # [4]"
   "          haskellPackages_|_.cabal-fmt"
   "          haskellPackages.cabal-install"
   "          haskellPackages.haskell-language-server"
   "          haskellPackages.hlint"
   "        ]);")
  (tests-utils--multiline
   "        devShell = project (with haskellPackages; [ # [4]"
   "          foo_|_.cabal-fmt"
   "          foo.cabal-install"
   "          foo.haskell-language-server"
   "          foo.hlint"
   "        ]);"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-change-3
    (execute-kbd-macro (kbd "C-v F . n h c f o o <escape>"))
  (tests-utils--multiline
   "        devShell = project (with haskellPackages; [ # [4]"
   "          haskellPackages.cabal-fmt"
   "          haskellPackages.cabal-install"
   "          haskellPackages.haskell-language-serve_|_r"
   "          haskellPackages.hlint"
   "        ]);")
  (tests-utils--multiline
   "        devShell = project (with haskellPackages; [ # [4]"
   "          haskellPackages.cabal-fmt"
   "          haskellPackages.cabal-install"
   "          haskellPackages.fo_|_o"
   "          haskellPackages.foo"
   "        ]);"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (rust-mode)
    vim-tests/linewise-append-newline-1
    (execute-kbd-macro (kbd "d d V h h h A 1 2 3 <return> <escape>"))
  (tests-utils--multiline
   ""
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   ""
   "foo123"
   "_|_"
   "bar123"
   ""
   "baz123"
   ""
   "quux123"
   ""
   "fizz"
   "frobnicate"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (rust-mode)
    vim-tests/block-append-newline-1
    (execute-kbd-macro (kbd "d d C-v h h h A 1 2 3 <return> <escape>"))
  (tests-utils--multiline
   ""
   "fo_|_o"
   "bar"
   "baz"
   "quux"
   "fizz"
   "frobnicate")
  (tests-utils--multiline
   ""
   "f123"
   "_|_oo"
   "b123"
   "ar"
   "b123"
   "az"
   "q123"
   "uux"
   "fizz"
   "frobnicate"))

(ert-deftest vim-tests/comment-linewise-region-1/emacs-lisp-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (emacs-lisp-mode)
      (execute-kbd-macro (kbd "V h j c c"))
    (tests-utils--multiline
     ""
     "(foo"
     " b_|_ar"
     " baz"
     " quux)"
     "")
    (tests-utils--multiline
     ""
     "(foo"
     " ;; bar"
     " ;; b_|_az"
     " quux)"
     "")))

(ert-deftest vim-tests/comment-linewise-region-1/rust-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (rust-mode)
      (execute-kbd-macro (kbd "V h j c c"))
    (tests-utils--multiline
     ""
     "(foo"
     " b_|_ar"
     " baz"
     " quux)"
     "")
    (tests-utils--multiline
     ""
     "(foo"
     " // bar"
     " // b_|_az"
     " quux)"
     "")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/comment-linewise-region-1-haskell
    (execute-kbd-macro (kbd "V h j c c"))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " -- bar"
   " -- b_|_az"
   " quux)"
   ""))

(ert-deftest vim-tests/comment-linewise-region-1/c-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (c-mode)
      (execute-kbd-macro (kbd "V h j c c"))
    (tests-utils--multiline
     ""
     "(foo"
     " b_|_ar"
     " baz"
     " quux)"
     "")
    (tests-utils--multiline
     ""
     "(foo"
     " // bar"
     " // b_|_az"
     " quux)"
     "")))

(ert-deftest vim-tests/comment-linewise-region-2/c-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (c-mode)
      (execute-kbd-macro (kbd "V h j c c"))
    (tests-utils--multiline
     ""
     "(foo"
     " b_|_ar"
     "   baz"
     " quux)"
     "")
    (tests-utils--multiline
     ""
     "(foo"
     " // bar"
     " //  _|_ baz"
     " quux)"
     "")))

(ert-deftest vim-tests/comment-linewise-region-2a/c-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (c-mode)
      (execute-kbd-macro (kbd "V h h j c c"))
    (tests-utils--multiline
     ""
     "(foo"
     " b_|_ar"
     ""
     "   baz"
     " quux)"
     "")
    (tests-utils--multiline
     ""
     "(foo"
     " // bar"
     " // "
     " //  _|_ baz"
     " quux)"
     "")))

(ert-deftest vim-tests/comment-linewise-region-3/c-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (c-mode)
      (execute-kbd-macro (kbd "V h j c c"))
    (tests-utils--multiline
     ""
     "(foo"
     "\tb_|_ar"
     "\t\tbaz"
     "\tquux)"
     "")
    (tests-utils--multiline
     ""
     "(foo"
     "\t// bar"
     "\t// \t_|_baz"
     "\tquux)"
     "")))

(ert-deftest vim-tests/comment-linewise-region-4/sh-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (sh-mode)
      (execute-kbd-macro (kbd "V h h h h h h j c c"))
    (tests-utils--multiline
     ""
     "    _|_# --skip-depends \\"
     "#--freeze1"
     "if [[ \"$#\" = 0 ]]; then"
     "    do_build \"$builddir_to_use/stage1/bin/\"{ghc,ghc-pkg}"
     "    # do_build \"$builddir_to_use/stage2/bin/\"{ghc,ghc-pkg}"
     "else"
     "fi"
     "")
    (tests-utils--multiline
     ""
     "#     # --skip-depends \\"
     "# #--freeze1"
     "# if [[ \"$#\" = 0 ]]; then"
     "#     do_build \"$builddir_to_use/stage1/bin/\"{ghc,ghc-pkg}"
     "#     # do_build \"$builddir_to_use/stage2/bin/\"{ghc,ghc-pkg}"
     "# else"
     "# fi_|_"
     "")))

(ert-deftest vim-tests/comment-linewise-region-5/sh-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (sh-mode)
      (execute-kbd-macro (kbd "V h h h h h h h j c c"))
    (tests-utils--multiline
     ""
     "    _|_# --skip-depends \\"
     "#--freeze1"
     "if [[ \"$#\" = 0 ]]; then"
     ""
     "    do_build \"$builddir_to_use/stage1/bin/\"{ghc,ghc-pkg}"
     "    # do_build \"$builddir_to_use/stage2/bin/\"{ghc,ghc-pkg}"
     "else"
     "fi"
     "")
    (tests-utils--multiline
     ""
     "#     # --skip-depends \\"
     "# #--freeze1"
     "# if [[ \"$#\" = 0 ]]; then"
     "# "
     "#     do_build \"$builddir_to_use/stage1/bin/\"{ghc,ghc-pkg}"
     "#     # do_build \"$builddir_to_use/stage2/bin/\"{ghc,ghc-pkg}"
     "# else"
     "# fi_|_"
     "")))

(ert-deftest vim-tests/comment-sexp-1/emacs-lisp-mode ()
  (vim-tests--test-fresh-buffer-contents-init
   (emacs-lisp-mode)
   (execute-kbd-macro (kbd "j c c"))
   (tests-utils--multiline
    ""
    "_|_(foo bar)"
    "(quux)"
    "")
   (tests-utils--multiline
    ""
    "_|_;; (foo bar)"
    "(quux)"
    "")))

(ert-deftest vim-tests/comment-sexp-2/emacs-lisp-mode ()
  (vim-tests--test-fresh-buffer-contents-init
   (emacs-lisp-mode)
   (execute-kbd-macro (kbd "j c c h ."))
   (tests-utils--multiline
    ""
    "_|_(foo bar)"
    "(quux)"
    "")
   (tests-utils--multiline
    ""
    ";; (foo bar)"
    "_|_;; (quux)"
    "")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/copy-paste-linewise-region-after-1
    (execute-kbd-macro (kbd "V h y p"))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " bar"
   " _|_bar"
   " baz"
   " baz"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/copy-paste-linewise-region-before-1
    (execute-kbd-macro (kbd "V h y P"))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " _|_bar"
   " baz"
   " bar"
   " baz"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-before-1
    (execute-kbd-macro (kbd "y s P"))
  (tests-utils--multiline
   ""
   "foo _|_bar baz"
   "")
  (tests-utils--multiline
   ""
   "foo _|_barbar baz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-after-1
    (execute-kbd-macro (kbd "y s p"))
  (tests-utils--multiline
   ""
   "foo _|_bar baz"
   "")
  (tests-utils--multiline
   ""
   "foo bba_|_rar baz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-before-visual-block-region-1
    (execute-kbd-macro (kbd "y w C-v h P"))
  (tests-utils--multiline
   ""
   "_|_abc"
   "def"
   "")
  (tests-utils--multiline
   ""
   "_|_abcabc"
   "abcdef"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-before-visual-block-region-2
    (execute-kbd-macro (kbd "y s C-v h P"))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_barar"
   " bbaraz"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-before-visual-block-region-3
    (execute-kbd-macro (kbd "y s C-v h h h P"))
  (tests-utils--multiline
   ""
   "(foo"
   " a_|_bc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " a_|_abcbc"
   " dabcef"
   " gabchi"
   " jabckl"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-before-visual-block-region-4
    (execute-kbd-macro (kbd "C-v b h y <escape> 0 P"))
  (tests-utils--multiline
   ""
   "  , setFunctionFinalizer = mkFunc env (#peek emacs_env, set_function_finalizer) dynSetFunctionFinalize_|_r"
   "  , processInput         = mkFunc env (#peek emacs_env, process_input)          dynProcessInput"
   "")
  (tests-utils--multiline
   ""
   "_|_dynSetFunctionFinalizer  , setFunctionFinalizer = mkFunc env (#peek emacs_env, set_function_finalizer) dynSetFunctionFinalizer"
   "dynProcessInput          , processInput         = mkFunc env (#peek emacs_env, process_input)          dynProcessInput"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-before-visual-block-region-undo-1
    ;; Enable undo tracking.
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "y s C-v h P k")))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-before-visual-block-region-undo-2
    ;; Enable undo tracking.
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "y s C-v h h h P k")))
  (tests-utils--multiline
   ""
   "(foo"
   " a_|_bc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " a_|_bc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-before-insert-mode-visual-block-region-1
    (execute-kbd-macro (kbd "y s C-v h h h I C-p <escape>"))
  (tests-utils--multiline
   ""
   "(foo"
   " a_|_bc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " aab_|_cbc"
   " dabcef"
   " gabchi"
   " jabckl"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-before-insert-mode-visual-block-region-preserves-correct-command-for-repeat-1
    (execute-kbd-macro (kbd "y s C-v h h h I C-p <escape> h ."))
  (tests-utils--multiline
   ""
   "(foo"
   " a_|_bc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " aabcbc"
   " dababc_|_cef"
   " gabchi"
   " jabckl"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-after-visual-block-region-1
    (execute-kbd-macro (kbd "y w C-v h p"))
  (tests-utils--multiline
   ""
   "_|_abc"
   "def"
   "")
  (tests-utils--multiline
   ""
   "aab_|_cbc"
   "dabcef"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-after-visual-block-region-2
    (execute-kbd-macro (kbd "y s C-v h p"))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " baba_|_rr"
   " babarz"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-after-visual-block-region-3
    (execute-kbd-macro (kbd "y s C-v h h h p"))
  (tests-utils--multiline
   ""
   "(foo"
   " a_|_bc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " abab_|_cc"
   " deabcf"
   " ghabci"
   " jkabcl"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-after-visual-block-region-4
    (execute-kbd-macro (kbd "y s C-v h h h n p"))
  (tests-utils--multiline
   ""
   "(foo"
   " a_|_bc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " abcab_|_c"
   " defabc"
   " ghiabc"
   " jklabc"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-after-visual-block-region-undo-1
    ;; Enable undo tracking.
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "y s C-v h p k")))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-after-visual-block-region-undo-2
    ;; Enable undo tracking.
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "y s C-v h h h p k")))
  (tests-utils--multiline
   ""
   "(foo"
   " a_|_bc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " a_|_bc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/copy-line-and-paste-after-1
    (execute-kbd-macro (kbd "y y p"))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " bar"
   " _|_bar"
   " baz"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/copy-line-and-paste-before-1
    (execute-kbd-macro (kbd "y y P"))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " _|_bar"
   " bar"
   " baz"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/navigation-insertion-1
    (execute-kbd-macro (kbd "h h h o <escape>"))
  (tests-utils--multiline
   ""
   "(foo"
   " _|_abc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " abc"
   " def"
   " ghi"
   " jkl"
   "_|_ "
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-cycle-after-1
    (execute-kbd-macro (kbd "y w h y w h y w h y w o <escape> p p p"))
  (tests-utils--multiline
   ""
   "(foo"
   " _|_abc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " abc"
   " def"
   " ghi"
   " jkl"
   " de_|_f"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (text-mode haskell-mode haskell-ts-mode)
    vim-tests/paste-cycle-after-2
    (execute-kbd-macro (kbd "y w h y w h y w h y w o <escape> <tab> p p p"))
  (tests-utils--multiline
   ""
   "(foo"
   " _|_abc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " abc"
   " def"
   " ghi"
   " jkl"
   " "
   "de_|_f quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode haskell-mode haskell-ts-mode)
    vim-tests/paste-cycle-after-2a
    (execute-kbd-macro (kbd "y w h y w h y w h y w o <escape> p p p"))
  (tests-utils--multiline
   ""
   "(foo"
   " _|_abc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " abc"
   " def"
   " ghi"
   " jkl"
   " de_|_f"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (haskell-mode haskell-ts-mode)
    vim-tests/tab-on-newly-created-empty-line-1
    (progn
      (execute-kbd-macro (kbd "h h h o <escape> <tab>"))
      (when (eq major-mode 'text-mode)
        (while (and (char-after)
                    (char= (char-after) ?\s))
          (delete-char 1))))
  (tests-utils--multiline
   ""
   "(foo"
   " _|_abc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " abc"
   " def"
   " ghi"
   " jkl"
   " _|_"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (text-mode haskell-mode haskell-ts-mode)
    vim-tests/paste-cycle-before-1
    (execute-kbd-macro (kbd "y w h y w h y w h y w o <escape> <tab> P P P"))
  (tests-utils--multiline
   ""
   "(foo"
   " _|_abc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " abc"
   " def"
   " ghi"
   " jkl"
   " _|_def"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode haskell-mode haskell-ts-mode)
    vim-tests/paste-cycle-before-1a
    (execute-kbd-macro (kbd "y w h y w h y w h y w o <escape> P P P"))
  (tests-utils--multiline
   ""
   "(foo"
   " _|_abc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " abc"
   " def"
   " ghi"
   " jkl"
   "_|_def "
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-select-delete-1
    (execute-kbd-macro (kbd "C-v h h h 0 ,"))
  (tests-utils--multiline
   ""
   "(foo"
   " _|_abc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   "_|_bc"
   "ef"
   "hi"
   "kl"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/block-select-delete-then-paste-1
    (execute-kbd-macro (kbd "C-v h h h 0 , 2 P"))
  (tests-utils--multiline
   ""
   "(foo"
   " _|_abc"
   " def"
   " ghi"
   " jkl"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   "_|_ a abc"
   " d def"
   " g ghi"
   " j jkl"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/interleave-search-and-repeat-last-find-1
    (execute-kbd-macro (kbd "$ a SPC <escape> , F : / : f o o <return> . u ."))
  (tests-utils--multiline
   ""
   "abc:foo"
   "d_|_ef:foo"
   "ghi:foo"
   "jkl:foo"
   "mno:foo"
   "pqr:foo"
   "stu:foo"
   "")
  (tests-utils--multiline
   ""
   "abc:foo"
   "def "
   "ghi"
   "jk_|_l"
   "mno:foo"
   "pqr:foo"
   "stu:foo"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/interleave-search-and-repeat-last-find-2
    (execute-kbd-macro (kbd "$ F : , e / : f o o <return> ; . u ; ."))
  (tests-utils--multiline
   ""
   "abc:foo"
   "d_|_ef:foo"
   "ghi:foo"
   "jkl:foo"
   "mno:foo"
   "pqr:foo"
   "stu:foo"
   "")
  (tests-utils--multiline
   ""
   "abc:foo"
   "def"
   "ghi"
   "jk_|_l"
   "mno:foo"
   "pqr:foo"
   "stu:foo"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/visual-reactivate-1
    ;; Enable undo tracking.
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "v e h <escape> d t g v ,")))
  (tests-utils--multiline
   ""
   "(foo"
   " _|_bar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " _|_"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/insert-linewise-region-1
    (execute-kbd-macro (kbd "V h I x y z <escape>"))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " xy_|_zbar"
   " xyzbaz"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (c-mode)
    vim-tests/insert-linewise-region-newline-1
    (execute-kbd-macro (kbd "V h I x y z <return> <escape>"))
  (tests-utils--multiline
   ""
   "(foo"
   " b_|_ar"
   " baz"
   " quux)"
   "")
  (tests-utils--multiline
   ""
   "(foo"
   " xyz"
   "_|_ bar"
   " xyz"
   " baz"
   " quux)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (c-mode)
    vim-tests/motion-inner-single-quote-1
    (execute-kbd-macro (kbd ", i '"))
  (tests-utils--multiline
   ""
   "foo 'b_|_ar' baz"
   "")
  (tests-utils--multiline
   ""
   "foo '_|_' baz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (c-mode)
    vim-tests/motion-outer-single-quote-1
    (execute-kbd-macro (kbd ", a '"))
  (tests-utils--multiline
   ""
   "foo 'b_|_ar' baz"
   "")
  (tests-utils--multiline
   ""
   "foo _|_baz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/motion-inner-double-quote-1
    (execute-kbd-macro (kbd ", i \""))
  (tests-utils--multiline
   ""
   "foo \"b_|_ar\" baz"
   "")
  (tests-utils--multiline
   ""
   "foo \"_|_\" baz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/motion-outer-double-quote-1
    (execute-kbd-macro (kbd ", a \""))
  (tests-utils--multiline
   ""
   "foo \"b_|_ar\" baz"
   "")
  (tests-utils--multiline
   ""
   "foo _|_baz"
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-symbol-value
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((is (execute-kbd-macro (kbd ", i s")))
   (s (execute-kbd-macro (kbd ", s"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  ba_|_r_baz_Quux''' (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  Bar_baz_Quux''_|_' (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  ba_|_r_baz_Quux# (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_ (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-symbol-number
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((is (execute-kbd-macro (kbd ", i s")))
   (s (execute-kbd-macro (kbd ", s"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = x + _|_100"
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = x + 10_|_0"
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = x + 1_|_00"
       "")))
  (tests-utils--multiline
   ""
   "foo x = x +_|_ "
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-symbol-value-after-operator
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((is (execute-kbd-macro (kbd ", i s")))
   (s (execute-kbd-macro (kbd ", s"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  ++ba_|_r_baz_Quux''' (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  ++Bar_baz_Quux''_|_' (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  ++ba_|_r_baz_Quux# (x + 1) y \"foo\""
       ""))
   (4 (tests-utils--multiline
       ""
       "foo x = do"
       "  ++_|_bar_baz_Quux# (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  ++_|_ (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-symbol-value-before-operator
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((is (execute-kbd-macro (kbd ", i s")))
   (s (execute-kbd-macro (kbd ", s"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  ba_|_r_baz_Quux'''++ (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  Bar_baz_Quux''_|_'++ (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  ba_|_r_baz_Quux#++ (x + 1) y \"foo\""
       ""))
   (4 (tests-utils--multiline
       ""
       "foo x = do"
       "  _|_bar_baz_Quux#++ (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_++ (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-symbol-value-qualified-names
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((is (execute-kbd-macro (kbd ", i s")))
   (s (execute-kbd-macro (kbd ", s"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.ba_|_r_baz_Quux''' (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.Bar_baz_Quux''_|_' (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.ba_|_r_baz_Quux# (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  Frobnicator._|_ (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-symbol-type
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((is (execute-kbd-macro (kbd ", i s")))
   (s (execute-kbd-macro (kbd ", s"))))
  ((1 (tests-utils--multiline
       ""
       "foo :: ValidateM m => Email '_|_Unvalidated -> m (Email 'Validated)"
       "foo x = undefined"
       ""))
   (2 (tests-utils--multiline
       ""
       "foo :: ValidateM m => Email 'Unvalid_|_ated -> m (Email 'Validated)"
       "foo x = undefined"
       "")))
  (tests-utils--multiline
   ""
   "foo :: ValidateM m => Email _|_ -> m (Email 'Validated)"
   "foo x = undefined"
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-outer-symbol-value
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((as (execute-kbd-macro (kbd ", a s"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  ba_|_r_baz_Quux''' (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  Bar_baz_Quux''_|_' (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  Bar_baz_Quux_|_# (x + 1) y \"foo\""
       ""))
   (4 (tests-utils--multiline
       ""
       "foo x = do"
       "  _|_Bar_baz_Quux# (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_(x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-outer-symbol-value-qualified
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((as (execute-kbd-macro (kbd ", a s"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.ba_|_r_baz_Quux''' (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.Bar_baz_Quux''_|_' (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.Bar_baz_Quux_|_# (x + 1) y \"foo\""
       ""))
   (4 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator._|_Bar_baz_Quux# (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  Frobnicator._|_(x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-qualified-symbol-value
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((iS (execute-kbd-macro (kbd ", i S")))
   (S (execute-kbd-macro (kbd ", S"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  ba_|_r_baz_Quux''' (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  Bar_baz_Quux''_|_' (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  ba_|_r_baz_Quux# (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_ (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-qualified-symbol-value-qualified-names
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((iS (execute-kbd-macro (kbd ", i S")))
   (S (execute-kbd-macro (kbd ", S"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.ba_|_r_baz_Quux''' (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.Bar_baz_Quux''_|_' (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.ba_|_r_baz_Quux# (x + 1) y \"foo\""
       ""))
   (4 (tests-utils--multiline
       ""
       "foo x = do"
       "  Żółć.Frobnicator.ba_|_r_baz_Quux# (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_ (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-qualified-symbol-value-qualified-names-after-operator
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((iS (execute-kbd-macro (kbd ", i S")))
   (S (execute-kbd-macro (kbd ", S"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  ++Frobnicator.ba_|_r_baz_Quux''' (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  ++Frobnicator.Bar_baz_Quux''_|_' (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  ++Frobnicator.ba_|_r_baz_Quux# (x + 1) y \"foo\""
       ""))
   (4 (tests-utils--multiline
       ""
       "foo x = do"
       "  ++Żółć.Frobnicator.ba_|_r_baz_Quux# (x + 1) y \"foo\""
       ""))
   (5 (tests-utils--multiline
       ""
       "foo x = do"
       "  ++_|_Żółć.Frobnicator.bar_baz_Quux# (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  ++_|_ (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-qualified-symbol-value-qualified-names-before-operator
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((iS (execute-kbd-macro (kbd ", i S")))
   (S (execute-kbd-macro (kbd ", S"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.ba_|_r_baz_Quux'''++ (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.Bar_baz_Quux''_|_'++ (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.ba_|_r_baz_Quux#++ (x + 1) y \"foo\""
       ""))
   (4 (tests-utils--multiline
       ""
       "foo x = do"
       "  Żółć.Frobnicator.ba_|_r_baz_Quux#++ (x + 1) y \"foo\""
       ""))
   (5 (tests-utils--multiline
       ""
       "foo x = do"
       "  _|_Żółć.Frobnicator.bar_baz_Quux#++ (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_++ (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-inner-qualified-symbol-type
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((iS (execute-kbd-macro (kbd ", i S")))
   (S (execute-kbd-macro (kbd ", S"))))
  ((1 (tests-utils--multiline
       ""
       "foo :: ValidateM m => Email '_|_Unvalidated -> m (Email 'Validated)"
       "foo x = undefined"
       ""))
   (2 (tests-utils--multiline
       ""
       "foo :: ValidateM m => Email 'Unvalid_|_ated -> m (Email 'Validated)"
       "foo x = undefined"
       ""))
   (3 (tests-utils--multiline
       ""
       "foo :: ValidateM m => Email 'Żółć.Unvalid_|_ated -> m (Email 'Validated)"
       "foo x = undefined"
       "")))
  (tests-utils--multiline
   ""
   "foo :: ValidateM m => Email _|_ -> m (Email 'Validated)"
   "foo x = undefined"
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-outer-qualified-symbol-value
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((aS (execute-kbd-macro (kbd ", a S"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  ba_|_r_baz_Quux''' (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  Bar_baz_Quux''_|_' (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  Bar_baz_Quux_|_# (x + 1) y \"foo\""
       ""))
   (4 (tests-utils--multiline
       ""
       "foo x = do"
       "  _|_Bar_baz_Quux# (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_(x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-outer-qualified-symbol-value-qualified
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((aS (execute-kbd-macro (kbd ", a S"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.ba_|_r_baz_Quux''' (x + 1) y \"foo\""
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.Bar_baz_Quux''_|_' (x + 1) y \"foo\""
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator.Bar_baz_Quux_|_# (x + 1) y \"foo\""
       ""))
   (4 (tests-utils--multiline
       ""
       "foo x = do"
       "  Frobnicator._|_Bar_baz_Quux# (x + 1) y \"foo\""
       ""))
   (5 (tests-utils--multiline
       ""
       "foo x = do"
       "  Żółć.Frobnicator._|_Bar_baz_Quux# (x + 1) y \"foo\""
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_(x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-symbol-value-in-quasiquoter
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((is (execute-kbd-macro (kbd ", i s")))
   (s (execute-kbd-macro (kbd ", s")))
   (as (execute-kbd-macro (kbd ", a s")))
   (iS (execute-kbd-macro (kbd ", i S")))
   (S (execute-kbd-macro (kbd ", S")))
   (aS (execute-kbd-macro (kbd ", a S"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  putStrLn [_|_myosstr|test|]"
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  putStrLn [myos_|_str|test|]"
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  putStrLn [myosst_|_r|test|]"
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  putStrLn [_|_|test|]"
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-symbol-value-in-qualified-quasiquoter
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((is (execute-kbd-macro (kbd ", i s")))
   (s (execute-kbd-macro (kbd ", s")))
   (as (execute-kbd-macro (kbd ", a s"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  putStrLn [Żółć._|_myosstr|test|]"
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  putStrLn [Żółć.myos_|_str|test|]"
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  putStrLn [Żółć.myosst_|_r|test|]"
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  putStrLn [Żółć._|_|test|]"
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-symbol-value-in-qualified-quasiquoter-qualified
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((iS (execute-kbd-macro (kbd ", i S")))
   (S (execute-kbd-macro (kbd ", S")))
   (aS (execute-kbd-macro (kbd ", a S"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  putStrLn [Żółć._|_myosstr|test|]"
       ""))
   (2 (tests-utils--multiline
       ""
       "foo x = do"
       "  putStrLn [Żółć.myos_|_str|test|]"
       ""))
   (3 (tests-utils--multiline
       ""
       "foo x = do"
       "  putStrLn [Żółć.myosst_|_r|test|]"
       ""))
   (4 (tests-utils--multiline
       ""
       "foo x = do"
       "  putStrLn [_|_Żółć.myosstr|test|]"
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  putStrLn [_|_|test|]"
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-symbol-operator
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((is (execute-kbd-macro (kbd ", i s")))
   (s (execute-kbd-macro (kbd ", s"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  foo |+_|_+| bar"
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  foo _|_ bar"
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-motion-symbol-operator-qualified
    ((haskell-mode (haskell-mode))
     (haskell-ts-mode (haskell-ts-mode)))
  ((iS (execute-kbd-macro (kbd ", i S")))
   (S (execute-kbd-macro (kbd ", S"))))
  ((1 (tests-utils--multiline
       ""
       "foo x = do"
       "  foo Decombobulate.|+_|_+| bar"
       "")))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  foo _|_ bar"
   ""))

(ert-deftest vim-tests/c-motion-inner-symbol-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (c-mode)
      (execute-kbd-macro (kbd ", i s"))
    (tests-utils--multiline
     ""
     "int foo(int x) {"
     "    int foo_b_|_ar_Baz__ = x + 1;"
     "}"
     "")
    (tests-utils--multiline
     ""
     "int foo(int x) {"
     "    int _|_ = x + 1;"
     "}"
     "")))

(ert-deftest vim-tests/c-motion-outer-symbol-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (c-mode)
      (execute-kbd-macro (kbd ", a s"))
    (tests-utils--multiline
     ""
     "int foo(int x) {"
     "    int foo_b_|_ar_Baz__ = x + 1;"
     "}"
     "")
    (tests-utils--multiline
     ""
     "int foo(int x) {"
     "    int _|_= x + 1;"
     "}"
     "")))

(ert-deftest vim-tests/emacs-lisp-motion-inner-symbol-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (emacs-lisp-mode)
      (execute-kbd-macro (kbd ", i s"))
    (tests-utils--multiline
     ""
     "(defun foo (x)"
     "  (+ foo-_|_bar-baz-quux+1 x))"
     "")
    (tests-utils--multiline
     ""
     "(defun foo (x)"
     "  (+ _|_ x))"
     "")))

(ert-deftest vim-tests/emacs-lisp-motion-outer-symbol-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (emacs-lisp-mode)
      (execute-kbd-macro (kbd ", a s"))
    (tests-utils--multiline
     ""
     "(defun foo (x)"
     "  (+ foo-_|_bar-baz-quux+1 x))"
     "")
    (tests-utils--multiline
     ""
     "(defun foo (x)"
     "  (+ _|_x))"
     "")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/repeated-search-1
    (execute-kbd-macro (kbd "/ f o o <return>"))
  (tests-utils--multiline
   ""
   "_|_"
   "foo1"
   "bar1"
   "foo2"
   "bar2"
   "foo3"
   "bar3"
   "foo4"
   "bar4"
   "")
  (tests-utils--multiline
   ""
   ""
   "foo_|_1"
   "bar1"
   "foo2"
   "bar2"
   "foo3"
   "bar3"
   "foo4"
   "bar4"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/repeated-search-2
    (execute-kbd-macro (kbd "/ f o o <return> u"))
  (tests-utils--multiline
   ""
   "_|_"
   "foo1"
   "bar1"
   "foo2"
   "bar2"
   "foo3"
   "bar3"
   "foo4"
   "bar4"
   "")
  (tests-utils--multiline
   ""
   ""
   "foo1"
   "bar1"
   "foo_|_2"
   "bar2"
   "foo3"
   "bar3"
   "foo4"
   "bar4"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/repeated-search-3
    (execute-kbd-macro (kbd "/ f o o <return> u u"))
  (tests-utils--multiline
   ""
   "_|_"
   "foo1"
   "bar1"
   "foo2"
   "bar2"
   "foo3"
   "bar3"
   "foo4"
   "bar4"
   "")
  (tests-utils--multiline
   ""
   ""
   "foo1"
   "bar1"
   "foo2"
   "bar2"
   "foo_|_3"
   "bar3"
   "foo4"
   "bar4"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/repeated-search-4
    (execute-kbd-macro (kbd "/ f o o <return> 2 u"))
  (tests-utils--multiline
   ""
   "_|_"
   "foo1"
   "bar1"
   "foo2"
   "bar2"
   "foo3"
   "bar3"
   "foo4"
   "bar4"
   "")
  (tests-utils--multiline
   ""
   ""
   "foo1"
   "bar1"
   "foo2"
   "bar2"
   "foo_|_3"
   "bar3"
   "foo4"
   "bar4"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/repeated-search-5
    (execute-kbd-macro (kbd "/ f o o <return> u u U"))
  (tests-utils--multiline
   ""
   "_|_"
   "foo1"
   "bar1"
   "foo2"
   "bar2"
   "foo3"
   "bar3"
   "foo4"
   "bar4"
   "")
  (tests-utils--multiline
   ""
   ""
   "foo1"
   "bar1"
   "foo2"
   "bar2"
   "_|_foo3"
   "bar3"
   "foo4"
   "bar4"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/delete-to-beginning-of-buffer-1
    (execute-kbd-macro (kbd ", g g"))
  (tests-utils--multiline
   ""
   ""
   "foo1"
   "bar1"
   "foo2"
   "ba_|_r2"
   "foo3"
   "bar3"
   "")
  (tests-utils--multiline
   "_|_r2"
   "foo3"
   "bar3"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (rust-mode)
    vim-tests/record-and-execute-macro-1
    (let ((vim--macro-definitions (make-hash-table :test #'equal))
          (name (cons nil 0)))
      (vim-macro--add-new-definition! name
                                      (kbd "i a b c <return> <escape>"))
      (vim-cmd-execute-macro)
      (vim-cmd-execute-macro))
  (tests-utils--multiline
   ""
   "_|_"
   "")
  (tests-utils--multiline
   ""
   "abc"
   "abc"
   "_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/vim:yank-current-line-1
    (execute-kbd-macro (kbd "Y h P"))
  (tests-utils--multiline
   ""
   "a_|_bc"
   "def"
   "xyz"
   "")
  (tests-utils--multiline
   ""
   "abc"
   "d_|_bcef"
   "xyz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/vim:yank-current-line-2
    (execute-kbd-macro (kbd "y y p"))
  "foo
_|_bar"
  "foo
bar
_|_bar")

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-cycle-with-region-block-paste-1
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "Y C-v h y P P")))
  (tests-utils--multiline
   ""
   "_|_abc"
   "def"
   "xyz"
   "")
  (tests-utils--multiline
   ""
   "_|_abcabc"
   "def"
   "xyz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-cycle-with-region-block-paste-2
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "Y C-v h y Y P P P")))
  (tests-utils--multiline
   ""
   "_|_abc"
   "def"
   "xyz"
   "")
  (tests-utils--multiline
   ""
   "_|_abcabc"
   "def"
   "xyz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-cycle-with-region-block-paste-3
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "Y C-v h y Y P P P k")))
  (tests-utils--multiline
   ""
   "_|_abc"
   "def"
   "xyz"
   "")
  (tests-utils--multiline
   ""
   "_|_aabc"
   "ddef"
   "xyz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/paste-cycle-with-region-block-paste-4
    (vim-tests--enable-undo
     (execute-kbd-macro (kbd "Y C-v h y Y P P P k K")))
  (tests-utils--multiline
   ""
   "_|_abc"
   "def"
   "xyz"
   "")
  (tests-utils--multiline
   ""
   "_|_abcabc"
   "def"
   "xyz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/linewise-region-paste-cycle-1
    (execute-kbd-macro (kbd "t y f , t 2 y y h h P P"))
  (tests-utils--multiline
   "(progn"
   "  (lsp-isar-font-background-markdown-bullet2 :foreground ,orange)"
   "  (lsp-isar-font-background-markdown-bullet3 :foreground ,blue)"
   "  (lsp-isar-font-background-markdown-bullet4 _|_magenta))")
  (tests-utils--multiline
   "(progn"
   "  (lsp-isar-font-background-markdown-bullet2 :foreground ,orange)"
   "  (lsp-isar-font-background-markdown-bullet3 :foreground ,blue)"
   "  (lsp-isar-font-background-markdown-bullet4 _|_:foreground ,magenta))"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/linewise-region-paste-check-paste-info-1
    (progn
      (execute-kbd-macro (kbd "t t 2 y y h h P"))
      (should vim--last-paste)
      (should (equal nil (vim-paste-info-count vim--last-paste)))
      (should (equal 138 (vim-paste-info-begin vim--last-paste)))
      (should (equal 268 (vim-paste-info-end vim--last-paste)))
      (should (equal 183 (vim-paste-info-point vim--last-paste))))
  (tests-utils--multiline
   "(progn"
   "  (lsp-isar-font-background-markdown-bullet2 :foreground ,orange)"
   "  (lsp-isar-font-background-markdown-bullet3 :foreground ,blue)"
   "  (lsp-isar-font-background-markdown-bullet4 _|_magenta))")
  (tests-utils--multiline
   "(progn"
   "  (lsp-isar-font-background-markdown-bullet2 :foreground ,orange)"
   "  (lsp-isar-font-background-markdown-bullet3 :foreground ,blue)"
   "  _|_(lsp-isar-font-background-markdown-bullet2 :foreground ,orange)"
   "  (lsp-isar-font-background-markdown-bullet3 :foreground ,blue)"
   "  (lsp-isar-font-background-markdown-bullet4 magenta))"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-equivalent-commands
    ((vim-tests/splice-sexp-killing-backward-binding-1 (execute-kbd-macro (kbd "M-(")))
     (vim-tests/splice-sexp-killing-backward-binding-2 (execute-kbd-macro (kbd "j M-(")))
     (vim-tests/splice-sexp-killing-backward-binding-3 (execute-kbd-macro (kbd "j ( d"))))
  (tests-utils--multiline
   ""
   "(a _|_(b c) d)"
   "")
  (tests-utils--multiline
   ""
   "_|_(b c) d"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-equivalent-commands
    ((vim-tests/splice-sexp-killing-forward-binding-1 (execute-kbd-macro (kbd "M-)")))
     (vim-tests/splice-sexp-killing-forward-binding-2 (execute-kbd-macro (kbd "j M-)")))
     (vim-tests/splice-sexp-killing-forward-binding-3 (execute-kbd-macro (kbd "j ) d"))))
  (tests-utils--multiline
   ""
   "(a _|_(b c) d)"
   "")
  (tests-utils--multiline
   ""
   "a_|_ "
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-equivalent-commands
    ((vim-tests/raise-binding-1 (execute-kbd-macro (kbd "M-<up>")))
     (vim-tests/raise-binding-2 (execute-kbd-macro (kbd "j M-<up>")))
     (vim-tests/raise-binding-3 (execute-kbd-macro (kbd "j r"))))
  (tests-utils--multiline
   ""
   "(a _|_(b c) d)"
   "")
  (tests-utils--multiline
   ""
   "_|_(b c)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-equivalent-commands
    ((vim-tests/backward-slurp-sexp-binding-1 (execute-kbd-macro (kbd "C-(")))
     (vim-tests/backward-slurp-sexp-binding-2 (execute-kbd-macro (kbd "j C-(")))
     (vim-tests/backward-slurp-sexp-binding-3 (execute-kbd-macro (kbd "j ( ("))))
  (tests-utils--multiline
   ""
   "(a (b_|_ c) d)"
   "")
  (tests-utils--multiline
   ""
   "((a b_|_ c) d)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-equivalent-commands
    ((vim-tests/forward-slurp-sexp-binding-1 (execute-kbd-macro (kbd "C-)")))
     (vim-tests/forward-slurp-sexp-binding-2 (execute-kbd-macro (kbd "j C-)")))
     (vim-tests/forward-slurp-sexp-binding-3 (execute-kbd-macro (kbd "j ) )"))))
  (tests-utils--multiline
   ""
   "(a (b_|_ c) d)"
   "")
  (tests-utils--multiline
   ""
   "(a (b_|_ c d))"
   ""))

(ert-deftest vim-tests/vim:cmd-insert-line-below-repeat-complex-insert-command-1/emacs-lisp-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (emacs-lisp-mode)
      (execute-kbd-macro (kbd "o \( f o o C-\) <escape>"))
    (tests-utils--multiline
     ""
     "a_|_bc"
     "def"
     "xyz"
     "")
    (tests-utils--multiline
     ""
     "abc"
     "(fo_|_o"
     " def)"
     "xyz"
     "")))

(ert-deftest vim-tests/vim:cmd-insert-line-below-repeat-complex-insert-command-2/emacs-lisp-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (emacs-lisp-mode)
      (execute-kbd-macro (kbd "o \( f o o C-\) <escape> h ."))
    (tests-utils--multiline
     ""
     "a_|_bc"
     "def"
     "xyz"
     "")
    (tests-utils--multiline
     ""
     "abc"
     "(foo"
     " def)"
     "(fo_|_o"
     " xyz)"
     "")))

;; Check that ` prefix is not stripped.
(ert-deftest vim-tests/vim-splice-sexp-killing-backward-1/emacs-lisp-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (emacs-lisp-mode)
      (progn
        (should (eq paredit-indent-sexp-function #'indent-sexp))
        (execute-kbd-macro (kbd "j \( d")))
    (tests-utils--multiline
     ""
     "(let ((counter '#:counter))"
     " _|_ `(dotimes (_ (or count 1))"
     "     (,func)))"
     "")
    (tests-utils--multiline
     ""
     "_|_`(dotimes (_ (or count 1))"
     "   (,func))"
     "")))

;; Check that ` prefix is not stripped.
(ert-deftest vim-tests/vim-raise-sexp-1/emacs-lisp-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (emacs-lisp-mode)
      (execute-kbd-macro (kbd "M-<up>"))
    (tests-utils--multiline
     ""
     "(let ((counter '#:counter))"
     " _|_ `(dotimes (_ (or count 1))"
     "     (,func)))"
     "")
    (tests-utils--multiline
     ""
     "_|_`(dotimes (_ (or count 1))"
     "   (,func))"
     "")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-smart-operators-expand-pragma-pair-1
    (execute-kbd-macro (kbd "i { - # S C <return> ok <tab> <escape>"))
  (tests-utils--multiline
   ""
   "_|_"
   "")
  (tests-utils--multiline
   ""
   "{-# SCC \"ok\" #-_|_}"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/paren-insert-1
    (execute-kbd-macro (kbd "i \("))
  (tests-utils--multiline
   ""
   "foo x = _|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = (_|_)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/bracket-insert-1
    (execute-kbd-macro (kbd "i \["))
  (tests-utils--multiline
   ""
   "foo x = _|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = [_|_]"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/dquote-insert-1
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline
   ""
   "foo x = _|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = \"_|_\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/dquote-insert-2
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline
   ""
   "foo x = bar \"_|_\" baz"
   "")
  (tests-utils--multiline
   ""
   "foo x = bar \"\"_|_ baz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/dquote-insert-3
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline
   ""
   "foo x = bar \"xxx_|_\" baz"
   "")
  (tests-utils--multiline
   ""
   "foo x = bar \"xxx\"_|_ baz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/brace-insert-1-haskell
    (execute-kbd-macro (kbd "i \{"))
  (tests-utils--multiline
   ""
   "foo x = _|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = {_|_}"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/wrap-paren-1-haskell
    (execute-kbd-macro (kbd "i \( C-\) C-\)"))
  (tests-utils--multiline
   ""
   "foo x = _|_bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = (_|_bar x)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/wrap-paren-2-haskell
    (execute-kbd-macro (kbd "v e e \("))
  (tests-utils--multiline
   ""
   "foo x = _|_bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = \(_|_bar x\)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/wrap-bracket-1-haskell
    (execute-kbd-macro (kbd "i \[ C-\) C-\)"))
  (tests-utils--multiline
   ""
   "foo x = _|_bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = \[_|_bar x\]"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/wrap-bracket-2-haskell
    (execute-kbd-macro (kbd "v e e \["))
  (tests-utils--multiline
   ""
   "foo x = _|_bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = [_|_bar x]"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/wrap-brace-1-haskell
    (execute-kbd-macro (kbd "i \{ C-\) C-\)"))
  (tests-utils--multiline
   ""
   "foo x = _|_bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = \{_|_bar x\}"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/wrap-brace-2-haskell
    (execute-kbd-macro (kbd "v e e \{"))
  (tests-utils--multiline
   ""
   "foo x = _|_bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = \{_|_bar x\}"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/wrap-backtick-1-haskell
    (execute-kbd-macro (kbd "i y SPC <escape> w v e `"))
  (tests-utils--multiline
   ""
   "foo x = _|_bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = y `_|_bar` x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/wrap-dquotes-1-haskell
    (execute-kbd-macro (kbd "i y SPC <escape> w v e \""))
  (tests-utils--multiline
   ""
   "foo x = _|_bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = y \"_|_bar\" x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/wrap-dquotes-2-haskell
    (execute-kbd-macro (kbd "v E E E \""))
  (tests-utils--multiline
   ""
   "foo x = baz _|_\"bar x\" quux"
   "")
  (tests-utils--multiline
   ""
   "foo x = baz \"_|_\\\"bar x\\\" quux\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-mode-backward-up-indentation-or-sexp-1
    (execute-kbd-macro (kbd "'"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g (h y \"hello \\(Haskell _|_world\\)\" y))"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g (h y _|_\"hello \\(Haskell world\\)\" y))"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-mode-backward-up-indentation-or-sexp-2
    (execute-kbd-macro (kbd "' '"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g (h y \"hello \\(Haskell _|_world\\)\" y))"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g _|_(h y \"hello \\(Haskell world\\)\" y))"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-mode-backward-up-indentation-or-sexp-3
    (execute-kbd-macro (kbd "' ' '"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g (h y \"hello \\(Haskell _|_world\\)\" y))"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux _|_(g (h y \"hello \\(Haskell world\\)\" y))"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-mode-backward-up-indentation-or-sexp-4
    (execute-kbd-macro (kbd "' ' ' '"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g (h y \"hello \\(Haskell _|_world\\)\" y))"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    _|_baz y = quux (g (h y \"hello \\(Haskell world\\)\" y))"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-mode-backward-up-indentation-or-sexp-5
    (execute-kbd-macro (kbd "' ' ' ' '"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g (h y \"hello \\(Haskell _|_world\\)\" y))"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  _|_where"
   "    baz y = quux (g (h y \"hello \\(Haskell world\\)\" y))"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-mode-backward-up-indentation-or-sexp-6
    (execute-kbd-macro (kbd "' ' ' ' ' '"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g (h y \"hello \\(Haskell _|_world\\)\" y))"
   "")
  (tests-utils--multiline
   ""
   "_|_foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g (h y \"hello \\(Haskell world\\)\" y))"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-mode-backward-up-indentation-or-sexp-7
    (progn
      (execute-kbd-macro (kbd "' ' ' ' ' '"))
      (should-error (execute-kbd-macro (kbd "'"))))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g (h y \"hello \\(Haskell _|_world\\)\" y))"
   "")
  (tests-utils--multiline
   ""
   "_|_foo x = do"
   "  bar (x + 1) y"
   "  baz (f y (x + 1))"
   "  where"
   "    baz y = quux (g (h y \"hello \\(Haskell world\\)\" y))"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-newline-auto-comment-1
    (execute-kbd-macro (kbd "i <return> o k <escape>"))
  (tests-utils--multiline
   ""
   "-- foobar_|_"
   "foo x = do"
   "  bar ( x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "-- foobar"
   "-- o_|_k"
   "foo x = do"
   "  bar ( x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-newline-auto-comment-2
    (execute-kbd-macro (kbd "i <return> o k <escape>"))
  (tests-utils--multiline
   ""
   "-- foobar_|_"
   "foo x = do"
   "  bar ( x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "-- foobar"
   "-- o_|_k"
   "foo x = do"
   "  bar ( x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-delete-commented-part-1
    (execute-kbd-macro (kbd "j c d"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  -- important"
   "  -- comment"
   "  -- fo_|_o"
   "  -- bar"
   "  bar (x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_bar (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-delete-commented-part-2
    (execute-kbd-macro (kbd "j c d"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  baz x -- should’ve deleted but couldn’t!"
   "        -- comment"
   "        -- fo_|_o"
   "        -- bar"
   "  bar (x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  baz x -- should’ve deleted but couldn’t!"
   "  _|_bar (x + 1) y \"foo\""
   ""))

(ert-deftest vim-tests/emacs-lisp-abbrev-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (emacs-lisp-mode)
      (execute-kbd-macro (kbd "i \( d k f m SPC"))
    (tests-utils--multiline
     ""
     "_|_"
     "")
    (tests-utils--multiline
     ""
     "(def-keys-for-map _|_)"
     "")))

(ert-deftest vim-tests/python-abbrev-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (python-mode)
      (execute-kbd-macro (kbd "i p r SPC foo <tab> <escape>"))
    (tests-utils--multiline
     ""
     "_|_"
     "")
    (tests-utils--multiline
     ""
     "print(\"foo\"_|_)"
     "")))

(ert-deftest vim-tests/shell-script-abbrev-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (shell-script-mode)
      (execute-kbd-macro (kbd "i i n f o SPC foo <return> bar <return> <return> <escape>"))
    (tests-utils--multiline
     ""
     "_|_"
     "")
    (tests-utils--multiline
     ""
     "echo \"foo = ${foo}, bar = ${bar}_|_\""
     "")))

(ert-deftest vim-tests/c-abbrev-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (c-mode)
      (execute-kbd-macro (kbd "i p r f SPC foo <tab> , SPC bar <tab> <escape>"))
    (tests-utils--multiline
     ""
     "_|_"
     "")
    (tests-utils--multiline
     ""
     "printf(\"foo\\n\", bar)_|_;"
     "")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-import-1
    (execute-kbd-macro (kbd "i i m SPC"))
  (tests-utils--multiline
   ""
   "_|_"
   "")
  (tests-utils--multiline
   ""
   "import _|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-import-2
    (execute-kbd-macro (kbd "i i SPC"))
  (tests-utils--multiline
   ""
   "_|_"
   "")
  (tests-utils--multiline
   ""
   "import _|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-import-3
    (execute-kbd-macro (kbd "i SPC i SPC = i SPC + i <escape>"))
  (tests-utils--multiline
   ""
   "foo_|_"
   "")
  (tests-utils--multiline
   ""
   "foo i = i + _|_i"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-import-4
    (execute-kbd-macro (kbd "i SPC i m SPC = i m SPC + i m <escape>"))
  (tests-utils--multiline
   ""
   "foo_|_"
   "")
  (tests-utils--multiline
   ""
   "foo im = im + i_|_m"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-import-5
    (execute-kbd-macro (kbd "i SPC i m p SPC = b a r SPC i m p SPC 1 <escape>"))
  (tests-utils--multiline
   ""
   "foo_|_"
   "")
  (tests-utils--multiline
   ""
   "foo imp = bar imp _|_1"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-import-6
    (execute-kbd-macro (kbd "i i q SPC F o o <tab> <escape>"))
  (tests-utils--multiline
   ""
   "_|_"
   "")
  (tests-utils--multiline
   ""
   "import qualified Foo as _|_F"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-import--no-expansion-if-not-on-first-column-1
    (execute-kbd-macro (kbd "i i q SPC F o o <escape>"))
  (tests-utils--multiline
   ""
   " _|_"
   "")
  (tests-utils--multiline
   ""
   " iq Fo_|_o"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-1
    (execute-kbd-macro (kbd "i # # SPC u n p a c k <return>"))
  (tests-utils--multiline
   ""
   "_|_"
   "")
  (tests-utils--multiline
   ""
   "{-# UNPACK #-}_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-2a
    (execute-kbd-macro (kbd "i # # SPC u n p a c k <return>"))
  (tests-utils--multiline
   ""
   "_|_"
   "foo :: Int -> Int"
   "foo x = x * x"
   "")
  (tests-utils--multiline
   ""
   "{-# UNPACK #-}_|_"
   "foo :: Int -> Int"
   "foo x = x * x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-2b
    (execute-kbd-macro (kbd "i # # SPC u n p a c k <return>"))
  (tests-utils--multiline
   ""
   "_|_"
   "-- | A function with haddoc docs"
   "foo :: Int -> Int"
   "foo x = x * x"
   "")
  (tests-utils--multiline
   ""
   "{-# UNPACK #-}_|_"
   "-- | A function with haddoc docs"
   "foo :: Int -> Int"
   "foo x = x * x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-abbrev-2c
    (execute-kbd-macro (kbd "i # # SPC i n l i n e <return> <tab>"))
  (tests-utils--multiline
   ""
   "bar root ="
   "  myFold"
   "    Nothing"
   "    (\\absPath _ _ -> pure (Just absPath))"
   "    (Just root)"
   ""
   "_|_"
   "-- | A function with haddoc docs"
   "--"
   "-- more docs"
   "foo :: Int -> Int"
   "foo x = x * x"
   "")
  (tests-utils--multiline
   ""
   "bar root ="
   "  myFold"
   "    Nothing"
   "    (\\absPath _ _ -> pure (Just absPath))"
   "    (Just root)"
   ""
   "{-# INLINE foo #-}_|_"
   "-- | A function with haddoc docs"
   "--"
   "-- more docs"
   "foo :: Int -> Int"
   "foo x = x * x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-abbrev-2d
    (execute-kbd-macro (kbd "i # # SPC i n l i n e <return> <tab>"))
    (tests-utils--multiline
     ""
     "_|_"
     "-- | General form of gathering directory contents."
     "--"
     "-- Treats symlinks the same as regular files and directories. Folding functions can"
     "-- decide how to handle symlinks."
     "listContentsRecFold"
     "  :: forall f a. Foldable f"
     "  => Maybe Int"
     "  -- ^ Depth limit if specified, negative values treated the same as positive ones."
     "  -> (OsPath -> Rel OsPath -> Streaming.FileType -> (IO [a] -> IO [a]) -> IO [a] -> IO [a])"
     "  -- ^ Fold directory by running passed IO action that will scan its contents."
     "  -- Can ignore the action to avoid traversing the directory."
     "  -> (OsPath -> Rel OsPath -> Streaming.FileType -> IO (Maybe a))"
     "  -- ^ What to do with file"
     "  -> f OsPath"
     "  -- ^ Roots to search in, either absolute or relative"
     "  -> IO [a]"
     "listContentsRecFold depthLimit foldDir filePred = undefined")
    (tests-utils--multiline
     ""
     "{-# INLINE listContentsRecFold #-}_|_"
     "-- | General form of gathering directory contents."
     "--"
     "-- Treats symlinks the same as regular files and directories. Folding functions can"
     "-- decide how to handle symlinks."
     "listContentsRecFold"
     "  :: forall f a. Foldable f"
     "  => Maybe Int"
     "  -- ^ Depth limit if specified, negative values treated the same as positive ones."
     "  -> (OsPath -> Rel OsPath -> Streaming.FileType -> (IO [a] -> IO [a]) -> IO [a] -> IO [a])"
     "  -- ^ Fold directory by running passed IO action that will scan its contents."
     "  -- Can ignore the action to avoid traversing the directory."
     "  -> (OsPath -> Rel OsPath -> Streaming.FileType -> IO (Maybe a))"
     "  -- ^ What to do with file"
     "  -> f OsPath"
     "  -- ^ Roots to search in, either absolute or relative"
     "  -> IO [a]"
     "listContentsRecFold depthLimit foldDir filePred = undefined"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-3
    (execute-kbd-macro (kbd "i # s c c SPC 1 2 3 <tab> <escape>"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (_|_ x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar ({-# SCC \"123\" #-_|_} x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-4
    (execute-kbd-macro (kbd "i h p l n SPC"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (_|_ x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (hPutStrLn_|_ x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-5
    (execute-kbd-macro (kbd "i h p l n SPC"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y \"foo\""
   "  _|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y \"foo\""
   "  hPutStrLn _|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-6
    (execute-kbd-macro (kbd "i p n SPC $"))
  (tests-utils--multiline
   ""
   "foo x = _|_do"
   "  bar (x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "foo x = putStrLn $ _|_do"
   "  bar (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-7
    (execute-kbd-macro (kbd "i i m p o r t s SPC"))
  (tests-utils--multiline
   ""
   "_|_"
   "foo x = do"
   "  bar (x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "import Data.Set (Set)"
   "import qualified Data.Set as S_|_"
   "foo x = do"
   "  bar (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-7a
    (progn
      (haskell-ext-tracking-mode +1)
      (should (haskell-ext-tracking-have-import-qualified-post?))
      (execute-kbd-macro (kbd "i i m p o r t s SPC")))
  (tests-utils--multiline
   ""
   "{-# LANGUAGE ImportQualifiedPost #-}"
   "_|_"
   "foo x = do"
   "  bar (x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "{-# LANGUAGE ImportQualifiedPost #-}"
   "import Data.Set (Set)"
   "import Data.Set qualified as S_|_"
   "foo x = do"
   "  bar (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-8
    (execute-kbd-macro (kbd "i # # SPC i n l i n <return> \( + + + <tab> <escape>"))
  (tests-utils--multiline
   ""
   "_|_"
   "(+++) :: Int -> Int"
   "(+++) x y = x + y"
   "")
  (tests-utils--multiline
   ""
   "{-# INLINE (+++) #-_|_}"
   "(+++) :: Int -> Int"
   "(+++) x y = x + y"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-9
    (execute-kbd-macro (kbd "i p p i n f o SPC h e a d e r <return> x <return> y <return> <return>"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_"
   "  bar x"
   "")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import qualified Debug.Trace"
   "import Prettyprinter.Combinators"
   ""
   "foo x = do"
   "  Debug.Trace.trace"
   "    (renderString $ ppDictHeader \"header\""
   "      [ \"x\" --> x"
   "      , \"y\" --> y"
   "      ]) $_|_"
   "  bar x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-9a
    (execute-kbd-macro (kbd "i p p i n f o SPC h e a d e r <return> x <return> y <return> <return>"))
  (tests-utils--multiline
   "import Data.List"
   ""
   "foo x = do"
   "  _|_"
   "  bar x"
   "")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import qualified Debug.Trace"
   "import Prettyprinter.Combinators"
   ""
   "import Data.List"
   ""
   "foo x = do"
   "  Debug.Trace.trace"
   "    (renderString $ ppDictHeader \"header\""
   "      [ \"x\" --> x"
   "      , \"y\" --> y"
   "      ]) $_|_"
   "  bar x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-10
    (execute-kbd-macro (kbd "i p p i n f o SPC h e a d e r <return> x <return> y <return> <return>"))
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  _|_"
   "  bar x"
   "")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  Debug.Trace.trace"
   "    (renderString $ ppDictHeader \"header\""
   "      [ \"x\" --> x"
   "      , \"y\" --> y"
   "      ]) $_|_"
   "  bar x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-11
    (execute-kbd-macro (kbd "i p p i n f o m SPC h e a d e r <return> x <return> y <return> <return>"))
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  _|_"
   "  bar x"
   "")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  Debug.Trace.traceM $ renderString $ ppDictHeader \"header\""
   "    [ \"x\" --> x"
   "    , \"y\" --> y"
   "    ]_|_"
   "  bar x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-12
    (execute-kbd-macro (kbd "i t r a c e SPC x <return> y <return> <return>"))
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  _|_"
   "  bar x"
   "")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  Debug.Trace.trace (\"x = \" ++ show x ++ \", y = \" ++ show y) $ _|_"
   "  bar x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-13
    (execute-kbd-macro (kbd "i t r a c e m SPC x <return> y <return> <return>"))
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  _|_"
   "  bar x"
   "")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  Debug.Trace.traceM $ \"x = \" ++ show x ++ \", y = \" ++ show y_|_"
   "  bar x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode)
    vim-tests/haskell-abbrev-14a
    (execute-kbd-macro (kbd "i i n f o m SPC x <return> y <return> <return>"))
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  _|_"
   "  bar x"
   "")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  liftIO $ putStrLn $ \"x = \" ++ show x ++ \", y = \" ++ show y_|_"
   "  bar x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-abbrev-14b
    (execute-kbd-macro (kbd "i i n f o m SPC x <return> y <return> <return>"))
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  _|_"
   "  bar x"
   "")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "import Prettyprinter.Combinators"
   "import Debug.Trace"
   ""
   "foo x = do"
   "  putStrLn $ \"x = \" ++ show x ++ \", y = \" ++ show y_|_"
   "  bar x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-should-expand-only-on-full-abbrev-1
    (execute-kbd-macro (kbd "i SPC = b a r <escape>"))
  (tests-utils--multiline
   ""
   "foo pinfo_|_"
   "")
  (tests-utils--multiline
   ""
   "foo pinfo = ba_|_r"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-should-expand-only-on-full-abbrev-2
    (execute-kbd-macro (kbd "i SPC = b a r <escape>"))
  (tests-utils--multiline
   ""
   "foo myppinfo_|_"
   "")
  (tests-utils--multiline
   ""
   "foo myppinfo = ba_|_r"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-1
    (execute-kbd-macro (kbd "i SPC S C C <return> f o o b a r <tab>"))
  (tests-utils--multiline
   ""
   "##_|_"
   "")
  (tests-utils--multiline
   ""
   "{-# SCC \"foobar\" #-}_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-2
    (execute-kbd-macro (kbd "i SPC unpack <return>"))
  (tests-utils--multiline
   ""
   "##_|_"
   "")
  (tests-utils--multiline
   ""
   "{-# UNPACK #-}_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-3
    (execute-kbd-macro (kbd "i SPC i n line <return> foobar <tab>"))
  (tests-utils--multiline
   ""
   "##_|_"
   "")
  (tests-utils--multiline
   ""
   "{-# INLINE foobar #-}_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-4
    (execute-kbd-macro (kbd "i SPC language <return> overstr <return> <tab>"))
  (tests-utils--multiline
   ""
   "##_|_"
   "")
  (tests-utils--multiline
   ""
   "{-# LANGUAGE OverloadedStrings #-}_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-5
    (execute-kbd-macro (kbd "i SPC i n l <return> <tab>"))
  (tests-utils--multiline
   ""
   "##_|_"
   "foo :: a -> a"
   "foo x = x"
   "")
  (tests-utils--multiline
   ""
   "{-# INLINE foo #-}_|_"
   "foo :: a -> a"
   "foo x = x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-5a
    (execute-kbd-macro (kbd "i SPC i n l <return> <tab>"))
  (tests-utils--multiline
   ""
   "  ##_|_"
   "foo :: a -> a"
   "foo x = x"
   "")
  (tests-utils--multiline
   ""
   "  {-# INLINE foo #-}_|_"
   "foo :: a -> a"
   "foo x = x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-5b
    (execute-kbd-macro (kbd "i SPC i n l <return> <tab>"))
  (tests-utils--multiline
   ""
   "\t\t##_|_"
   "foo :: a -> a"
   "foo x = x"
   "")
  (tests-utils--multiline
   ""
   "\t\t{-# INLINE foo #-}_|_"
   "foo :: a -> a"
   "foo x = x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-5c
    (execute-kbd-macro (kbd "i SPC i n l <return> <tab>"))
  (tests-utils--multiline
   ""
   "##_|_"
   "-- Comment"
   "foo :: a -> a"
   "foo x = x"
   "")
  (tests-utils--multiline
   ""
   "{-# INLINE foo #-}_|_"
   "-- Comment"
   "foo :: a -> a"
   "foo x = x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-5d
    (execute-kbd-macro (kbd "i SPC i n l <return> <tab>"))
  (tests-utils--multiline
   ""
   "##_|_"
   "-- Comment 1"
   "-- Comment 2"
   "foo :: a -> a"
   "foo x = x"
   "")
  (tests-utils--multiline
   ""
   "{-# INLINE foo #-}_|_"
   "-- Comment 1"
   "-- Comment 2"
   "foo :: a -> a"
   "foo x = x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-5e
    (execute-kbd-macro (kbd "i SPC i n l <return> <tab>"))
  (tests-utils--multiline
   ""
   "##_|_"
   "-- | Haddock docs 1"
   "foo :: a -> a"
   "foo x = x"
   "")
  (tests-utils--multiline
   ""
   "{-# INLINE foo #-}_|_"
   "-- | Haddock docs 1"
   "foo :: a -> a"
   "foo x = x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-5f
    (execute-kbd-macro (kbd "i SPC i n l <return> <tab>"))
  (tests-utils--multiline
   ""
   "##_|_"
   "-- | Haddock docs 1"
   "-- Haddock docs 2"
   "foo :: a -> a"
   "foo x = x"
   "")
  (tests-utils--multiline
   ""
   "{-# INLINE foo #-}_|_"
   "-- | Haddock docs 1"
   "-- Haddock docs 2"
   "foo :: a -> a"
   "foo x = x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-5g
    (execute-kbd-macro (kbd "i SPC i n l <return> <tab>"))
  (tests-utils--multiline
   ""
   "##_|_"
   "-- | Haddock docs 1"
   "-- Haddock docs 2"
   ""
   "foo :: a -> a"
   "foo x = x"
   "")
  (tests-utils--multiline
   ""
   "{-# INLINE foo #-}_|_"
   "-- | Haddock docs 1"
   "-- Haddock docs 2"
   ""
   "foo :: a -> a"
   "foo x = x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma-5h
    (execute-kbd-macro (kbd "i SPC i n l <return> <tab>"))
  (tests-utils--multiline
   ""
   "##_|_"
   ""
   "-- Comment 1"
   ""
   "-- Comment 2"
   ""
   "foo :: a -> a"
   "foo x = x"
   "")
  (tests-utils--multiline
   ""
   "{-# INLINE foo #-}_|_"
   ""
   "-- Comment 1"
   ""
   "-- Comment 2"
   ""
   "foo :: a -> a"
   "foo x = x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma--no-expansion-if-not-on-empty-line-1
    (execute-kbd-macro (kbd "i SPC <escape>"))
  (tests-utils--multiline
   ""
   "-- ##_|_"
   "foo :: a -> a"
   "foo x = x"
   "")
  (tests-utils--multiline
   ""
   "-- ##_|_ "
   "foo :: a -> a"
   "foo x = x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma--no-expansion-if-not-on-empty-line-1a
    (execute-kbd-macro (kbd "i SPC <escape>"))
  (tests-utils--multiline
   ""
   "x = y ##_|_"
   "")
  (tests-utils--multiline
   ""
   "x = y ##_|_ "
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma--no-expansion-if-not-on-empty-line-1b
    (execute-kbd-macro (kbd "i SPC"))
  (tests-utils--multiline
   ""
   "test = decombobulate"
   "   ##_|_ baz"
   "")
  (tests-utils--multiline
   ""
   "test = decombobulate"
   "   ## _|_ baz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma--no-expansion-if-not-on-empty-line-2
    (execute-kbd-macro (kbd "i # # p r e t t y SPC f o o <escape>"))
  (tests-utils--multiline
   ""
   "foo :: a -> a"
   "foo x = x_|_"
   "")
  (tests-utils--multiline
   ""
   "foo :: a -> a"
   "foo x = x ## pretty fo_|_o"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-abbrev-pragma--expands-in-instance-1
    (execute-kbd-macro (kbd "i # # SPC o v e r l a p p i n g <return>"))
  (tests-utils--multiline
   ""
   "instance _|_ Pretty a => PPGenericOverride a where"
   "  ppGenericOverride = compositeMetaDoc . pretty"
   "")
  (tests-utils--multiline
   ""
   "instance {-# OVERLAPPING #-}_|_ Pretty a => PPGenericOverride a where"
   "  ppGenericOverride = compositeMetaDoc . pretty"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-insert-quote-1
    (execute-kbd-macro (kbd "i '"))
  (tests-utils--multiline
   ""
   "foo x_|_ = do"
   "  bar (x + 1) y"
   "")
  (tests-utils--multiline
   ""
   "foo x'_|_ = do"
   "  bar (x + 1) y"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-insert-quote-2
    (execute-kbd-macro (kbd "i '"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y _|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y '_|_'"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-insert-quote-3
    (execute-kbd-macro (kbd "i '"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y \"_|_\""
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  bar (x + 1) y \"'_|_\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-insert-quote-4
    (execute-kbd-macro (kbd "i '"))
  (tests-utils--multiline
   ""
   "-- _|_"
   "foo x = do"
   "  bar (x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "-- '_|_"
   "foo x = do"
   "  bar (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-insert-quote-5
    (execute-kbd-macro (kbd "i ' f o o ' SPC b a r <escape>"))
  (tests-utils--multiline
   ""
   "-- _|_"
   "foo x = do"
   "  bar (x + 1) y \"foo\""
   "")
  (tests-utils--multiline
   ""
   "-- 'foo' ba_|_r"
   "foo x = do"
   "  bar (x + 1) y \"foo\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-insert-paren-1
    (execute-kbd-macro (kbd "i \( x , y \) - > z <escape>"))
  (tests-utils--multiline
   ""
   "foo | any (\\_|_) = _"
   "")
  (tests-utils--multiline
   ""
   "foo | any (\\(x, y) -> _|_z) = _"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-insert-paren-2
    (execute-kbd-macro (kbd "i \( \\ \( x , y <escape>"))
  (tests-utils--multiline
   ""
   "foo | any _|_ = _"
   "")
  (tests-utils--multiline
   ""
   "foo | any (\\(x, _|_y)) = _"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-move-to-topmost-end-1
    (execute-kbd-macro (kbd "g h"))
  (tests-utils--multiline
   ""
   "foo _|_x ="
   "  bar + 1 +"
   "    baz (quux x)"
   "")
  (tests-utils--multiline
   ""
   "foo x ="
   "  bar + 1 +"
   "    baz (quux x)_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-move-to-topmost-end-2
    (execute-kbd-macro (kbd "g h"))
  (tests-utils--multiline
   ""
   "foo :: Int -> _|_Int"
   "foo x ="
   "  bar + 1 +"
   "    baz (quux x)"
   "")
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   "foo x ="
   "  bar + 1 +"
   "    baz (quux x)_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-move-to-topmost-end-3
    (execute-kbd-macro (kbd "g h"))
  (tests-utils--multiline
   ""
   "foo :: Int -> _|_Int"
   "foo x ="
   ""
   "  bar + 1 +"
   ""
   "    baz (quux x)"
   "")
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   "foo x ="
   ""
   "  bar + 1 +"
   ""
   "    baz (quux x)_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode)
    vim-tests/haskell-move-to-topmost-end-3a
    (execute-kbd-macro (kbd "g h"))
  (tests-utils--multiline
   ""
   "foo :: Int -> _|_Int"
   ""
   "foo x ="
   ""
   "  bar + 1 +"
   ""
   "    baz (quux x)"
   "")
  (tests-utils--multiline
   ""
   "foo :: Int -> Int_|_"
   ""
   "foo x ="
   ""
   "  bar + 1 +"
   ""
   "    baz (quux x)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-move-to-topmost-end-3b
    (execute-kbd-macro (kbd "g h"))
  (tests-utils--multiline
   ""
   "foo :: Int -> _|_Int"
   ""
   "foo x ="
   ""
   "  bar + 1 +"
   ""
   "    baz (quux x)"
   "")
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   ""
   "foo x ="
   ""
   "  bar + 1 +"
   ""
   "    baz (quux x)_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-move-to-topmost-end-4
    (execute-kbd-macro (kbd "g h"))
  (tests-utils--multiline
   ""
   "foo :: Int -> _|_Int"
   "foo x ="
   "#if FOO"
   "  bar + 1 +"
   "#else"
   "  bar + 2 +"
   "#endif"
   "    baz (quux x)"
   "")
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   "foo x ="
   "#if FOO"
   "  bar + 1 +"
   "#else"
   "  bar + 2 +"
   "#endif"
   "    baz (quux x)_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode)
    vim-tests/haskell-move-to-topmost-start-1a
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   "foo x ="
   "#if FOO"
   "  bar + 1 +"
   "#else"
   "  bar + 2 +_|_"
   "#endif"
   "    baz (quux x)"
   "")
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   "_|_foo x ="
   "#if FOO"
   "  bar + 1 +"
   "#else"
   "  bar + 2 +"
   "#endif"
   "    baz (quux x)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-move-to-topmost-start-1b
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   "foo x ="
   "#if FOO"
   "  bar + 1 +"
   "#else"
   "  bar + 2 +_|_"
   "#endif"
   "    baz (quux x)"
   "")
  (tests-utils--multiline
   ""
   "_|_foo :: Int -> Int"
   "foo x ="
   "#if FOO"
   "  bar + 1 +"
   "#else"
   "  bar + 2 +"
   "#endif"
   "    baz (quux x)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode)
    vim-tests/haskell-move-to-topmost-start-2a
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   ""
   "foo x ="
   ""
   "  bar + 1 +"
   ""
   "    baz (quux _|_x)"
   "")
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   ""
   "_|_foo x ="
   ""
   "  bar + 1 +"
   ""
   "    baz (quux x)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-move-to-topmost-start-2b
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   ""
   "foo x ="
   ""
   "  bar + 1 +"
   ""
   "    baz (quux _|_x)"
   "")
  (tests-utils--multiline
   ""
   "_|_foo :: Int -> Int"
   ""
   "foo x ="
   ""
   "  bar + 1 +"
   ""
   "    baz (quux x)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-move-to-topmost-end-position-saving-1
    (execute-kbd-macro (kbd "g h M"))
  (tests-utils--multiline
   ""
   "foo _|_x ="
   "  bar + 1 +"
   "    baz (quux x)"
   "")
  (tests-utils--multiline
   ""
   "foo _|_x ="
   "  bar + 1 +"
   "    baz (quux x)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-move-to-topmost-start-position-saving-1
    (execute-kbd-macro (kbd "g t M"))
  (tests-utils--multiline
   ""
   "foo x ="
   "  bar + 1 +"
   "    baz (quux _|_x)"
   "")
  (tests-utils--multiline
   ""
   "foo x ="
   "  bar + 1 +"
   "    baz (quux _|_x)"
   ""))

(ert-deftest vim-tests/c-open-paren-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (c-mode)
      (execute-kbd-macro (kbd "i ( f o o + 1 ) ; <escape>"))
    (tests-utils--multiline
     ""
     "int main() {"
     "    int foo = 1;"
     "    int bar = baz_|_"
     "}"
     "")
    (tests-utils--multiline
     ""
     "int main() {"
     "    int foo = 1;"
     "    int bar = baz(foo+1)_|_;"
     "}"
     "")))

(ert-deftest vim-tests/rust-open-paren-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (rust-mode)
      (execute-kbd-macro (kbd "i ( f o o + 1 ) ; <escape>"))
    (tests-utils--multiline
     ""
     "fn main() {"
     "    let foo = 1;"
     "    let bar = baz_|_"
     "}"
     "")
    (tests-utils--multiline
     ""
     "fn main() {"
     "    let foo = 1;"
     "    let bar = baz(foo + 1)_|_;"
     "}"
     "")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-cmd-insert-line-below-1
    (execute-kbd-macro (kbd "o f o o <escape>"))
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "import Data._|_List"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "import Data.List"
   "fo_|_o"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-cmd-insert-line-below-2
    (execute-kbd-macro (kbd "o f o o <escape>"))
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-- import Data._|_List"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-- import Data.List"
   "-- fo_|_o"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-cmd-insert-line-below-3
    (execute-kbd-macro (kbd "o f o o <escape>"))
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "--_|_ import Data.List"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-- import Data.List"
   "-- fo_|_o"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-cmd-insert-line-below-4
    (execute-kbd-macro (kbd "o f o o <escape>"))
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-_|_- import Data.List"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-- import Data.List"
   "-- fo_|_o"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-cmd-insert-line-below-5
    (execute-kbd-macro (kbd "o f o o <escape>"))
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "_|_-- import Data.List"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-- import Data.List"
   "-- fo_|_o"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-cmd-insert-line-above-1
    (execute-kbd-macro (kbd "O f o o <escape>"))
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "import Data._|_List"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "fo_|_o"
   "import Data.List"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-cmd-insert-line-above-2
    (execute-kbd-macro (kbd "O f o o <escape>"))
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-- import Data._|_List"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-- fo_|_o"
   "-- import Data.List"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-cmd-insert-line-above-3
    (execute-kbd-macro (kbd "O f o o <escape>"))
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "--_|_ import Data.List"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-- fo_|_o"
   "-- import Data.List"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-cmd-insert-line-above-4
    (execute-kbd-macro (kbd "O f o o <escape>"))
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-_|_- import Data.List"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-- fo_|_o"
   "-- import Data.List"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-cmd-insert-line-above-5
    (execute-kbd-macro (kbd "O f o o <escape>"))
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "_|_-- import Data.List"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "-- fo_|_o"
   "-- import Data.List"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-uncomment-region-1
    (execute-kbd-macro (kbd "j c u"))
  (tests-utils--multiline
   ""
   "-- abc"
   ""
   "-- def"
   "--  xyz"
   "-- fo_|_o"
   "-- bar"
   "")
  (tests-utils--multiline
   ""
   "-- abc"
   ""
   "def"
   " xyz"
   "fo_|_o"
   "bar"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-uncomment-region-2
    (execute-kbd-macro (kbd "j c u"))
  (tests-utils--multiline
   ""
   "  -- abc"
   ""
   "  -- def"
   "  --  xyz"
   "  -- fo_|_o"
   "  -- bar"
   "")
  (tests-utils--multiline
   ""
   "  -- abc"
   ""
   "  def"
   "   xyz"
   "  fo_|_o"
   "  bar"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-uncomment-selected-region-1
    (execute-kbd-macro (kbd "h V h h j c u"))
  (tests-utils--multiline
   ""
   "-- _|_abc"
   "--  def"
   "-- xyz"
   "-- foo"
   "-- bar"
   "")
  (tests-utils--multiline
   ""
   "-- abc"
   " def"
   "xyz"
   "_|_foo"
   "-- bar"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell-uncomment-selected-region-2
    (execute-kbd-macro (kbd "h V h h j c u"))
  (tests-utils--multiline
   ""
   "  -- _|_abc"
   "  --  def"
   "  -- xyz"
   "  -- foo"
   "  -- bar"
   "")
  (tests-utils--multiline
   ""
   "  -- abc"
   "   def"
   "  xyz"
   "  _|_foo"
   "  -- bar"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-1
    (execute-kbd-macro (kbd "i \("))
  (tests-utils--multiline "" "foo_|_bar" "")
  (tests-utils--multiline "" "foo (_|_) bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-1a
    (execute-kbd-macro (kbd "i \("))
  (tests-utils--multiline "" "foo _|_bar" "")
  (tests-utils--multiline "" "foo (_|_) bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-1b
    (execute-kbd-macro (kbd "i \("))
  (tests-utils--multiline "" "foo_|_ bar" "")
  (tests-utils--multiline "" "foo (_|_) bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-1c
    (execute-kbd-macro (kbd "i \("))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo (_|_) bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-2
    (execute-kbd-macro (kbd "i \["))
  (tests-utils--multiline "" "foo_|_bar" "")
  (tests-utils--multiline "" "foo [_|_] bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-2a
    (execute-kbd-macro (kbd "i \["))
  (tests-utils--multiline "" "foo _|_bar" "")
  (tests-utils--multiline "" "foo [_|_] bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-2b
    (execute-kbd-macro (kbd "i \["))
  (tests-utils--multiline "" "foo_|_ bar" "")
  (tests-utils--multiline "" "foo [_|_] bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-2c
    (execute-kbd-macro (kbd "i \["))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo [_|_] bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-3
    (execute-kbd-macro (kbd "i \{"))
  (tests-utils--multiline "" "foo_|_bar" "")
  (tests-utils--multiline "" "foo{_|_}bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-3a
    (execute-kbd-macro (kbd "i \{"))
  (tests-utils--multiline "" "foo _|_bar" "")
  (tests-utils--multiline "" "foo {_|_}bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-3b
    (execute-kbd-macro (kbd "i \{"))
  (tests-utils--multiline "" "foo_|_ bar" "")
  (tests-utils--multiline "" "foo{_|_} bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-3c
    (execute-kbd-macro (kbd "i \{"))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo {_|_} bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-3
    (execute-kbd-macro (kbd "i \{"))
  (tests-utils--multiline "" "foo_|_bar" "")
  (tests-utils--multiline "" "foo {_|_} bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-3a
    (execute-kbd-macro (kbd "i \{"))
  (tests-utils--multiline "" "foo _|_bar" "")
  (tests-utils--multiline "" "foo {_|_} bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-3b
    (execute-kbd-macro (kbd "i \{"))
  (tests-utils--multiline "" "foo_|_ bar" "")
  (tests-utils--multiline "" "foo {_|_} bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-3c
    (execute-kbd-macro (kbd "i \{"))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo {_|_} bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-4
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline "" "foo_|_bar" "")
  (tests-utils--multiline "" "foo \"_|_\" bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-4a
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline "" "foo _|_bar" "")
  (tests-utils--multiline "" "foo \"_|_\" bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-4b
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline "" "foo_|_ bar" "")
  (tests-utils--multiline "" "foo \"_|_\" bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode emacs-lisp-mode)
    vim-tests/pseudoparedit-4c
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo \"_|_\" bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-5
    (execute-kbd-macro (kbd "i \("))
  (tests-utils--multiline "" "foo_|_bar" "")
  (tests-utils--multiline "" "foo (_|_) bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-5a
    (execute-kbd-macro (kbd "i \("))
  (tests-utils--multiline "" "foo _|_bar" "")
  (tests-utils--multiline "" "foo (_|_) bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-5b
    (execute-kbd-macro (kbd "i \("))
  (tests-utils--multiline "" "foo_|_ bar" "")
  (tests-utils--multiline "" "foo (_|_) bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/pseudoparedit-5c
    (execute-kbd-macro (kbd "i \("))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo (_|_) bar" ""))


(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode)
    vim-tests/pseudoparedit-6
    (execute-kbd-macro (kbd "i \["))
  (tests-utils--multiline "" "foo_|_bar" "")
  (tests-utils--multiline "" "foo[_|_]bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode)
    vim-tests/pseudoparedit-6a
    (execute-kbd-macro (kbd "i \["))
  (tests-utils--multiline "" "foo _|_bar" "")
  (tests-utils--multiline "" "foo [_|_]bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode)
    vim-tests/pseudoparedit-6b
    (execute-kbd-macro (kbd "i \["))
  (tests-utils--multiline "" "foo_|_ bar" "")
  (tests-utils--multiline "" "foo[_|_] bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode)
    vim-tests/pseudoparedit-6c
    (execute-kbd-macro (kbd "i \["))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo [_|_] bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode)
    vim-tests/pseudoparedit-8
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline "" "foo_|_bar" "")
  (tests-utils--multiline "" "foo\"_|_\"bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode)
    vim-tests/pseudoparedit-8a
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline "" "foo _|_bar" "")
  (tests-utils--multiline "" "foo \"_|_\"bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode)
    vim-tests/pseudoparedit-8b
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline "" "foo_|_ bar" "")
  (tests-utils--multiline "" "foo\"_|_\" bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode)
    vim-tests/pseudoparedit-8c
    (execute-kbd-macro (kbd "i \""))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo \"_|_\" bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode c-mode rust-mode)
    vim-tests/pseudoparedit-8d
    (execute-kbd-macro (kbd "i \" x \""))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo \"x\"_|_ bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/pseudoparedit-backspace-deletes-pairs-1
    (pseudoparedit-backspace)
  (tests-utils--multiline "" "foo (_|_) bar" "")
  (tests-utils--multiline "" "foo _|_ bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/pseudoparedit-backspace-deletes-pairs-2
    (pseudoparedit-backspace)
  (tests-utils--multiline "" "foo (_|_ ) bar" "")
  (tests-utils--multiline "" "foo _|_ bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/pseudoparedit-backspace-deletes-pairs-3
    (pseudoparedit-backspace)
  (tests-utils--multiline "" "foo [_|_] bar" "")
  (tests-utils--multiline "" "foo _|_ bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/pseudoparedit-backspace-deletes-pairs-4
    (pseudoparedit-backspace)
  (tests-utils--multiline "" "foo {_|_} bar" "")
  (tests-utils--multiline "" "foo _|_ bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/pseudoparedit-backspace-deletes-pairs-5
    (pseudoparedit-backspace)
  (tests-utils--multiline "" "foo \"_|_\" bar" "")
  (tests-utils--multiline "" "foo _|_ bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-1
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = _|_0 + bar" "")
  (tests-utils--multiline "" "foo = 1_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-2
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = _|_0 + bar" "")
  (tests-utils--multiline "" "foo = 1_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-3
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = 0_|_0 + bar" "")
  (tests-utils--multiline "" "foo = 1_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-4
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = 0_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 2_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-5
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = _|_11 + bar" "")
  (tests-utils--multiline "" "foo = 12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-6
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = 1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-7
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = 11_|_ + bar" "")
  (tests-utils--multiline "" "foo = 12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-8
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = -11_|_ + bar" "")
  (tests-utils--multiline "" "foo = -10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-9
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = -1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = -10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-10
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = -_|_11 + bar" "")
  (tests-utils--multiline "" "foo = -10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-11
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = _|_-11 + bar" "")
  (tests-utils--multiline "" "foo = -10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-12
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = +11_|_ + bar" "")
  (tests-utils--multiline "" "foo = +12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-13
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = +1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = +12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-14
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = +_|_11 + bar" "")
  (tests-utils--multiline "" "foo = +12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-15
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = _|_+11 + bar" "")
  (tests-utils--multiline "" "foo = +12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-16
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = _|_+11.2e-5 + bar" "")
  (tests-utils--multiline "" "foo = +12_|_.2e-5 + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-17
    (execute-kbd-macro (kbd "+"))
  (tests-utils--multiline "" "foo = +11_|_.2e-5 + bar" "")
  (tests-utils--multiline "" "foo = +12_|_.2e-5 + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-is-repeatable-1
    (execute-kbd-macro (kbd "+ ."))
  (tests-utils--multiline "" "foo = 1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 13_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-takes-numeric-arg-1
    (execute-kbd-macro (kbd "5 +"))
  (tests-utils--multiline "" "foo = 1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 16_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-takes-numeric-arg-2
    (execute-kbd-macro (kbd "5 + ."))
  (tests-utils--multiline "" "foo = 1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 21_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-at-point-takes-numeric-arg-3
    (execute-kbd-macro (kbd "5 + 4 ."))
  (tests-utils--multiline "" "foo = 1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 36_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-region-1
    (execute-kbd-macro (kbd "v +"))
  (tests-utils--multiline "" "foo = 1_|_9 + bar" "")
  (tests-utils--multiline "" "foo = 110_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/increment-region-2
    (execute-kbd-macro (kbd "v n +"))
  (tests-utils--multiline "" "foo = foo-_|_19 + bar" "")
  (tests-utils--multiline "" "foo = foo-20_|_ + bar"  ""))



(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-1
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = _|_0 + bar" "")
  (tests-utils--multiline "" "foo = -1_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-2
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = _|_0 + bar" "")
  (tests-utils--multiline "" "foo = -1_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-3
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = 0_|_0 + bar" "")
  (tests-utils--multiline "" "foo = -1_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-4
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = 0_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 0_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-5
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = _|_11 + bar" "")
  (tests-utils--multiline "" "foo = 10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-6
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = 1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-7
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = 11_|_ + bar" "")
  (tests-utils--multiline "" "foo = 10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-8
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = -11_|_ + bar" "")
  (tests-utils--multiline "" "foo = -12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-9
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = -1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = -12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-10
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = -_|_11 + bar" "")
  (tests-utils--multiline "" "foo = -12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-11
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = _|_-11 + bar" "")
  (tests-utils--multiline "" "foo = -12_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-12
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = +11_|_ + bar" "")
  (tests-utils--multiline "" "foo = +10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-13
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = +1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = +10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-14
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = +_|_11 + bar" "")
  (tests-utils--multiline "" "foo = +10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-15
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = _|_+11 + bar" "")
  (tests-utils--multiline "" "foo = +10_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-16
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = _|_+11.2e-5 + bar" "")
  (tests-utils--multiline "" "foo = +10_|_.2e-5 + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-17
    (execute-kbd-macro (kbd "="))
  (tests-utils--multiline "" "foo = +11_|_.2e-5 + bar" "")
  (tests-utils--multiline "" "foo = +10_|_.2e-5 + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-is-repeatable-1
    (execute-kbd-macro (kbd "= ."))
  (tests-utils--multiline "" "foo = 1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 9_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-takes-numeric-arg-1
    (execute-kbd-macro (kbd "5 ="))
  (tests-utils--multiline "" "foo = 1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 6_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-takes-numeric-arg-2
    (execute-kbd-macro (kbd "5 = ."))
  (tests-utils--multiline "" "foo = 1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = 1_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-at-point-takes-numeric-arg-3
    (execute-kbd-macro (kbd "5 = 4 ."))
  (tests-utils--multiline "" "foo = 1_|_1 + bar" "")
  (tests-utils--multiline "" "foo = -14_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-region-1
    (execute-kbd-macro (kbd "v ="))
  (tests-utils--multiline "" "foo = 1_|_9 + bar" "")
  (tests-utils--multiline "" "foo = 18_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/decrement-region-2
    (execute-kbd-macro (kbd "v n ="))
  (tests-utils--multiline "" "foo = foo-_|_19 + bar" "")
  (tests-utils--multiline "" "foo = foo-18_|_ + bar"  ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/ex-align-1
    (execute-kbd-macro (kbd "V h h s a / = <return>"))
  (tests-utils--multiline
   ""
   "a     = 1_|_ = 10"
   "bar = 2        = 20"
   "quux = 3=30"
   "")
  (tests-utils--multiline
   ""
   "a    = 1 = 10"
   "bar  = 2 = 20"
   "quux = 3 =_|_30"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-equivalent-commands
    ((vim-tests/ex-align-2 (execute-kbd-macro (kbd "V h h s a / = / n <return>")))
     (vim-tests/ex-align-3 (execute-kbd-macro (kbd "V h h s a / \\ ( SPC * \\ ) = / n <return>"))))
  (tests-utils--multiline
   ""
   "_|_a     = 1 = 10"
   "bar = 2        = 20"
   "quux = 3=30"
   "")
  (tests-utils--multiline
   ""
   "a    = 1 = 10"
   "bar  = 2        = 20"
   "_|_quux = 3=30"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/ex-align-4
    (execute-kbd-macro (kbd "V h h s a / = \\ ( SPC * \\ ) / <return>"))
  (tests-utils--multiline
   ""
   "_|_a     = 1 = 10"
   "bar = 2        = 20"
   "quux = 3=30"
   "")
  (tests-utils--multiline
   ""
   "a     = 1 =        10"
   "bar =   2        = 20"
   "_|_quux =  3=         30"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/cmd-shift-right-1
    (execute-kbd-macro (kbd "> >"))
  (tests-utils--multiline
   ""
   "foo"
   " bar"
   "_|_  baz"
   " quux"
   "")
  (tests-utils--multiline
   ""
   "foo"
   " bar"
   "_|_    baz"
   " quux"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/cmd-shift-right-2
    (execute-kbd-macro (kbd "h v h >"))
  (tests-utils--multiline
   ""
   "_|_foo"
   " bar"
   "  baz"
   " quux"
   "")
  (tests-utils--multiline
   ""
   "foo"
   "   bar"
   "_|_    baz"
   " quux"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/cmd-shift-right-3
    (execute-kbd-macro (kbd "h v h > > >"))
  (tests-utils--multiline
   ""
   "_|_foo"
   " bar"
   "  baz"
   " quux"
   "")
  (tests-utils--multiline
   ""
   "foo"
   "       bar"
   "_|_        baz"
   " quux"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/cmd-shift-left-1
    (execute-kbd-macro (kbd "< <"))
  (tests-utils--multiline
   ""
   "foo"
   "_|_       bar"
   "        baz"
   " quux"
   "")
  (tests-utils--multiline
   ""
   "foo"
   "_|_     bar"
   "        baz"
   " quux"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/cmd-shift-left-2
    (execute-kbd-macro (kbd "V h <"))
  (tests-utils--multiline
   ""
   "foo"
   "_|_       bar"
   "        baz"
   " quux"
   "")
  (tests-utils--multiline
   ""
   "foo"
   "     bar"
   "_|_      baz"
   " quux"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/cmd-shift-left-3
    (execute-kbd-macro (kbd "V h < <"))
  (tests-utils--multiline
   ""
   "foo"
   "_|_       bar"
   "        baz"
   " quux"
   "")
  (tests-utils--multiline
   ""
   "foo"
   "   bar"
   "_|_    baz"
   " quux"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (text-mode)
    vim-tests/motion-down-1
    (execute-kbd-macro (kbd "h"))
  (tests-utils--multiline
   ""
   "\t\t_|_foo"
   "\t\tbar"
   "\t\tbaz"
   "")
  (tests-utils--multiline
   ""
   "\t\tfoo"
   "\t\t_|_bar"
   "\t\tbaz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-hsc-mode)
    vim-tests/haskell-hsc-pragma-insertion
    (execute-kbd-macro (kbd "i # \{ c o n s t"))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo #{const_|_} bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/regular-haskell-pragma-insertion
    (execute-kbd-macro (kbd "i # \{ c o n s t"))
  (tests-utils--multiline "" "foo _|_ bar" "")
  (tests-utils--multiline "" "foo # {const_|_} bar" ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-1
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo _|_bar = putStrLn $ bar ++ \"... OK\" "
   "")
  (tests-utils--multiline
   ""
   "foo bar = putStrLn $ bar_|_ ++ \"... OK\" "
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-2
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo _|_bar = putStrLn $ bar' ++ bar ++ \"... OK\" "
   "  where"
   "    bar' = \"decombobulate\""
   "")
  (tests-utils--multiline
   ""
   "foo bar = putStrLn $ bar' ++ bar_|_ ++ \"... OK\" "
   "  where"
   "    bar' = \"decombobulate\""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-3
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo _|_x=x:Node(x-1)"
   "")
  (tests-utils--multiline
   ""
   "foo x=x_|_:Node(x-1)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-4
    (execute-kbd-macro (kbd "* u"))
  (tests-utils--multiline
   ""
   "foo _|_x=x:Node(x-1)"
   "")
  (tests-utils--multiline
   ""
   "foo x=x:Node(x_|_-1)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-5
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "data Node = N_|_ode Int"
   ""
   "foo x=x:Node(x-1)"
   "")
  (tests-utils--multiline
   ""
   "data Node = Node Int"
   ""
   "foo x=x:Node_|_(x-1)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-6
    (execute-kbd-macro (kbd "#"))
  (tests-utils--multiline
   ""
   "data Node = Node Int"
   ""
   "foo x=x:No_|_de(x-1)"
   "")
  (tests-utils--multiline
   ""
   "data Node = _|_Node Int"
   ""
   "foo x=x:Node(x-1)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-7
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo x=x_|_:Node(x:xs)"
   "")
  (tests-utils--multiline
   ""
   "foo x=x:Node(x:_|_xs)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-8
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo x = Foo.Bar.de_|_combobulate x + decombobulate y"
   "  where"
   "    y = Foo.Bar.decombobulate (x + x)"
   "")
  (tests-utils--multiline
   ""
   "foo x = Foo.Bar.decombobulate x + decombobulate y"
   "  where"
   "    y = Foo.Bar.decombobulate_|_ (x + x)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-9
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "import Codec.CBOR.Read qualified as _|_CBOR"
   ""
   "foo :: CBOR.DeserialiseFailure -> IO a"
   "foo = undefined"
   "")
  (tests-utils--multiline
   ""
   "import Codec.CBOR.Read qualified as CBOR"
   ""
   "foo :: CBOR_|_.DeserialiseFailure -> IO a"
   "foo = undefined"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-9a
    (execute-kbd-macro (kbd "* u"))
  (tests-utils--multiline
   ""
   "import Codec.CBOR.Read qualified as _|_CBOR"
   ""
   "foo :: CBOR.DeserialiseFailure -> IO a"
   "foo = undefined"
   "")
  (tests-utils--multiline
   ""
   "import Codec.CBOR_|_.Read qualified as CBOR"
   ""
   "foo :: CBOR.DeserialiseFailure -> IO a"
   "foo = undefined"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-10
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo :: Id 'Fo_|_o -> Int"
   "foo = undefined"
   ""
   "bar :: Id 'Foo -> Int"
   "bar = undefined"
   "")
  (tests-utils--multiline
   ""
   "foo :: Id 'Foo -> Int"
   "foo = undefined"
   ""
   "bar :: Id 'Foo_|_ -> Int"
   "bar = undefined"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-10a
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo :: Id Fo_|_o' -> Int"
   "foo = undefined"
   ""
   "baz :: Foo -> Int"
   "baz = undefined"
   ""
   "bar :: Id Foo' -> Int"
   "bar = undefined"
   "")
  (tests-utils--multiline
   ""
   "foo :: Id Foo' -> Int"
   "foo = undefined"
   ""
   "baz :: Foo -> Int"
   "baz = undefined"
   ""
   "bar :: Id Foo'_|_ -> Int"
   "bar = undefined"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-10b
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo :: Id 'Fo_|_o -> Int"
   "foo = undefined"
   ""
   "bar :: Foo -> Int"
   "bar = undefined"
   "")
  (tests-utils--multiline
   ""
   "foo :: Id 'Foo -> Int"
   "foo = undefined"
   ""
   "bar :: Foo_|_ -> Int"
   "bar = undefined"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-10c
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo :: Id 'Fo_|_o -> Int"
   "foo = undefined"
   ""
   "baz :: Foo' -> Int"
   "baz = undefined"
   ""
   "bar :: Foo -> Int"
   "bar = undefined"
   "")
  (tests-utils--multiline
   ""
   "foo :: Id 'Foo -> Int"
   "foo = undefined"
   ""
   "baz :: Foo' -> Int"
   "baz = undefined"
   ""
   "bar :: Foo_|_ -> Int"
   "bar = undefined"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-10d
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo :: Id ''Fo_|_o -> Int"
   "foo = undefined"
   ""
   "bar :: Id ''Foo -> Int"
   "bar = undefined"
   "")
  (tests-utils--multiline
   ""
   "foo :: Id ''Foo -> Int"
   "foo = undefined"
   ""
   "bar :: Id ''Foo_|_ -> Int"
   "bar = undefined"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-10e
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo :: Id ''Fo_|_o -> Int"
   "foo = undefined"
   ""
   "bar :: Id 'Foo -> Int"
   "bar = undefined"
   "")
  (tests-utils--multiline
   ""
   "foo :: Id ''Foo -> Int"
   "foo = undefined"
   ""
   "bar :: Id 'Foo_|_ -> Int"
   "bar = undefined"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--search-for-haskell-symbol-at-point-11
    (execute-kbd-macro (kbd "*"))
  (tests-utils--multiline
   ""
   "foo :: XXX -> Int"
   "foo x = x + _|_1000"
   ""
   "bar :: Int"
   "bar = 1000"
   "")
  (tests-utils--multiline
   ""
   "foo :: XXX -> Int"
   "foo x = x + 1000"
   ""
   "bar :: Int"
   "bar = 1000_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-pragma-1
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "_|_{-# INLINE foo #-}"
   "")
  (tests-utils--multiline
   ""
   "{-# INLINE foo #-_|_}"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-pragma-2
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "data AlexInput = AlexInput"
   "  { aiInput    :: !Text"
   "  , aiPrevChar :: _|_{-# UNPACK #-} !Char"
   "  , aiBytes    :: [Word8]"
   "  , aiLine     :: {-# UNPACK #-} !Line"
   "  , aiColumn   :: {-# UNPACK #-} !Column"
   "  } deriving (Show, Eq, Ord)"
   "")
  (tests-utils--multiline
   ""
   "data AlexInput = AlexInput"
   "  { aiInput    :: !Text"
   "  , aiPrevChar :: {-# UNPACK #-_|_} !Char"
   "  , aiBytes    :: [Word8]"
   "  , aiLine     :: {-# UNPACK #-} !Line"
   "  , aiColumn   :: {-# UNPACK #-} !Column"
   "  } deriving (Show, Eq, Ord)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-char-1
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "foo x = _|_(bar ')' + 1)"
   "")
  (tests-utils--multiline
   ""
   "foo x = (bar ')' + 1_|_)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-string-1
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "foo x = _|_(bar \")\" + 1)"
   "")
  (tests-utils--multiline
   ""
   "foo x = (bar \")\" + 1_|_)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-multiline-string-1
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "foo = _|_(bar 41 \" )bb\\"
   "              \\ aa\" + 1)"
   "")
  (tests-utils--multiline
   ""
   "foo = (bar 41 \" )bb\\"
   "              \\ aa\" + 1_|_)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-multiline-string-2
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "foo = _|_(bar 41 \" bb\\"
   "              \\ )aa\" + 1)"
   "")
  (tests-utils--multiline
   ""
   "foo = (bar 41 \" bb\\"
   "              \\ )aa\" + 1_|_)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-comment-1
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "foo x = _|_(bar 41 {- ) -} + 1)"
   "")
  (tests-utils--multiline
   ""
   "foo x = (bar 41 {- ) -} + 1_|_)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-comment-2
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "foo x = _|_(bar 41 -- )"
   "              + 1)"
   "")
  (tests-utils--multiline
   ""
   "foo x = (bar 41 -- )"
   "              + 1_|_)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-comment-3
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "-- _|_(foo bar)"
   "")
  (tests-utils--multiline
   ""
   "-- (foo bar_|_)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-comment-4
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "-- (foo bar_|_)"
   "")
  (tests-utils--multiline
   ""
   "-- _|_(foo bar)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-comment-5
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "test3 x = () (bar 41 -- )"
   "               + 1)"
   ""
   "  {-# INLINE foo #-_|_}"
   "")
  (tests-utils--multiline
   ""
   "test3 x = () (bar 41 -- )"
   "               + 1)"
   ""
   "  _|_{-# INLINE foo #-}"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-quasiquote-1
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "foo x = _|_(bar [megachar| ) |] + 1)"
   "")
  (tests-utils--multiline
   ""
   "foo x = (bar [megachar| ) |] + 1_|_)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-mode haskell-ts-mode)
    vim-tests/haskell--motion-jump-item-quoting-quasiquote-2
    (execute-kbd-macro (kbd "m"))
  (tests-utils--multiline
   ""
   "foo x = _|_[f| 1 + x|] + 1"
   "")
  (tests-utils--multiline
   ""
   "foo x = [f| 1 + x|_|_] + 1"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-rename-at-point-1
    (execute-kbd-macro (kbd "C-r y z <return>"))
  (tests-utils--multiline
   ""
   "test :: forall a. [a] -> [(a, a)]"
   "test _|_x = do"
   "  y <- frobnicator"
   "  pure $ x + y + x"
   "")
  (tests-utils--multiline
   ""
   "test :: forall a. [a] -> [(a, a)]"
   "test xyz_|_ = do"
   "  y <- frobnicator"
   "  pure $ xyz + y + xyz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-rename-at-point-2
    (execute-kbd-macro (kbd "C-r C-w a b c <return>"))
  (tests-utils--multiline
   ""
   "test :: forall a. [a] -> [(a, a)]"
   "test _|_x = do"
   "  y <- frobnicator"
   "  pure $ x + y + x"
   "")
  (tests-utils--multiline
   ""
   "test :: forall a. [a] -> [(a, a)]"
   "test abc_|_ = do"
   "  y <- frobnicator"
   "  pure $ abc + y + abc"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-rename-at-point-3
    (execute-kbd-macro (kbd "C-r b c <return>"))
  (tests-utils--multiline
   ""
   "test :: forall a. [a] -> [(a, a)]"
   "test x = do"
   "  y <- frobnicator"
   "  pure $ (x :: _|_a) + y + x"
   "")
  (tests-utils--multiline
   ""
   "test :: forall abc. [abc] -> [(abc, abc)]"
   "test x = do"
   "  y <- frobnicator"
   "  pure $ (x :: abc_|_) + y + x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-rename-at-point-4
    (execute-kbd-macro (kbd "C-r b c <return>"))
  (tests-utils--multiline
   ""
   "test :: forall a. [a] -> [(a, a)]"
   "-- inconvenient comment"
   "test x = do"
   "  y <- frobnicator"
   "  pure $ (x :: _|_a) + y + x"
   "")
  (tests-utils--multiline
   ""
   "test :: forall abc. [abc] -> [(abc, abc)]"
   "-- inconvenient comment"
   "test x = do"
   "  y <- frobnicator"
   "  pure $ (x :: abc_|_) + y + x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-rename-at-point-5
    (execute-kbd-macro (kbd "C-r b c <return>"))
  (tests-utils--multiline
   ""
   "test :: forall a. [_|_a] -> [(a, a)]"
   "-- inconvenient comment"
   "test x = do"
   "  y <- frobnicator"
   "  pure $ (x :: a) + y + x"
   "")
  (tests-utils--multiline
   ""
   "test :: forall abc. [abc_|_] -> [(abc, abc)]"
   "-- inconvenient comment"
   "test x = do"
   "  y <- frobnicator"
   "  pure $ (x :: abc) + y + x"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-rename-at-point-6
    (execute-kbd-macro (kbd "C-r b c <return>"))
  (tests-utils--multiline
   ""
   "data Foo a = Foo"
   "  { foo :: (Bar Int)"
   "  , bar :: (_|_a, Double)"
   "  , baz :: {-# UNPACK #-} !Double"
   "  }"
   "")
  (tests-utils--multiline
   ""
    "data Foo abc = Foo"
    "  { foo :: (Bar Int)"
    "  , bar :: (abc_|_, Double)"
    "  , baz :: {-# UNPACK #-} !Double"
    "  }"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-1
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "data Foo a = Foo"
   "  { foo :: (Bar Int)"
   "  , bar :: (_|_a, Double)"
   "  , baz :: {-# UNPACK #-} !Double"
   "  }"
   "")
  (tests-utils--multiline
   ""
   "_|_data Foo a = Foo"
   "  { foo :: (Bar Int)"
   "  , bar :: (a, Double)"
   "  , baz :: {-# UNPACK #-} !Double"
   "  }"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-2
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "bar :: Int -> Int"
   "bar 0 = 1"
   "bar n = _|_n * n"
   "")
  (tests-utils--multiline
   ""
   "_|_bar :: Int -> Int"
   "bar 0 = 1"
   "bar n = n * n"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-3
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo x ="
   "#if FOO"
   "  bar + 1 +"
   "#else"
   "  bar + 2 +_|_"
   "#endif"
   "    baz (quux x)")
  (tests-utils--multiline
   "_|_foo :: Int -> Int"
   "foo x ="
   "#if FOO"
   "  bar + 1 +"
   "#else"
   "  bar + 2 +"
   "#endif"
   "    baz (quux x)"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-3a
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = \\x ->"
   "#if FOO"
   "  bar + 1 +"
   "#else"
   "  bar + 2 +_|_"
   "#endif"
   "    baz (quux x)")
  (tests-utils--multiline
   "_|_foo :: Int -> Int"
   "foo = \\x ->"
   "#if FOO"
   "  bar + 1 +"
   "#else"
   "  bar + 2 +"
   "#endif"
   "    baz (quux x)"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-4
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = bar_|_")
  (tests-utils--multiline
   "_|_foo :: Int -> Int"
   "foo = bar"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-5
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   "bar :: Int -> Int -> Int"
   "x `bar` y = foo_|_")
  (tests-utils--multiline
   "_|_bar :: Int -> Int -> Int"
   "x `bar` y = foo"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-5a
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   "bar :: Int -> Int -> Int"
   "0 `bar` y = 100 * y"
   "x `bar` y = foo_|_")
  (tests-utils--multiline
   "_|_bar :: Int -> Int -> Int"
   "0 `bar` y = 100 * y"
   "x `bar` y = foo"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-5b
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   "bar :: Int -> Int -> Int"
   "bar 0 y = 100 * y"
   "x `bar` 2 = foo"
   "bar x y = quux_|_")
  (tests-utils--multiline
   "_|_bar :: Int -> Int -> Int"
   "bar 0 y = 100 * y"
   "x `bar` 2 = foo"
   "bar x y = quux"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-6a
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +_|_"
   "-- #endif"
   "    baz (quux x)")
  (tests-utils--multiline
   "_|_foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-6b
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)_|_")
  (tests-utils--multiline
   "_|_foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-6c
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   "module Foo where"
   ""
   "foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)_|_")
  (tests-utils--multiline
   "module Foo where"
   ""
   "_|_foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-6d
    (execute-kbd-macro (kbd "g t g h"))
  (tests-utils--multiline
   "module Foo where"
   ""
   "foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)_|_")
  (tests-utils--multiline
   "module Foo where"
   ""
   "foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)_|_"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-6e
    (execute-kbd-macro (kbd "g t g h g t"))
  (tests-utils--multiline
   "module Foo where"
   ""
   "foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)_|_")
  (tests-utils--multiline
   "module Foo where"
   ""
   "_|_foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-6f
    (execute-kbd-macro (kbd "g t g h g t"))
  (tests-utils--multiline
   "module Foo where"
   ""
   "foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)_|_"
   "")
  (tests-utils--multiline
   "module Foo where"
   ""
   "_|_foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-6g
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   "module Foo where"
   ""
   "foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)"
   "_|_"
   "")
  (tests-utils--multiline
   "module Foo where"
   ""
   "_|_foo :: Int -> Int"
   "foo = \\x ->"
   "-- #if FOO"
   "  bar + 1 +"
   "-- #else"
   "  bar + 2 +"
   "-- #endif"
   "    baz (quux x)"
   ""
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-7
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   "-- foo"
   "bar n = _|_n * n"
   "")
  (tests-utils--multiline
   ""
   "_|_bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   "-- foo"
   "bar n = n * n"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-7a
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   "bar n = _|_n * n"
   "")
  (tests-utils--multiline
   ""
   "_|_bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   "bar n = n * n"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-7b
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "bar :: Int -> Int"
   "bar 0 = 1 -- baz"
   "bar n = _|_n * n"
   "")
  (tests-utils--multiline
   ""
   "_|_bar :: Int -> Int"
   "bar 0 = 1 -- baz"
   "bar n = n * n"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-7c
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   "-- foo_|_"
   "bar n = n * n"
   "")
  (tests-utils--multiline
   ""
   "_|_bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   "-- foo"
   "bar n = n * n"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-of-defun-7d
    (execute-kbd-macro (kbd "g t"))
  (tests-utils--multiline
   ""
   "bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   "-- foo"
   "-- foo"
   ""
   "-- foo_|_"
   "-- foo"
   "bar n = n * n"
   "")
  (tests-utils--multiline
   ""
   "_|_bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   "-- foo"
   "-- foo"
   ""
   "-- foo"
   "-- foo"
   "bar n = n * n"
   ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands
    vim-tests/haskell-ts-beginning-of-defun-7e
    ((haskell-ts-mode (haskell-ts-mode)))
  ((beginning-of-defun
    (execute-kbd-macro (kbd "g t"))))
  ((a
    (tests-utils--multiline
     ""
     "bar :: Int -> Int"
     "-- bar"
     "bar 0 = 1 -- baz"
     ""
     "-- foo"
     "-- foo"
     ""
     "-- foo"
     "-- foo"
     ""
     "-- foo_|_"
     "-- foo"
     ""
     "bar n = n * n"
     ""))
   (b
    (tests-utils--multiline
     ""
     "bar :: Int -> Int"
     "-- bar"
     "bar 0 = 1 -- baz"
     ""
     "-- foo"
     "-- foo"
     ""
     "-- foo"
     "-- foo"
     ""
     "-- foo"
     "-- foo_|_"
     ""
     "bar n = n * n"
     ""))
   (c
    (tests-utils--multiline
     ""
     "bar :: Int -> Int"
     "-- bar"
     "bar 0 = 1 -- baz"
     ""
     "-- foo"
     "-- foo"
     ""
     "-- foo"
     "-- foo"
     ""
     "_|_-- foo"
     "-- foo"
     ""
     "bar n = n * n"
     ""))
   (d
    (tests-utils--multiline
     ""
     "bar :: Int -> Int"
     "-- bar"
     "bar 0 = 1 -- baz"
     ""
     "-- foo"
     "-- foo"
     ""
     "-- foo"
     "-- foo"
     "_|_"
     "-- foo"
     "-- foo"
     ""
     "bar n = n * n"
     ""))
   (e
    (tests-utils--multiline
     ""
     "bar :: Int -> Int"
     "-- bar"
     "bar 0 = 1 -- baz"
     ""
     "-- foo"
     "-- foo"
     ""
     "-- foo"
     "-- foo"
     ""
     "-- foo"
     "-- foo"
     "_|_"
     "bar n = n * n"
     ""))
   (f
    (tests-utils--multiline
     ""
     "bar :: Int -> Int"
     "-- bar"
     "bar 0 = 1 -- baz"
     ""
     "-- foo"
     "-- foo"
     ""
     "-- foo"
     "-- foo"
     ""
     "-- foo"
     "-- foo"
     ""
     "bar n = n * n_|_"
     "")))
  (tests-utils--multiline
   ""
   "_|_bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   ""
   "-- foo"
   "-- foo"
   ""
   "-- foo"
   "-- foo"
   ""
   "-- foo"
   "-- foo"
   ""
   "bar n = n * n"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-end-of-defun-1
    (execute-kbd-macro (kbd "g h"))
  (tests-utils--multiline
   ""
   "_|_bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   "-- foo"
   "bar n = n * n"
   "")
  (tests-utils--multiline
   ""
   "bar :: Int -> Int"
   "-- bar"
   "bar 0 = 1 -- baz"
   "-- foo"
   "bar n = n * n_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-end-of-defun-1a
    (execute-kbd-macro (kbd "g h"))
  (tests-utils--multiline
   ""
   "_|_bar :: Int -> Int"
   "bar 0 = 1 -- baz"
   "-- foo"
   "bar n = n * n"
   "")
  (tests-utils--multiline
   ""
   "bar :: Int -> Int"
   "bar 0 = 1 -- baz"
   "-- foo"
   "bar n = n * n_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-end-of-defun-1b
    (execute-kbd-macro (kbd "g h"))
  (tests-utils--multiline
   ""
   "_|_bar :: Int -> Int"
   "bar 0 = 1 -- baz"
   "bar n = n * n"
   "")
  (tests-utils--multiline
   ""
   "bar :: Int -> Int"
   "bar 0 = 1 -- baz"
   "bar n = n * n_|_"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-pair-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-and-end-of-defun-1
    (tests-utils--multiline
     "quux :: Int -> Int"
     "quux 0 = 1"
     "quux x = x"
     ""
     "-----------------"
     "-- foo"
     "_|_"
     ""
     "-- | bar"
     "foo :: Int -> Int"
     "foo 0 = 3"
     "foo x = x + x")
  (beginning
   (execute-kbd-macro (kbd "g t"))
   (tests-utils--multiline
    "_|_quux :: Int -> Int"
    "quux 0 = 1"
    "quux x = x"
    ""
    "-----------------"
    "-- foo"
    ""
    ""
    "-- | bar"
    "foo :: Int -> Int"
    "foo 0 = 3"
    "foo x = x + x"))
  (end
   (execute-kbd-macro (kbd "g h"))
   (tests-utils--multiline
    "quux :: Int -> Int"
    "quux 0 = 1"
    "quux x = x"
    ""
    "-----------------"
    "-- foo"
    ""
    ""
    "-- | bar"
    "foo :: Int -> Int"
    "foo 0 = 3"
    "foo x = x + x_|_")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-pair-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-and-end-of-defun-2
    (tests-utils--multiline
     "data Foo"
     "  = Foo Int"
     "  | Bar Foo Foo"
     "  deriving (Eq, Ord, Show)"
     ""
     "-----------------"
     "-- foo"
     "_|_"
     ""
     "-- | bar"
     "foo :: Int -> Int"
     "foo 0 = 3"
     "foo x = x + x")
  (beginning
   (execute-kbd-macro (kbd "g t"))
   (tests-utils--multiline
    "_|_data Foo"
    "  = Foo Int"
    "  | Bar Foo Foo"
    "  deriving (Eq, Ord, Show)"
    ""
    "-----------------"
    "-- foo"
    ""
    ""
    "-- | bar"
    "foo :: Int -> Int"
    "foo 0 = 3"
    "foo x = x + x"))
  (end
   (execute-kbd-macro (kbd "g h"))
   (tests-utils--multiline
    "data Foo"
    "  = Foo Int"
    "  | Bar Foo Foo"
    "  deriving (Eq, Ord, Show)"
    ""
    "-----------------"
    "-- foo"
    ""
    ""
    "-- | bar"
    "foo :: Int -> Int"
    "foo 0 = 3"
    "foo x = x + x_|_")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-pair-only
    (haskell-ts-mode)
    vim-tests/haskell-ts-beginning-and-end-of-defun-3
    (tests-utils--multiline
     "foo :: Int -> Int"
     "foo 0 = 3"
     "foo x = x + x"
     ""
     "-----------------"
     "-- foo"
     "_|_"
     ""
     "-- | bar"
     "data Foo"
     "  = Foo Int"
     "  | Bar Foo Foo"
     "  deriving (Eq, Ord, Show)")
  (beginning
   (execute-kbd-macro (kbd "g t"))
   (tests-utils--multiline
     "_|_foo :: Int -> Int"
     "foo 0 = 3"
     "foo x = x + x"
     ""
     "-----------------"
     "-- foo"
     ""
     ""
     "-- | bar"
     "data Foo"
     "  = Foo Int"
     "  | Bar Foo Foo"
     "  deriving (Eq, Ord, Show)"))
  (end
   (execute-kbd-macro (kbd "g h"))
   (tests-utils--multiline
     "foo :: Int -> Int"
     "foo 0 = 3"
     "foo x = x + x"
     ""
     "-----------------"
     "-- foo"
     ""
     ""
     "-- | bar"
     "data Foo"
     "  = Foo Int"
     "  | Bar Foo Foo"
     "  deriving (Eq, Ord, Show)_|_")))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands-all-known-inits
    vim-tests/substitute-1
  ((yes-confirm (execute-kbd-macro (kbd "s % s / f o o / d e c o m b o b u l a t e / c <return> y y y")))
   (no-confirm (execute-kbd-macro (kbd "s % s / f o o / d e c o m b o b u l a t e / <return>"))))
  ((a
    (tests-utils--multiline
     "_|_foo foo"
     "bar"
     "baz"
     "quux"
     "foo"))
   (b
    (tests-utils--multiline
     "foo_|_ foo"
     "bar"
     "baz"
     "quux"
     "foo"))
   (c
    (tests-utils--multiline
     "foo foo"
     "_|_bar"
     "baz"
     "quux"
     "foo"))
   (d
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "_|_quux"
     "foo"))
   (e
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "quux_|_"
     "foo")
    (f
     (tests-utils--multiline
      "foo foo"
      "bar"
      "baz"
      "quux"
      "_|_foo"))
    (g
     (tests-utils--multiline
      "foo foo"
      "bar"
      "baz"
      "quux"
      "foo_|_"))))
  (tests-utils--multiline
   "decombobulate decombobulate"
   "bar"
   "baz"
   "quux"
   "decombobulate_|_"))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands-all-known-inits
    vim-tests/substitute-2
  ((yes-confirm (execute-kbd-macro (kbd "s % s / f o o / d e c o m b o b u l a t e / c n <return> y y")))
   (no-confirm (execute-kbd-macro (kbd "s % s / f o o / d e c o m b o b u l a t e / n <return>"))))
  ((a
    (tests-utils--multiline
     "_|_foo foo"
     "bar"
     "baz"
     "quux"
     "foo"))
   (b
    (tests-utils--multiline
     "foo_|_ foo"
     "bar"
     "baz"
     "quux"
     "foo"))
   (c
    (tests-utils--multiline
     "foo foo"
     "_|_bar"
     "baz"
     "quux"
     "foo"))
   (d
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "_|_quux"
     "foo"))
   (e
    (tests-utils--multiline
     "foo foo"
     "bar"
     "baz"
     "quux_|_"
     "foo")
    (f
     (tests-utils--multiline
      "foo foo"
      "bar"
      "baz"
      "quux"
      "_|_foo"))
    (g
     (tests-utils--multiline
      "foo foo"
      "bar"
      "baz"
      "quux"
      "foo_|_"))))
  (tests-utils--multiline
   "decombobulate foo"
   "bar"
   "baz"
   "quux"
   "decombobulate_|_"))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands-all-known-inits
    vim-tests/substitute-3
  ((yes-confirm (execute-kbd-macro (kbd "s % s / e / X X X / c n <return> y y")))
   (no-confirm (execute-kbd-macro (kbd "s % s / e / X X X / n <return>"))))
  ((a
    (tests-utils--multiline
     "_|_e e"
     "bar"
     "baz"
     "quux"
     "e"))
   (b
    (tests-utils--multiline
     "e _|_e"
     "bar"
     "baz"
     "quux"
     "e"))
   (c
    (tests-utils--multiline
     "e e_|_"
     "bar"
     "baz"
     "quux"
     "e"))
   (d
    (tests-utils--multiline
     "e e"
     "bar_|_"
     "baz"
     "quux"
     "e")))
  (tests-utils--multiline
   "XXX e"
   "bar"
   "baz"
   "quux"
   "XXX_|_"))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands-all-known-inits
    vim-tests/substitute-4
  ((yes-confirm (execute-kbd-macro (kbd "s % s / e / X X X / c <return> y y y")))
   (no-confirm (execute-kbd-macro (kbd "s % s / e / X X X / <return>"))))
  ((a
    (tests-utils--multiline
     "_|_e e"
     "bar"
     "baz"
     "quux"
     "e"))
   (b
    (tests-utils--multiline
     "e _|_e"
     "bar"
     "baz"
     "quux"
     "e"))
   (c
    (tests-utils--multiline
     "e e_|_"
     "bar"
     "baz"
     "quux"
     "e"))
   (d
    (tests-utils--multiline
     "e e"
     "bar_|_"
     "baz"
     "quux"
     "e")))
  (tests-utils--multiline
   "XXX XXX"
   "bar"
   "baz"
   "quux"
   "XXX_|_"))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands-all-known-inits
    vim-tests/substitute-5
  ((no-confirm (execute-kbd-macro (kbd "V 4 h s s / \\ ( . * <right> <right> / \" \\ 1 \" <return>"))))
  ((a
    (tests-utils--multiline
     ""
     "_|_e e"
     "bar"
     ""
     "quux"
     "e"
     "")))
  (tests-utils--multiline
     ""
     "\"e e\""
     "\"bar\""
     "\"\""
     "\"quux\""
     "\"e\"_|_"
     ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands-all-known-inits
 vim-tests/substitute-6
 ((no-confirm
   (condition-case nil
       (execute-kbd-macro (kbd "s % s / U 1 / P a r 1 / <escape>"))
     (quit t))))
 ((a
   (tests-utils--multiline
    "_|_"
    "- | @since base-4.21.0.0"
    "instance Eq1 U1 where"
    "  liftEq _ = \_ _ -> True"
    ""
    "-- | @since base-4.21.0.0"
    "instance Ord1 U1 where"
    "  liftCompare _ = \_ _ -> EQ"
    ""
    "-- | @since base-4.21.0.0"
    "instance Show1 U1 where"
    "  liftShowsPrec _ _ _ U1 = showString U1"
    ""
    "-- | @since base-4.21.0.0"
    ""
    "instance Read1 U1 where"
    "  liftReadPrec _ _ ="
    "    parens (expectP (Ident U1) *> pure U1)"
    ""
    "  liftReadListPrec  = liftReadListPrecDefault"
    "  liftReadList      = liftReadListDefault"
    "")))
 (tests-utils--multiline
  "_|_"
  "- | @since base-4.21.0.0"
  "instance Eq1 U1 where"
  "  liftEq _ = \_ _ -> True"
  ""
  "-- | @since base-4.21.0.0"
  "instance Ord1 U1 where"
  "  liftCompare _ = \_ _ -> EQ"
  ""
  "-- | @since base-4.21.0.0"
  "instance Show1 U1 where"
  "  liftShowsPrec _ _ _ U1 = showString U1"
  ""
  "-- | @since base-4.21.0.0"
  ""
  "instance Read1 U1 where"
  "  liftReadPrec _ _ ="
  "    parens (expectP (Ident U1) *> pure U1)"
  ""
  "  liftReadListPrec  = liftReadListPrecDefault"
  "  liftReadList      = liftReadListDefault"
  ""))

(vim-tests--test-fresh-buffer-contents-equivalent-inits-and-commands-all-known-inits
    vim-tests/substitute-7
  ((no-confirm (execute-kbd-macro (kbd "s % s / U 1 / P a r 1 / <return>"))))
  ((a
    (tests-utils--multiline
     "_|_"
     "- | @since base-4.21.0.0"
     "instance Eq1 U1 where"
     "  liftEq _ = \\_ _ -> True"
     ""
     "-- | @since base-4.21.0.0"
     "instance Ord1 U1 where"
     "  liftCompare _ = \\_ _ -> EQ"
     ""
     "-- | @since base-4.21.0.0"
     "instance Show1 U1 where"
     "  liftShowsPrec _ _ _ U1 = showString U1"
     ""
     "-- | @since base-4.21.0.0"
     ""
     "instance Read1 U1 where"
     "  liftReadPrec _ _ ="
     "    parens (expectP (Ident U1) *> pure U1)"
     ""
     "  liftReadListPrec  = liftReadListPrecDefault"
     "  liftReadList      = liftReadListDefault"
     "")))
  (tests-utils--multiline
   ""
   "- | @since base-4.21.0.0"
   "instance Eq1 Par1 where"
   "  liftEq _ = \\_ _ -> True"
   ""
   "-- | @since base-4.21.0.0"
   "instance Ord1 Par1 where"
   "  liftCompare _ = \\_ _ -> EQ"
   ""
   "-- | @since base-4.21.0.0"
   "instance Show1 Par1 where"
   "  liftShowsPrec _ _ _ Par1 = showString Par1"
   ""
   "-- | @since base-4.21.0.0"
   ""
   "instance Read1 Par1 where"
   "  liftReadPrec _ _ ="
   "    parens (expectP (Ident Par1) *> pure Par1_|_)"
   ""
   "  liftReadListPrec  = liftReadListPrecDefault"
   "  liftReadList      = liftReadListDefault"
   ""))

(ert-deftest vim-tests/indent-region/c-mode-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (c-mode)
      (execute-kbd-macro (kbd "V m <tab>"))
    (tests-utils--multiline
     ""
     "foo() {"
     "while (1) _|_{"
     "if (condition()) {"
     "break;"
     "}"
     "process();"
     "}"
     "}"
     "")
    (tests-utils--multiline
     ""
     "foo() {"
     "    while (1) {"
     "        if (condition()) {"
     "            break;"
     "        }"
     "        process();"
     "_|_    }"
     "}"
     "")))

(ert-deftest vim-tests/indent-region/c-mode-2 ()
  (vim-tests--test-fresh-buffer-contents-init
      (progn
        (c-mode)
        (indent-tabs-mode +1)
        (setq-local tab-width 4
                    c-basic-offset 4))
      (execute-kbd-macro (kbd "V m <tab>"))
    (tests-utils--multiline
     ""
     "foo() {"
     "while (1) _|_{"
     "if (condition()) {"
     "break;"
     "}"
     "process();"
     "}"
     "}"
     "")
    (tests-utils--multiline
     ""
     "foo() {"
     "\twhile (1) {"
     "\t\tif (condition()) {"
     "\t\t\tbreak;"
     "\t\t}"
     "\t\tprocess();"
     "_|_\t}"
     "}"
     "")))

(provide 'vim-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; vim-tests.el ends here
