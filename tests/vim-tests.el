;; vim-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Thursday,  6 August 2015
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'tests-utils)

(require 'vim)
(require 'vim-search)
(require 'ert)

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

(defmacro vim-tests--test-fresh-buffer-contents-init-all (name inits action contents expected-value)
  (declare (indent 3))
  (cl-assert (symbolp name) "invalid name: %s" name)
  `(progn
     ,@(cl-loop
         for init in inits
         collecting
         (let ((subname (car init))
               (expr (cdr init)))
           (cl-assert (symbolp subname))
           `(ert-deftest ,(string->symbol (format "%s//%s" name subname)) ()
              (tests-utils--test-buffer-contents
               :action ,action
               :contents ,contents
               :expected-value ,expected-value
               :initialisation (progn ,@expr)
               ;; Don’t reuse buffer to start out in fresh environment each time and don’t
               ;; share things like last cmd events, etc.
               :buffer-id nil))))))

(defconst vim-tests--modes-and-init
  '((text-mode (text-mode))
    (haskell-mode (haskell-mode))
    (emacs-lisp-mode (emacs-lisp-mode))
    (rust-mode (rust-mode))
    (c-mode (c-mode))))

;; Text mode has surprising bindings for <tab>. It doesn’t really matter
;; day to day but breaks tests significantly without much benefit testingwise.
(defmacro vim-tests--test-fresh-buffer-contents-init-standard-modes-except (skip-modes name action contents expected-value)
  (declare (indent 3))
  (cl-assert (listp skip-modes))
  (cl-assert (cl-every #'symbolp skip-modes))
  `(vim-tests--test-fresh-buffer-contents-init-all
       ,name
       ,(--remove (memq (car it) skip-modes) vim-tests--modes-and-init)
       ,action
     ,contents
     ,expected-value))

(defmacro vim-tests--test-fresh-buffer-contents-init-standard-modes (name action contents expected-value)
  (declare (indent 2))
  `(vim-tests--test-fresh-buffer-contents-init-all
       ,name
       ,vim-tests--modes-and-init
       ,action
     ,contents
     ,expected-value))

(defmacro vim-tests--test-fresh-buffer-contents (action contents expected-value)
  (declare (indent 1))
  `(vim-tests--test-fresh-buffer-contents-init
       (text-mode)
       ,action
     ,contents
     ,expected-value))

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
                 '("hello" "world\\/g" nil))))

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
   "(foo"
   "   (bar (baz "
   "_|_"))

(vim-tests--test-fresh-buffer-contents-init-standard-modes-except
    (text-mode haskell-mode)
    vim-tests/repeat-vim:splice-sexp-killing-backward-1
    (execute-kbd-macro (kbd "j ( d"))
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
    (text-mode haskell-mode)
    vim-tests/repeat-vim:splice-sexp-killing-backward-2
    (execute-kbd-macro (kbd "j ( d ."))
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

(ert-deftest vim-tests/repeat-vim:splice-sexp-killing-backward-2/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "j ( d"))
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
     "")))

(ert-deftest vim-tests/repeat-vim:splice-sexp-killing-backward-3/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "j ( d ."))
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
     "")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    vim-tests/repeat-vim:splice-sexp-killing-backward-3
    (execute-kbd-macro (kbd "j ( d 2 ."))
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
    vim-tests/block-insert-1b
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
    vim-tests/block-insert-1bb
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
    vim-tests/block-insert-1ca
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
    vim-tests/block-insert-1d
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
    vim-tests/block-insert-1dd
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

(ert-deftest vim-tests/comment-linewise-region-1/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

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
    (text-mode)
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

(vim-tests--test-fresh-buffer-contents-init-standard-modes
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
    (text-mode)
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

(ert-deftest vim-tests/haskell-motion-inner-symbol-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd ", i s"))
    (tests-utils--multiline
     ""
     "foo x = do"
     "  ba_|_r_baz_Quux''' (x + 1) y \"foo\""
     "")
    (tests-utils--multiline
     ""
     "foo x = do"
     "  _|_ (x + 1) y \"foo\""
     "")))

(ert-deftest vim-tests/haskell-motion-inner-symbol-2 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd ", i s"))
    (tests-utils--multiline
     ""
     "foo x = do"
     "  Bar_baz_Quux''_|_' (x + 1) y \"foo\""
     "")
    (tests-utils--multiline
     ""
     "foo x = do"
     "  _|_ (x + 1) y \"foo\""
     "")))

(ert-deftest vim-tests/haskell-motion-outer-symbol-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd ", a s"))
    (tests-utils--multiline
     ""
     "foo x = do"
     "  ba_|_r_baz_Quux''' (x + 1) y \"foo\""
     "")
    (tests-utils--multiline
     ""
     "foo x = do"
     "  _|_(x + 1) y \"foo\""
     "")))

(ert-deftest vim-tests/haskell-motion-outer-symbol-2 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd ", a s"))
    (tests-utils--multiline
     ""
     "foo x = do"
     "  Bar_baz_Quux''_|_' (x + 1) y \"foo\""
     "")
    (tests-utils--multiline
     ""
     "foo x = do"
     "  _|_(x + 1) y \"foo\""
     "")))

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
        (execute-kbd-macro (kbd "j ( d")))
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

(ert-deftest vim-tests/haskell-newline-with-signature-expansion-expand-pragma-pair-1/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i { - # <return> <escape>"))
    (tests-utils--multiline
     ""
     "_|_"
     "")
    (tests-utils--multiline
     "{-# DEPRECATED_|_  #-}"
     ""
     "")))

(ert-deftest vim-tests/paren-insert-1/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i \("))
    (tests-utils--multiline
     ""
     "foo x = _|_"
     "")
    (tests-utils--multiline
     ""
     "foo x = (_|_)"
     "")))

(ert-deftest vim-tests/bracket-insert-1/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i \["))
    (tests-utils--multiline
     ""
     "foo x = _|_"
     "")
    (tests-utils--multiline
     ""
     "foo x = [_|_]"
     "")))

(ert-deftest vim-tests/brace-insert-1/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i \{"))
    (tests-utils--multiline
     ""
     "foo x = _|_"
     "")
    (tests-utils--multiline
     ""
     "foo x = {_|_}"
     "")))

(ert-deftest vim-tests/wrap-paren-1/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i \( C-\) C-\)"))
    (tests-utils--multiline
     ""
     "foo x = _|_bar x"
     "")
    (tests-utils--multiline
     ""
     "foo x = (_|_bar x)"
     "")))

(ert-deftest vim-tests/wrap-paren-2/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "v e e \("))
    (tests-utils--multiline
     ""
     "foo x = _|_bar x"
     "")
    (tests-utils--multiline
     ""
     "foo x = (_|_bar x)"
     "")))

(ert-deftest vim-tests/wrap-bracket-1/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i \[ C-\) C-\)"))
    (tests-utils--multiline
     ""
     "foo x = _|_bar x"
     "")
    (tests-utils--multiline
     ""
     "foo x = [_|_bar x]"
     "")))

(ert-deftest vim-tests/wrap-bracket-2/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "v e e \["))
    (tests-utils--multiline
     ""
     "foo x = _|_bar x"
     "")
    (tests-utils--multiline
     ""
     "foo x = [_|_bar x]"
     "")))

(ert-deftest vim-tests/wrap-brace-1/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i \{ C-\) C-\)"))
    (tests-utils--multiline
     ""
     "foo x = _|_bar x"
     "")
    (tests-utils--multiline
     ""
     "foo x = {_|_bar x}"
     "")))

(ert-deftest vim-tests/wrap-brace-2/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "v e e \{"))
    (tests-utils--multiline
     ""
     "foo x = _|_bar x"
     "")
    (tests-utils--multiline
     ""
     "foo x = {_|_bar x}"
     "")))

(ert-deftest vim-tests/wrap-backtick-1/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i y SPC <escape> w v e `"))
    (tests-utils--multiline
     ""
     "foo x = _|_bar x"
     "")
    (tests-utils--multiline
     ""
     "foo x = y `_|_bar` x"
     "")))

(ert-deftest vim-tests/wrap-dquotes-1/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i y SPC <escape> w v e \""))
    (tests-utils--multiline
     ""
     "foo x = _|_bar x"
     "")
    (tests-utils--multiline
     ""
     "foo x = y \"_|_bar\" x"
     "")))

(ert-deftest vim-tests/wrap-dquotes-2/haskell-mode ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "v E E E \""))
    (tests-utils--multiline
     ""
     "foo x = baz _|_\"bar x\" quux"
     "")
    (tests-utils--multiline
     ""
     "foo x = baz \"_|_\\\"bar x\\\" quux\""
     "")))

(ert-deftest vim-tests/haskell-mode-backward-up-indentation-or-sexp-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-mode-backward-up-indentation-or-sexp-2 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-mode-backward-up-indentation-or-sexp-3 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-mode-backward-up-indentation-or-sexp-4 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-mode-backward-up-indentation-or-sexp-5 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-mode-backward-up-indentation-or-sexp-6 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-mode-backward-up-indentation-or-sexp-7 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-newline-auto-comment-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-newline-auto-comment-2 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-delete-commented-part-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-delete-commented-part-2 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/emacs-lisp-abbrev-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (emacs-lisp-mode)
      (execute-kbd-macro (kbd "i ( d k f m SPC"))
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

(ert-deftest vim-tests/haskell-abbrev-import-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i i m SPC"))
    (tests-utils--multiline
     ""
     "_|_"
     "")
    (tests-utils--multiline
     ""
     "import _|_"
     "")))

(ert-deftest vim-tests/haskell-abbrev-import-2 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i i SPC"))
    (tests-utils--multiline
     ""
     "_|_"
     "")
    (tests-utils--multiline
     ""
     "import _|_"
     "")))

(ert-deftest vim-tests/haskell-abbrev-import-3 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i SPC i SPC = i SPC + i <escape>"))
    (tests-utils--multiline
     ""
     "foo_|_"
     "")
    (tests-utils--multiline
     ""
     "foo i = i + _|_i"
     "")))

(ert-deftest vim-tests/haskell-abbrev-import-4 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i SPC i m SPC = i m SPC + i m <escape>"))
    (tests-utils--multiline
     ""
     "foo_|_"
     "")
    (tests-utils--multiline
     ""
     "foo im = im + i_|_m"
     "")))

(ert-deftest vim-tests/haskell-abbrev-import-5 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i SPC i m p SPC = b a r SPC i m p SPC 1 <escape>"))
    (tests-utils--multiline
     ""
     "foo_|_"
     "")
    (tests-utils--multiline
     ""
     "foo imp = bar imp _|_1"
     "")))

(ert-deftest vim-tests/haskell-abbrev-import-6 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i i q SPC F o o <tab> <escape>"))
    (tests-utils--multiline
     ""
     "_|_"
     "")
    (tests-utils--multiline
     ""
     "import qualified Foo as _|_F"
     "")))

(ert-deftest vim-tests/haskell-abbrev-2 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      ;; Aborting pragma prompt with <escape> causes 'quit signal to be raised.
      ;; If we don’t catch it then ert will not proceed to compare buffer contents.
      (condition-case nil
          (execute-kbd-macro (kbd "i # # SPC <escape> <escape>"))
        (quit t))
    (tests-utils--multiline
     ""
     "_|_"
     "")
    (tests-utils--multiline
     ""
     "{-# _|_  #-}"
     "")))

(ert-deftest vim-tests/haskell-abbrev-3 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      ;; Aborting pragma prompt with <escape> causes 'quit signal to be raised.
      ;; If we don’t catch it then ert will not proceed to compare buffer contents.
      (condition-case nil
          (execute-kbd-macro (kbd "i SPC <escape> <escape>"))
        (quit t))
    (tests-utils--multiline
     ""
     "##    _|_"
     "")
    (tests-utils--multiline
     ""
     "{-# _|_  #-}"
     "")))

(ert-deftest vim-tests/haskell-abbrev-4 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-abbrev-5 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-abbrev-6 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-abbrev-7 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-abbrev-8 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i imports SPC"))
    (tests-utils--multiline
     ""
     "_|_"
     "foo x = do"
     "  bar (x + 1) y \"foo\""
     "")
    (tests-utils--multiline
     ""
     "import Data.Set (Set)"
     "import qualified Data.Set as S _|_"
     "foo x = do"
     "  bar (x + 1) y \"foo\""
     "")))

(ert-deftest vim-tests/haskell-insert-quote-1 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-insert-quote-2 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-insert-quote-3 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-insert-quote-4 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
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
     "")))

(ert-deftest vim-tests/haskell-insert-quote-5 ()
  (vim-tests--test-fresh-buffer-contents-init
      (haskell-mode)
      (execute-kbd-macro (kbd "i ' foo ' SPC b a r <escape>"))
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
     "")))

(provide 'vim-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; vim-tests.el ends here
