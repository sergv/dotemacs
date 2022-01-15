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
  (cl-assert (stringp name) "invalid name: %s" name)
  `(progn
     ,@(cl-loop
         for init in inits
         collecting
         (let ((subname (car init))
               (expr (cdr init)))
           (cl-assert (symbolp subname))
           `(ert-deftest ,(string->symbol (format name subname)) ()
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

(ert-deftest vim-tests/dd-1 ()
  (vim-tests--test-fresh-buffer-contents
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
     "_|_")))

(ert-deftest vim-tests/repeat-vim:sp-splice-sexp-killing-backward-1 ()
  (vim-tests--test-fresh-buffer-contents
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
     "(bar _|_quux))"
     "")))

(ert-deftest vim-tests/repeat-vim:sp-splice-sexp-killing-backward-2 ()
  (vim-tests--test-fresh-buffer-contents
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
     "_|_quux)"
     "")))

(ert-deftest vim-tests/repeat-vim:sp-splice-sexp-killing-backward-3 ()
  (vim-tests--test-fresh-buffer-contents
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
     "_|_quux"
     "")))

(ert-deftest vim-tests/block-insert-1 ()
  (vim-tests--test-fresh-buffer-contents
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
     "frobnicate")))

(ert-deftest vim-tests/block-insert-2 ()
  (vim-tests--test-fresh-buffer-contents
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
     "frobnicate")))

(ert-deftest vim-tests/block-insert-undo-1 ()
  (vim-tests--test-fresh-buffer-contents
      ;; Enable undo tracking.
      (let ((buffer-undo-list nil))
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
     "frobnicate")))

(ert-deftest vim-tests/block-insert-undo-2 ()
  (vim-tests--test-fresh-buffer-contents
   ;; Enable undo tracking.
   (let ((buffer-undo-list nil))
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
    "frobnicate")))

(ert-deftest vim-tests/block-insert-undo-redo-1 ()
  (vim-tests--test-fresh-buffer-contents
      ;; Enable undo tracking.
      (let ((buffer-undo-list nil))
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
     "frobnicate")))

(ert-deftest vim-tests/block-insert-newline-1 ()
  (vim-tests--test-fresh-buffer-contents
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
     "frobnicate")))

(ert-deftest vim-tests/linewise-append-newline-1 ()
  (vim-tests--test-fresh-buffer-contents
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
     "frobnicate")))

(ert-deftest vim-tests/block-append-newline-1 ()
  (vim-tests--test-fresh-buffer-contents
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
     "frobnicate")))

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
    "vim-tests/copy-paste-linewise-region-after-%s-1"
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
    "vim-tests/copy-paste-linewise-region-before-%s-1"
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

(ert-deftest vim-tests/paste-before-1 ()
  (vim-tests--test-fresh-buffer-contents
      (execute-kbd-macro (kbd "y s P"))
    (tests-utils--multiline
     ""
     "foo _|_bar baz"
     "")
    (tests-utils--multiline
     ""
     "foo _|_barbar baz"
     "")))

(ert-deftest vim-tests/paste-after-1 ()
  (vim-tests--test-fresh-buffer-contents
      (execute-kbd-macro (kbd "y s p"))
    (tests-utils--multiline
     ""
     "foo _|_bar baz"
     "")
    (tests-utils--multiline
     ""
     "foo bba_|_rar baz"
     "")))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    "vim-tests/paste-before-visual-block-region-%s-1"
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
    "vim-tests/paste-before-visual-block-region-%s-2"
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
    "vim-tests/paste-before-visual-block-region-%s-3"
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
    "vim-tests/paste-before-visual-block-region-undo-%s-1"
    ;; Enable undo tracking.
    (let ((buffer-undo-list nil))
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
    "vim-tests/paste-before-visual-block-region-undo-%s-2"
    ;; Enable undo tracking.
    (let ((buffer-undo-list nil))
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
    "vim-tests/paste-before-insert-mode-visual-block-region-%s-1"
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
    "vim-tests/paste-after-visual-block-region-%s-1"
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
    "vim-tests/paste-after-visual-block-region-%s-2"
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
    "vim-tests/paste-after-visual-block-region-%s-3"
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
    "vim-tests/paste-after-visual-block-region-%s-4"
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
    "vim-tests/paste-after-visual-block-region-undo-%s-1"
    ;; Enable undo tracking.
    (let ((buffer-undo-list nil))
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
    "vim-tests/paste-after-visual-block-region-undo-%s-2"
    ;; Enable undo tracking.
    (let ((buffer-undo-list nil))
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
    "vim-tests/copy-line-and-paste-after-%s-1"
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
    "vim-tests/copy-line-and-paste-before-%s-1"
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
    "vim-tests/navigation-insertion-%s-1"
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
    "vim-tests/paste-cycle-after-%s-1"
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
    "vim-tests/paste-cycle-after-%s-2"
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
    "vim-tests/tab-on-newly-created-empty-line-%s-1"
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
    "vim-tests/paste-cycle-before-%s-1"
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
    "vim-tests/block-select-delete-%s-1"
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
    "vim-tests/block-select-delete-then-paste-%s-1"
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
    "vim-tests/interleave-search-and-repeat-last-find-%s-1"
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
    "vim-tests/interleave-search-and-repeat-last-find-%s-2"
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
    "vim-tests/visual-reactivate-%s-1"
    ;; Enable undo tracking.
    (let ((buffer-undo-list nil))
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
    "vim-tests/insert-linewise-region-%s-1"
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

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    "vim-tests/insert-linewise-region-newline-%s-1"
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
    "vim-tests/motion-inner-single-quote-%s-1"
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
    "vim-tests/motion-outer-single-quote-%s-1"
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
    "vim-tests/motion-inner-double-quote-%s-1"
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
    "vim-tests/motion-outer-double-quote-%s-1"
    (execute-kbd-macro (kbd ", a \""))
  (tests-utils--multiline
   ""
   "foo \"b_|_ar\" baz"
   "")
  (tests-utils--multiline
   ""
   "foo _|_baz"
   ""))

(vim-tests--test-fresh-buffer-contents-init-standard-modes
    "vim-tests/repeated-search-%s-1"
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
    "vim-tests/repeated-search-%s-2"
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
    "vim-tests/repeated-search-%s-3"
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
    "vim-tests/repeated-search-%s-4"
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
    "vim-tests/repeated-search-%s-5"
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
    "vim-tests/delete-to-beginning-of-buffer-%s-1"
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
    "vim-tests/record-and-execute-macro-%s-1"
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

(provide 'vim-tests)

;; Local Variables:
;; End:

;; vim-tests.el ends here
