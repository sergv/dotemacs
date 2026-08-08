;; treesit-sexp-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created:  8 August 2026
;; Description:

(require 'treesit-sexp)

(require 'ert)
(require 'tests-utils)

(cl-defmacro treesit-sexp-tests--test-fresh-buffer-contents-init-all
    (&key name modes action contents expected-value fresh-buffer)
  (declare (indent nil))
  `(tests-utils--test-buffer-contents-for-inits
    :name ,name
    :inits ,(--filter (memq (car it) modes) vim-tests--all-known-modes-and-init)
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    ;; Don’t reuse buffer to start out in fresh environment each time and don’t
    ;; share things like last cmd events, etc.
    :buffer-id ,(if fresh-buffer nil (lambda (mode) (string->symbol (format "treesit-sexp-tests-%s" mode))))))

(cl-defmacro treesit-sexp-tests--default-test-buffer-contents*
    (&key modes
          name
          action
          contents
          expected-value
          fresh-buffer)
  (declare (indent nil))
  (cl-assert (listp modes))
  (cl-assert (cl-every #'symbolp modes))
  `(treesit-sexp-tests--test-fresh-buffer-contents-init-all
    :name
    ,name
    :modes
    ,(if modes
         (--filter (memq it modes) (-map #'car vim-tests--all-known-modes-and-init))
       (-map #'car vim-tests--all-known-modes-and-init))
    :action
    ,action
    :contents
    ,contents
    :expected-value
    ,expected-value
    :fresh-buffer ,fresh-buffer))

(treesit-sexp-tests--default-test-buffer-contents*
 :modes '(haskell-mode nix-mode rust-mode)
 :name treesit-sexp-tests/treesit-sexp-forward-1
 :action (treesit-sexp-forward)
 :contents
 "(foo _|_bar baz)"
 :expected-value
 "(foo bar_|_ baz)")

(treesit-sexp-tests--default-test-buffer-contents*
 :modes '(haskell-mode nix-mode rust-mode)
 :name treesit-sexp-tests/treesit-sexp-forward-2
 :action (treesit-sexp-forward)
 :contents
 "(foo _|_(bar quux) baz)"
 :expected-value
 "(foo (bar quux)_|_ baz)")

(treesit-sexp-tests--default-test-buffer-contents*
 :modes '(haskell-mode nix-mode rust-mode)
 :name treesit-sexp-tests/treesit-sexp-forward-3
 :action (treesit-sexp-forward)
 :contents
 "(_|_foo (bar quux) baz)"
 :expected-value
 "(foo_|_ (bar quux) baz)")

(treesit-sexp-tests--default-test-buffer-contents*
 :modes '(nix-mode)
 :name treesit-sexp-tests/treesit-sexp-forward-4
 :action (treesit-sexp-forward)
 :contents
 "(xx_|_ \"foo${quux}bar\" baz)"
 :expected-value
 "(xx \"foo${quux}bar\"_|_ baz)")

(treesit-sexp-tests--default-test-buffer-contents*
 :modes '(haskell-mode nix-mode rust-mode)
 :name treesit-sexp-tests/treesit-sexp-forward-list-1
 :action (should-fail (treesit-sexp-forward-list))
 :contents
 "(foo _|_bar baz)"
 :expected-value
 "(foo bar_|_ baz)")

(treesit-sexp-tests--default-test-buffer-contents*
 :modes '(haskell-mode nix-mode rust-mode)
 :name treesit-sexp-tests/treesit-sexp-forward-list-2
 :action (should-fail (treesit-sexp-forward-list))
 :contents
 "(foo _|_(bar quux) baz)"
 :expected-value
 "(foo (bar quux)_|_ baz)")

(treesit-sexp-tests--default-test-buffer-contents*
 :modes '(haskell-mode nix-mode rust-mode)
 :name treesit-sexp-tests/treesit-sexp-forward-list-3
 :action (should-fail (treesit-sexp-forward-list))
 :contents
 "(_|_foo (bar quux) baz)"
 :expected-value
 "(foo (bar quux)_|_ baz)")

(provide 'treesit-sexp-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; treesit-sexp-tests.el ends here
