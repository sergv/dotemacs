;; isabelle-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 31 January 2025
;; Description:

(defvar lsp-isar-use-lsp)

(require 'isabelle-setup)
(require 'lsp-isar-output)

(require 'ert)
(require 'tests-utils)

(ert-deftest isabelle-tests/fix-indents-1 ()
  (tests-utils--test-buffer-contents
   :action
   (lsp-isar-output--fix-indents!)
   :contents
   (tests-utils--multiline
    "_|_have 0 \\<le> (\\<Sum>j = 1..n. (x\\<^bsub>j\\<^esub>)\\<^sup>2)"
    "proof (state)"
    "            this:"
    "              0 \\<le> (\\<Sum>j = 1..n. (x\\<^bsub>j\\<^esub>)\\<^sup>2)"
    ""
    "            goal (1 subgoal):"
    "             1. 0 \\<le> (\\<parallel>x\\<parallel> * \\<parallel>y\\<parallel>)\\<^sup>2 - \\<bar>x \\<cdot> y\\<bar>\\<^sup>2")
   :expected-value
   (tests-utils--multiline
    "_|_have 0 \\<le> (\\<Sum>j = 1..n. (x\\<^bsub>j\\<^esub>)\\<^sup>2)"
    "proof (state)"
    "this:"
    "  0 \\<le> (\\<Sum>j = 1..n. (x\\<^bsub>j\\<^esub>)\\<^sup>2)"
    ""
    "goal (1 subgoal):"
    " 1. 0 \\<le> (\\<parallel>x\\<parallel> * \\<parallel>y\\<parallel>)\\<^sup>2 - \\<bar>x \\<cdot> y\\<bar>\\<^sup>2")
   :initialisation nil
   :buffer-id isabelle-tests))

(ert-deftest isabelle-tests/fix-indents-2a ()
  (tests-utils--test-buffer-contents
   :action
   (lsp-isar-output--fix-indents!)
   :contents
   (tests-utils--multiline
    "_|_have foo"
    "proof (state)"
    "            this:"
    "              0 foo"
    ""
    "            goal (1 subgoal):"
    "             1. foo")
   :expected-value
   (tests-utils--multiline
    "_|_have foo"
    "proof (state)"
    "this:"
    "  0 foo"
    ""
    "goal (1 subgoal):"
    " 1. foo")
   :initialisation nil
   :buffer-id isabelle-tests))

(ert-deftest isabelle-tests/fix-indents-2b ()
  (tests-utils--test-buffer-contents
   :action
   (lsp-isar-output--fix-indents!)
   :contents
   (tests-utils--multiline
    "_|_have foo"
    "proof (state)"
    "            this:"
    "              0 foo"
    "              "
    "            goal (1 subgoal):"
    "             1. foo")
   :expected-value
   (tests-utils--multiline
    "_|_have foo"
    "proof (state)"
    "this:"
    "  0 foo"
    "  "
    "goal (1 subgoal):"
    " 1. foo")
   :initialisation nil
   :buffer-id isabelle-tests))

(ert-deftest isabelle-tests/fix-indents-3 ()
  (tests-utils--test-buffer-contents
   :action
   (lsp-isar-output--fix-indents!)
   :contents
   (tests-utils--multiline
    "_|_have foo"
    "proof (state)"
    "            this:"
    "      0 foo"
    ""
    "            goal (1 subgoal):"
    "             1. foo")
   :expected-value
   (tests-utils--multiline
    "_|_have foo"
    "proof (state)"
    "this:"
    "0 foo"
    ""
    "goal (1 subgoal):"
    " 1. foo")
   :initialisation nil
   :buffer-id isabelle-tests))

(ert-deftest isabelle-tests/uncomment-nested-region-1 ()
  (tests-utils--test-buffer-contents
   :action
   (should-error (comment-util-uncomment-region))
   :contents
   (tests-utils--multiline
    ""
    "thm _|_natural_numbers.simps[of \"int_to_nat \\\<lfloor>M + 1\\\\<rfloor>\"]"
    "")
   :expected-value
   (tests-utils--multiline
    ""
    "thm _|_natural_numbers.simps[of \"int_to_nat \\\<lfloor>M + 1\\\\<rfloor>\"]"
    "")
   :initialisation (let ((lsp-isar-use-lsp nil)) (isar-mode) (isar-setup))
   :buffer-id isabelle-comment-tests))

(ert-deftest isabelle-tests/uncomment-nested-region-2a ()
  (tests-utils--test-buffer-contents
   :action
   (comment-util-uncomment-region)
   :contents
   (tests-utils--multiline
    ""
    "(* thm _|_natural_numbers.simps[of \"int_to_nat \\\<lfloor>M + 1\\\\<rfloor>\"] *)"
    "")
   :expected-value
   (tests-utils--multiline
    ""
    "thm _|_natural_numbers.simps[of \"int_to_nat \\\<lfloor>M + 1\\\\<rfloor>\"]"
    "")
   :initialisation (let ((lsp-isar-use-lsp nil)) (isar-mode) (isar-setup))
   :buffer-id isabelle-comment-tests))

(ert-deftest isabelle-tests/uncomment-nested-region-2b ()
  (tests-utils--test-buffer-contents
   :action
   (comment-util-uncomment-region)
   :contents
   (tests-utils--multiline
    ""
    "(*thm _|_natural_numbers.simps[of \"int_to_nat \\\<lfloor>M + 1\\\\<rfloor>\"]*)"
    "")
   :expected-value
   (tests-utils--multiline
    ""
    "thm _|_natural_numbers.simps[of \"int_to_nat \\\<lfloor>M + 1\\\\<rfloor>\"]"
    "")
   :initialisation (let ((lsp-isar-use-lsp nil)) (isar-mode) (isar-setup))
   :buffer-id isabelle-comment-tests))

(ert-deftest isabelle-tests/uncomment-nested-region-3 ()
  (tests-utils--test-buffer-contents
   :action
   (comment-util-uncomment-region)
   :contents
   (tests-utils--multiline
    "(*"
    "(* thm _|_natural_numbers.simps[of \"int_to_nat \\\<lfloor>M + 1\\\\<rfloor>\"] *)"
    "(* value \"1 :: nat\" *)"
    "*)")
   :expected-value
   (tests-utils--multiline
    ""
    "(* thm _|_natural_numbers.simps[of \"int_to_nat \\\<lfloor>M + 1\\\\<rfloor>\"] *)"
    "(* value \"1 :: nat\" *)"
    "")
   :initialisation (let ((lsp-isar-use-lsp nil)) (isar-mode) (isar-setup))
   :buffer-id isabelle-comment-tests))

(provide 'isabelle-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; isabelle-tests.el ends here
