;; isabelle-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 31 January 2025
;; Description:

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

(ert-deftest isabelle-tests/lsp-isar-output-parse-output-1 ()
  (let ((input '(html nil
                      (body nil
                            (pre ((class . "source"))
                                 (state_message
                                  ((serial . "5304220") (offset . "1")
                                   (end_offset . "3") (label . "command.by")
                                   (id . "23360"))
                                  (position
                                   ((offset . "1") (end_offset . "5")
                                    (label . "command.have") (id . "23340"))
                                   (keyword1 nil "have"))
                                  (break nil "' '")
                                  (language
                                   ((name . "term") (symbols . "true")
                                    (antiquotes . "false") (delimited . "false"))
                                   (entity
                                    ((ref . "4540414") (def_line . "958")
                                     (def_offset . "30596") (def_end_offset . "30600")
                                     (def_file . "~~/src/HOL/Real.thy")
                                     (def_id . "214") (name . "Real.real")
                                     (kind . "constant"))
                                    "real")
                                   (break nil "' '")
                                   (xml_elem ((xml_name . "typing"))
                                             (xml_body nil
                                                       (block nil
                                                         (entity
                                                          ((ref . "642430")
                                                           (def_line . "32")
                                                           (def_offset . "621")
                                                           (def_end_offset . "624")
                                                           (def_file
                                                            . "~~/src/HOL/Nat.thy")
                                                           (def_id . "44")
                                                           (name . "Nat.nat")
                                                           (kind . "type_name"))
                                                          (block nil "nat"))))
                                             (fixed ((name . "M__")) (skolem nil "M")))
                                   (break ((line . "1")) "'  '")
                                   (entity
                                    ((ref . "98074") (def_line . "175")
                                     (def_offset . "4523") (def_end_offset . "4528")
                                     (def_label . "command.class")
                                     (def_file . "~~/src/HOL/Orderings.thy")
                                     (def_id . "16")
                                     (name . "Orderings.ord_class.less")
                                     (kind . "constant"))
                                    (delimiter nil "<"))
                                   (break nil "' '")
                                   (entity
                                    ((ref . "5281300") (def_offset . "5")
                                     (def_end_offset . "20") (def_id . "-861")
                                     (name . "Limits.natural_numbers")
                                     (kind . "constant"))
                                    "natural_numbers")
                                   (sub nil
                                        (entity
                                         ((ref . "5292528") (def_offset . "5")
                                          (def_end_offset . "16") (def_id . "-1260")
                                          (name . "Limits.real_to_nat")
                                          (kind . "constant"))
                                         "real_to_nat")
                                        (break ((line . "1")) "'            '") "("
                                        (entity
                                         ((ref . "4540464") (def_line . "961")
                                          (def_offset . "30655")
                                          (def_end_offset . "30666")
                                          (def_file . "~~/src/HOL/Real.thy")
                                          (def_id . "214") (name . "Real.real_of_int")
                                          (kind . "constant"))
                                         "real_of_int")
                                        (break nil "' '")
                                        (entity
                                         ((ref . "4367158") (def_line . "207")
                                          (def_offset . "5831")
                                          (def_end_offset . "5836")
                                          (def_label . "command.class")
                                          (def_file
                                           . "~~/src/HOL/Archimedean_Field.thy")
                                          (def_id . "202")
                                          (name
                                           . "Archimedean_Field.floor_ceiling_class.floor")
                                          (kind . "constant"))
                                         (delimiter nil
                                                    (emacs_isabelle_symbol nil
                                                                           "lfloor")))
                                        (entity
                                         ((ref . "4540414") (def_line . "958")
                                          (def_offset . "30596")
                                          (def_end_offset . "30600")
                                          (def_file . "~~/src/HOL/Real.thy")
                                          (def_id . "214") (name . "Real.real")
                                          (kind . "constant"))
                                         "real")
                                        " ("
                                        (xml_elem ((xml_name . "typing"))
                                                  (xml_body nil
                                                            (block nil
                                                              (entity
                                                               ((ref . "642430")
                                                                (def_line . "32")
                                                                (def_offset . "621")
                                                                (def_end_offset
                                                                 . "624")
                                                                (def_file
                                                                 . "~~/src/HOL/Nat.thy")
                                                                (def_id . "44")
                                                                (name . "Nat.nat")
                                                                (kind . "type_name"))
                                                               (block nil "nat"))))
                                                  (fixed ((name . "M__"))
                                                         (skolem nil "M")))
                                        (break nil "' '")
                                        (entity
                                         ((ref . "145492") (def_line . "194")
                                          (def_offset . "5349")
                                          (def_end_offset . "5354")
                                          (def_label . "command.class")
                                          (def_file . "~~/src/HOL/Groups.thy")
                                          (def_id . "18")
                                          (name . "Groups.plus_class.plus")
                                          (kind . "constant"))
                                         (delimiter nil "+"))
                                        (break nil "' '")
                                        (xml_elem ((xml_name . "typing"))
                                                  (xml_body nil
                                                            (block nil
                                                              (entity
                                                               ((ref . "642430")
                                                                (def_line . "32")
                                                                (def_offset . "621")
                                                                (def_end_offset
                                                                 . "624")
                                                                (def_file
                                                                 . "~~/src/HOL/Nat.thy")
                                                                (def_id . "44")
                                                                (name . "Nat.nat")
                                                                (kind . "type_name"))
                                                               (block nil "nat"))))
                                                  (entity
                                                   ((ref . "145072")
                                                    (def_line . "163")
                                                    (def_offset . "4544")
                                                    (def_end_offset . "4549")
                                                    (def_label . "command.class")
                                                    (def_file
                                                     . "~~/src/HOL/Groups.thy")
                                                    (def_id . "18")
                                                    (name . "Groups.one_class.one")
                                                    (kind . "constant"))
                                                   (delimiter nil "1")))
                                        ")"
                                        (entity
                                         ((ref . "4367158") (def_line . "207")
                                          (def_offset . "5831")
                                          (def_end_offset . "5836")
                                          (def_label . "command.class")
                                          (def_file
                                           . "~~/src/HOL/Archimedean_Field.thy")
                                          (def_id . "202")
                                          (name
                                           . "Archimedean_Field.floor_ceiling_class.floor")
                                          (kind . "constant"))
                                         (delimiter nil
                                                    (emacs_isabelle_symbol nil
                                                                           "rfloor")))
                                        ")")))
                                 (state_message
                                  ((serial . "5304230") (offset . "1")
                                   (end_offset . "3") (label . "command.by")
                                   (id . "23364"))
                                  (text_fold nil
                                             (position
                                              ((offset . "1") (end_offset . "3")
                                               (label . "command.by") (id . "23364"))
                                              "proof")
                                             " (state)")
                                  (break ((line . "1")) "'                       '")
                                  (text_fold nil "this:"
                                             (break ((line . "1"))
                                                    "'                      '")
                                             (language
                                              ((name . "term") (symbols . "true")
                                               (antiquotes . "false")
                                               (delimited . "false"))
                                              (entity
                                               ((ref . "4540414") (def_line . "958")
                                                (def_offset . "30596")
                                                (def_end_offset . "30600")
                                                (def_file . "~~/src/HOL/Real.thy")
                                                (def_id . "214") (name . "Real.real")
                                                (kind . "constant"))
                                               "real")
                                              (break nil "' '")
                                              (xml_elem ((xml_name . "typing"))
                                                        (xml_body nil
                                                                  (block nil
                                                                    (entity
                                                                     ((ref . "642430")
                                                                      (def_line . "32")
                                                                      (def_offset
                                                                       . "621")
                                                                      (def_end_offset
                                                                       . "624")
                                                                      (def_file
                                                                       . "~~/src/HOL/Nat.thy")
                                                                      (def_id . "44")
                                                                      (name
                                                                       . "Nat.nat")
                                                                      (kind
                                                                       . "type_name"))
                                                                     (block nil "nat"))))
                                                        (fixed ((name . "M__"))
                                                               (skolem nil "M")))
                                              (break nil "' '")
                                              (entity
                                               ((ref . "98074") (def_line . "175")
                                                (def_offset . "4523")
                                                (def_end_offset . "4528")
                                                (def_label . "command.class")
                                                (def_file . "~~/src/HOL/Orderings.thy")
                                                (def_id . "16")
                                                (name . "Orderings.ord_class.less")
                                                (kind . "constant"))
                                               (delimiter nil "<"))
                                              (break nil "' '")
                                              (entity
                                               ((ref . "5281300") (def_offset . "5")
                                                (def_end_offset . "20")
                                                (def_id . "-861")
                                                (name . "Limits.natural_numbers")
                                                (kind . "constant"))
                                               "natural_numbers")
                                              (sub nil
                                                   (entity
                                                    ((ref . "5292528")
                                                     (def_offset . "5")
                                                     (def_end_offset . "16")
                                                     (def_id . "-1260")
                                                     (name . "Limits.real_to_nat")
                                                     (kind . "constant"))
                                                    "real_to_nat")
                                                   " ("
                                                   (entity
                                                    ((ref . "4540464")
                                                     (def_line . "961")
                                                     (def_offset . "30655")
                                                     (def_end_offset . "30666")
                                                     (def_file . "~~/src/HOL/Real.thy")
                                                     (def_id . "214")
                                                     (name . "Real.real_of_int")
                                                     (kind . "constant"))
                                                    "real_of_int")
                                                   (break nil "' '")
                                                   (entity
                                                    ((ref . "4367158")
                                                     (def_line . "207")
                                                     (def_offset . "5831")
                                                     (def_end_offset . "5836")
                                                     (def_label . "command.class")
                                                     (def_file
                                                      . "~~/src/HOL/Archimedean_Field.thy")
                                                     (def_id . "202")
                                                     (name
                                                      . "Archimedean_Field.floor_ceiling_class.floor")
                                                     (kind . "constant"))
                                                    (delimiter nil
                                                               (emacs_isabelle_symbol
                                                                nil "lfloor")))
                                                   (entity
                                                    ((ref . "4540414")
                                                     (def_line . "958")
                                                     (def_offset . "30596")
                                                     (def_end_offset . "30600")
                                                     (def_file . "~~/src/HOL/Real.thy")
                                                     (def_id . "214")
                                                     (name . "Real.real")
                                                     (kind . "constant"))
                                                    "real")
                                                   " ("
                                                   (xml_elem ((xml_name . "typing"))
                                                             (xml_body nil
                                                                       (block nil
                                                                         (entity
                                                                          ((ref
                                                                            . "642430")
                                                                           (def_line
                                                                            . "32")
                                                                           (def_offset
                                                                            . "621")
                                                                           (def_end_offset
                                                                            . "624")
                                                                           (def_file
                                                                            . "~~/src/HOL/Nat.thy")
                                                                           (def_id
                                                                            . "44")
                                                                           (name
                                                                            . "Nat.nat")
                                                                           (kind
                                                                            . "type_name"))
                                                                          (block nil
                                                                            "nat"))))
                                                             (fixed ((name . "M__"))
                                                                    (skolem nil "M")))
                                                   (break nil "' '")
                                                   (entity
                                                    ((ref . "145492")
                                                     (def_line . "194")
                                                     (def_offset . "5349")
                                                     (def_end_offset . "5354")
                                                     (def_label . "command.class")
                                                     (def_file
                                                      . "~~/src/HOL/Groups.thy")
                                                     (def_id . "18")
                                                     (name . "Groups.plus_class.plus")
                                                     (kind . "constant"))
                                                    (delimiter nil "+"))
                                                   (break nil "' '")
                                                   (xml_elem ((xml_name . "typing"))
                                                             (xml_body nil
                                                                       (block nil
                                                                         (entity
                                                                          ((ref
                                                                            . "642430")
                                                                           (def_line
                                                                            . "32")
                                                                           (def_offset
                                                                            . "621")
                                                                           (def_end_offset
                                                                            . "624")
                                                                           (def_file
                                                                            . "~~/src/HOL/Nat.thy")
                                                                           (def_id
                                                                            . "44")
                                                                           (name
                                                                            . "Nat.nat")
                                                                           (kind
                                                                            . "type_name"))
                                                                          (block nil
                                                                            "nat"))))
                                                             (entity
                                                              ((ref . "145072")
                                                               (def_line . "163")
                                                               (def_offset . "4544")
                                                               (def_end_offset
                                                                . "4549")
                                                               (def_label
                                                                . "command.class")
                                                               (def_file
                                                                . "~~/src/HOL/Groups.thy")
                                                               (def_id . "18")
                                                               (name
                                                                . "Groups.one_class.one")
                                                               (kind . "constant"))
                                                              (delimiter nil "1")))
                                                   ")"
                                                   (entity
                                                    ((ref . "4367158")
                                                     (def_line . "207")
                                                     (def_offset . "5831")
                                                     (def_end_offset . "5836")
                                                     (def_label . "command.class")
                                                     (def_file
                                                      . "~~/src/HOL/Archimedean_Field.thy")
                                                     (def_id . "202")
                                                     (name
                                                      . "Archimedean_Field.floor_ceiling_class.floor")
                                                     (kind . "constant"))
                                                    (delimiter nil
                                                               (emacs_isabelle_symbol
                                                                nil "rfloor")))
                                                   ")")))
                                  (break ((line . "1")) "'                       '")
                                  (text_fold nil)
                                  (break ((line . "1")) "'                       '")
                                  (text_fold nil (keyword1 nil "goal") " (1 subgoal):")
                                  (break ((line . "1")) "'                       '")
                                  (text_fold nil
                                             (subgoal ((name . "1")) " 1. "
                                                      (language
                                                       ((name . "term")
                                                        (symbols . "true")
                                                        (antiquotes . "false")
                                                        (delimited . "false"))
                                                       (entity
                                                        ((ref . "276")
                                                         (def_line . "203")
                                                         (def_file . "pure_thy.ML")
                                                         (name . "Pure.all")
                                                         (kind . "constant"))
                                                        (delimiter nil
                                                                   (emacs_isabelle_symbol
                                                                    nil "And")))
                                                       (xml_elem
                                                        ((xml_name . "typing"))
                                                        (xml_body nil
                                                                  (block nil
                                                                    (entity
                                                                     ((ref . "4521938")
                                                                      (def_line
                                                                       . "384")
                                                                      (def_offset
                                                                       . "12507")
                                                                      (def_end_offset
                                                                       . "12511")
                                                                      (def_file
                                                                       . "~~/src/HOL/Real.thy")
                                                                      (def_id . "214")
                                                                      (name
                                                                       . "Real.real")
                                                                      (kind
                                                                       . "type_name"))
                                                                     (block nil "real"))))
                                                        (bound nil "M"))
                                                       (entity
                                                        ((ref . "276")
                                                         (def_line . "203")
                                                         (def_file . "pure_thy.ML")
                                                         (name . "Pure.all")
                                                         (kind . "constant"))
                                                        (delimiter nil "."))
                                                       (break nil "' '")
                                                       (xml_elem
                                                        ((xml_name . "typing"))
                                                        (xml_body nil
                                                                  (block nil
                                                                    (entity
                                                                     ((ref . "4521938")
                                                                      (def_line
                                                                       . "384")
                                                                      (def_offset
                                                                       . "12507")
                                                                      (def_end_offset
                                                                       . "12511")
                                                                      (def_file
                                                                       . "~~/src/HOL/Real.thy")
                                                                      (def_id . "214")
                                                                      (name
                                                                       . "Real.real")
                                                                      (kind
                                                                       . "type_name"))
                                                                     (block nil "real"))))
                                                        (entity
                                                         ((ref . "144902")
                                                          (def_line . "160")
                                                          (def_offset . "4504")
                                                          (def_end_offset . "4509")
                                                          (def_label . "command.class")
                                                          (def_file
                                                           . "~~/src/HOL/Groups.thy")
                                                          (def_id . "18")
                                                          (name
                                                           . "Groups.zero_class.zero")
                                                          (kind . "constant"))
                                                         (delimiter nil "0")))
                                                       (break nil "' '")
                                                       (entity
                                                        ((ref . "98074")
                                                         (def_line . "175")
                                                         (def_offset . "4523")
                                                         (def_end_offset . "4528")
                                                         (def_label . "command.class")
                                                         (def_file
                                                          . "~~/src/HOL/Orderings.thy")
                                                         (def_id . "16")
                                                         (name
                                                          . "Orderings.ord_class.less")
                                                         (kind . "constant"))
                                                        (delimiter nil "<"))
                                                       (break nil "' '")
                                                       (xml_elem
                                                        ((xml_name . "typing"))
                                                        (xml_body nil
                                                                  (block nil
                                                                    (entity
                                                                     ((ref . "4521938")
                                                                      (def_line
                                                                       . "384")
                                                                      (def_offset
                                                                       . "12507")
                                                                      (def_end_offset
                                                                       . "12511")
                                                                      (def_file
                                                                       . "~~/src/HOL/Real.thy")
                                                                      (def_id . "214")
                                                                      (name
                                                                       . "Real.real")
                                                                      (kind
                                                                       . "type_name"))
                                                                     (block nil "real"))))
                                                        (bound nil "M"))
                                                       (break nil "' '")
                                                       (entity
                                                        ((ref . "274")
                                                         (def_line . "202")
                                                         (def_file . "pure_thy.ML")
                                                         (name . "Pure.imp")
                                                         (kind . "constant"))
                                                        (delimiter nil
                                                                   (emacs_isabelle_symbol
                                                                    nil
                                                                    "Longrightarrow")))
                                                       (break nil "' '")
                                                       (entity
                                                        ((ref . "68690")
                                                         (def_line . "106")
                                                         (def_offset . "3390")
                                                         (def_end_offset . "3392")
                                                         (def_file
                                                          . "~~/src/HOL/HOL.thy")
                                                         (def_id . "10")
                                                         (name . "HOL.Ex")
                                                         (kind . "constant"))
                                                        (delimiter nil
                                                                   (emacs_isabelle_symbol
                                                                    nil "exists")))
                                                       (xml_elem
                                                        ((xml_name . "typing"))
                                                        (xml_body nil
                                                                  (block nil
                                                                    (entity
                                                                     ((ref . "642430")
                                                                      (def_line . "32")
                                                                      (def_offset
                                                                       . "621")
                                                                      (def_end_offset
                                                                       . "624")
                                                                      (def_file
                                                                       . "~~/src/HOL/Nat.thy")
                                                                      (def_id . "44")
                                                                      (name
                                                                       . "Nat.nat")
                                                                      (kind
                                                                       . "type_name"))
                                                                     (block nil "nat"))))
                                                        (bound nil "k"))
                                                       (entity
                                                        ((ref . "68690")
                                                         (def_line . "106")
                                                         (def_offset . "3390")
                                                         (def_end_offset . "3392")
                                                         (def_file
                                                          . "~~/src/HOL/HOL.thy")
                                                         (def_id . "10")
                                                         (name . "HOL.Ex")
                                                         (kind . "constant"))
                                                        (delimiter nil "."))
                                                       (break nil "' '")
                                                       (xml_elem
                                                        ((xml_name . "typing"))
                                                        (xml_body nil
                                                                  (block nil
                                                                    (entity
                                                                     ((ref . "4521938")
                                                                      (def_line
                                                                       . "384")
                                                                      (def_offset
                                                                       . "12507")
                                                                      (def_end_offset
                                                                       . "12511")
                                                                      (def_file
                                                                       . "~~/src/HOL/Real.thy")
                                                                      (def_id . "214")
                                                                      (name
                                                                       . "Real.real")
                                                                      (kind
                                                                       . "type_name"))
                                                                     (block nil "real"))))
                                                        (bound nil "M"))
                                                       (break nil "' '")
                                                       (entity
                                                        ((ref . "98074")
                                                         (def_line . "175")
                                                         (def_offset . "4523")
                                                         (def_end_offset . "4528")
                                                         (def_label . "command.class")
                                                         (def_file
                                                          . "~~/src/HOL/Orderings.thy")
                                                         (def_id . "16")
                                                         (name
                                                          . "Orderings.ord_class.less")
                                                         (kind . "constant"))
                                                        (delimiter nil "<"))
                                                       (break nil "' '")
                                                       (entity
                                                        ((ref . "188492")
                                                         (def_line . "1166")
                                                         (def_offset . "31594")
                                                         (def_end_offset . "31599")
                                                         (def_label . "command.class")
                                                         (def_file
                                                          . "~~/src/HOL/Groups.thy")
                                                         (def_id . "18")
                                                         (name
                                                          . "Groups.abs_class.abs")
                                                         (kind . "constant"))
                                                        (delimiter nil
                                                                   (emacs_isabelle_symbol
                                                                    nil "bar")))
                                                       (entity
                                                        ((ref . "5281300")
                                                         (def_offset . "5")
                                                         (def_end_offset . "20")
                                                         (def_id . "-861")
                                                         (name
                                                          . "Limits.natural_numbers")
                                                         (kind . "constant"))
                                                        "natural_numbers")
                                                       (sub nil
                                                            (xml_elem
                                                             ((xml_name . "typing"))
                                                             (xml_body nil
                                                                       (block nil
                                                                         (entity
                                                                          ((ref
                                                                            . "642430")
                                                                           (def_line
                                                                            . "32")
                                                                           (def_offset
                                                                            . "621")
                                                                           (def_end_offset
                                                                            . "624")
                                                                           (def_file
                                                                            . "~~/src/HOL/Nat.thy")
                                                                           (def_id
                                                                            . "44")
                                                                           (name
                                                                            . "Nat.nat")
                                                                           (kind
                                                                            . "type_name"))
                                                                          (block nil
                                                                            "nat"))))
                                                             (bound nil "k")))
                                                       (entity
                                                        ((ref . "188492")
                                                         (def_line . "1166")
                                                         (def_offset . "31594")
                                                         (def_end_offset . "31599")
                                                         (def_label . "command.class")
                                                         (def_file
                                                          . "~~/src/HOL/Groups.thy")
                                                         (def_id . "18")
                                                         (name
                                                          . "Groups.abs_class.abs")
                                                         (kind . "constant"))
                                                        (delimiter nil
                                                                   (emacs_isabelle_symbol
                                                                    nil "bar"))))))))))))
    (tests-utils--test-buffer-contents
     :action
     (should (lsp-isar-output-parse-output input))
     :contents
     (tests-utils--multiline
      "_|_")
     :expected-value
     (tests-utils--multiline
      "have real M"
      "  < natural_numbers\\<^bsub>real_to_nat"
      "            (real_of_int \\<lfloor>real (M + 1)\\<rfloor>)\\<^esub>"
      "proof (state)"
      "                       this:"
      "                      real M < natural_numbers\\<^bsub>real_to_nat (real_of_int \\<lfloor>real (M + 1)\\<rfloor>)\\<^esub>"
      "                       "
      "                       goal (1 subgoal):"
      "                        1. \\<And>M. 0 < M \\<Longrightarrow> \\<exists>k. M < \\<bar>natural_numbers\\<^bsub>k\\<^esub>\\<bar>"
      "_|_")
     :initialisation nil
     :buffer-id nil)))

(provide 'isabelle-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; isabelle-tests.el ends here
