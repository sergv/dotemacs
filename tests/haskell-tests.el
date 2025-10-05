;; haskell-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 26 November 2013
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'cl))

(require 'alex-mode)
(require 'dante)
(require 'happy-mode)
(require 'haskell-abbrev+)
(require 'haskell-block-indent)
(require 'haskell-cabal-components)
(require 'haskell-format-setup)
(require 'haskell-misc)
(require 'haskell-regexen)
(require 'haskell-smart-operators-mode)
(require 'haskell-sort-imports)

(require 'common)
(require 'ert)
(require 'search)
(require 'tests-utils)

(ert-deftest haskell-tests/abbrev+-extract-module-name ()
  (should (string= (haskell-abbrev+-extract-mod-name "Foo.Bar")
                   "Bar"))
  (should (string= (haskell-abbrev+-extract-mod-name "Foo.Bar.Baz")
                   "Baz"))
  (should (string= (haskell-abbrev+-extract-mod-name "Foo'.Bar2.Baz_3.Quux")
                   "Quux")))

(cl-defmacro haskell-tests--test-buffer-contents*
    (&key name
          action
          contents
          expected-value
          (modes '(haskell-mode haskell-ts-mode))
          fresh-buffer)
  `(progn
     ,@(cl-loop
        for mode in modes
        collect
        `(ert-deftest ,(string->symbol (format "%s/%s" name mode)) ()
           (tests-utils--test-buffer-contents
            :action ,action
            :contents ,contents
            :expected-value ,expected-value
            :initialisation (,mode)
            :buffer-id ,(if fresh-buffer nil (string->symbol (format "haskell-tests-%s" mode))))))))

(defmacro haskell-tests--test-buffer-contents (name action contents expected-value)
  (declare (indent 2))
  `(haskell-tests--test-buffer-contents*
    :name ,name
    :action ,action
    :contents ,contents
    :expected-value ,expected-value))

(defmacro haskell-tests--test-buffer-contents-expect-failed (name action contents expected-value)
  (declare (indent 2))
  `(progn
     ,@(cl-loop
        for mode in '(haskell-mode haskell-ts-mode)
        collect
        `(ert-deftest ,(string->symbol (format "%s/%s" name mode)) ()
           :expected-result :failed
           (tests-utils--test-buffer-contents
            :action ,action
            :contents ,contents
            :expected-value ,expected-value
            :initialisation (,mode)
            :buffer-id ,(string->symbol (format "haskell-tests-%s" mode)))))))

(defmacro haskell-tests--cabal--test-buffer-contents (action contents expected-value)
  (declare (indent 1))
  `(tests-utils--test-buffer-contents
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    :initialisation (haskell-cabal-mode)
    :buffer-id haskell-cabal))

(defmacro haskell-tests--make-multiple-output-test-buffer-contents (initial entries)
  "Define a set of tests that share initial buffer state but
execute diffent actions and reach different buffer states in the
end."
  (declare (indent 1))
  `(progn
     ,@(cl-loop
        for entry in entries
        collect
        `(haskell-tests--test-buffer-contents
             ,(cl-first entry)
             ,(cl-second entry)
           ,initial
           ,(cl-third entry)))))

(cl-defmacro haskell-tests--make-multiple-input-test-buffer-contents*
    (&key action entries expected-value modes fresh-buffer)
  "Define a set of tests that share final buffer state but
have different input states."
  (declare (indent 0))
  `(progn
     ,@(cl-loop
        for entry in entries
        collect
        `(haskell-tests--test-buffer-contents*
          :name ,(cl-first entry)
          :action ,action
          :contents ,(cl-second entry)
          :expected-value ,expected-value
          ,@(when modes `(:modes ,modes))
          :fresh-buffer ,fresh-buffer))))

(cl-defmacro haskell-tests--make-multiple-test-result-tests (name &key entries contents modes)
  (declare (indent 1))
  `(progn
     ,@(cl-loop
        for entry in entries
        collect
        `(haskell-tests--test-result
             ,(string->symbol
               (concat (symbol->string name)
                       (symbol->string (or (plist-get entry :subname)
                                           (error "No :subname in entries")))))
           :action ,(or (plist-get entry :action)
                        (error "No :action in entries"))
           :expected-value ,(or (plist-get entry :expected-value)
                                (error "No :expected-value in entrties"))
           :contents ,contents
           :modes ,modes))))

(cl-defmacro haskell-tests--test-result (name &key action expected-value contents modes)
  (declare (indent 1))
  `(haskell-tests--test-results ,name
                                :actions-and-values ,(list (list action expected-value))
                                :contents ,contents
                                :modes ,modes))

(cl-defmacro haskell-tests--test-results (name &key actions-and-values contents modes)
  (declare (indent 1))
  `(progn
     ,@(cl-loop
        for mode in (or modes '(haskell-mode haskell-ts-mode))
        collect
        `(ert-deftest ,(string->symbol (format "%s/%s" name mode)) ()
           (tests-utils--with-temp-buffer
             :action
             (progn
               ,@(cl-loop
                  for entry in actions-and-values
                  collect
                  `(should (equal ,(car entry) ,(cadr entry)))))
             :contents ,contents
             :initialisation (,mode)
             :buffer-id ,(string->symbol (format "haskell-tests-%s" mode)))))))

;; (defmacro haskell-tests--test-evaluate (action contents expected-value)
;;   (declare (indent 1))
;;   `(tests-utils--with-temp-buffer
;;      :action ,action
;;      :contents ,contents
;;      :initialisation (haskell-mode)))

(haskell-tests--make-multiple-input-test-buffer-contents*
  :action
  (let ((start (point)))
    (forward-line 2)
    (goto-char (line-end-position))
    (set-mark start)
    (activate-mark)
    (haskell-align-on-comments))
  :entries
  ((haskell-tests/haskell-align-on-comments-1a
    (tests-utils--multiline
     ""
     "foo = do"
     "  _|_bar -- a"
     "  baz \t\t-- b"
     "  decombobulate -- c"
     ""))
   (haskell-tests/haskell-align-on-comments-1b
    (tests-utils--multiline
     ""
     "foo = do"
     "  _|_bar -- a"
     "  baz -- b"
     "  decombobulate -- c"
     ""))
   (haskell-tests/haskell-align-on-comments-1c
    (tests-utils--multiline
     ""
     "foo = do"
     "  _|_bar\t-- a"
     "  baz\t\t\t-- b"
     "  decombobulate\t-- c"
     ""))
   (haskell-tests/haskell-align-on-comments-1d
    (tests-utils--multiline
     ""
     "foo = do"
     "  _|_bar                                    -- a"
     "  baz                                    -- b"
     "  decombobulate -- c"
     ""))
   (haskell-tests/haskell-align-on-comments-1e
    (tests-utils--multiline
     ""
     "foo = do"
     "  _|_bar                                    -- a"
     "  baz                                    -- b"
     "  decombobulate\t-- c"
     "")))
  :expected-value
  (tests-utils--multiline
   ""
   "foo = do"
   "  bar           -- a"
   "  baz           -- b"
   "  decombobulate -- c_|_"
   "")
  :modes (haskell-mode haskell-ts-mode)
  :fresh-buffer t)

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-language-pragmas-1
    (haskell-align-language-pragmas (point))
  (tests-utils--multiline
   ""
   ""
   "{-# language"
   "             Safe, FlexibleContexts _|_ #-}"
   "")
  (tests-utils--multiline
   ""
   ""
   "{-# LANGUAGE FlexibleContexts #-}"
   "{-# LANGUAGE Safe             #-}_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-language-pragmas-2
    (haskell-align-language-pragmas (point))
  (tests-utils--multiline
   ""
   "-- foobar"
   "{-# language"
   "             Safe, FlexibleContexts _|_ #-}"
   "")
  (tests-utils--multiline
   ""
   "-- foobar"
   "{-# LANGUAGE FlexibleContexts #-}"
   "{-# LANGUAGE Safe             #-}_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-language-pragmas-3
    (haskell-align-language-pragmas (point))
  (tests-utils--multiline
   ""
   ""
   "{-# language"
   "   Safe _|_"
   " , FlexibleContexts"
   " #-}"
   "")
  (tests-utils--multiline
   ""
   ""
   "{-# LANGUAGE FlexibleContexts #-}"
   "{-# LANGUAGE Safe             #-}_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-language-pragmas-4
    (haskell-align-language-pragmas (point))
  (tests-utils--multiline
   ""
   "-- foo"
   "{-# language"
   "   Safe _|_"
   " , FlexibleContexts"
   " #-}"
   "-- bar")
  (tests-utils--multiline
   ""
   "-- foo"
   "{-# LANGUAGE FlexibleContexts #-}"
   "{-# LANGUAGE Safe             #-}_|_"
   "-- bar"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-language-pragmas-5
    (haskell-align-language-pragmas (point))
  (tests-utils--multiline
   "{-# LANGUAGE Safe #-}"
   "{-# LANGUAGE AlternativeLayoutRule _|_ #-}"
   "{-# LANGUAGE AllowAmbiguousTypes   #-}")
  (tests-utils--multiline
   "{-# LANGUAGE AllowAmbiguousTypes   #-}"
   "{-# LANGUAGE AlternativeLayoutRule #-}"
   "{-# LANGUAGE Safe                  #-}_|_"))

;; Nothing should happen, no errors and point should not move.
(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-options-ghc-pragmas-0
    (haskell-align-language-pragmas (point))
  (tests-utils--multiline
   "{-# OPTIONS_GHC -Wno-orphans #-}"
   "{-# OPTIONS_GHC -Wno-implicit-prelude_|_ #-}"
   "{-# OPTIONS_GHC -O0   #-}")
  (tests-utils--multiline
   "{-# OPTIONS_GHC -Wno-orphans #-}"
   "{-# OPTIONS_GHC -Wno-implicit-prelude_|_ #-}"
   "{-# OPTIONS_GHC -O0   #-}"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-options-ghc-pragmas-1
    (haskell-align-options-ghc-pragmas (point))
  (tests-utils--multiline
   "{-# OPTIONS_GHC -Wno-orphans #-}"
   "{-# OPTIONS_GHC -Wno-implicit-prelude_|_ #-}"
   "{-# OPTIONS_GHC -O0   #-}")
  (tests-utils--multiline
   "{-# OPTIONS_GHC -O0                   #-}"
   "{-# OPTIONS_GHC -Wno-implicit-prelude #-}"
   "{-# OPTIONS_GHC -Wno-orphans          #-}_|_"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-options-ghc-pragmas-2
    (haskell-align-options-ghc-pragmas (point))
  (tests-utils--multiline
   ""
   "{-# LANGUAGE DerivingVia          #-}"
   "{-# LANGUAGE UndecidableInstances #-}"
   ""
   "{-# OPTIONS_GHC -Wno-unused-top-binds #-}"
   "{-# _|_OPTIONS_GHC -Wno-unused-imports #-}"
   "{-# OPTIONS_GHC -Wno-orphans #-}"
   "")
  (tests-utils--multiline
   ""
   "{-# LANGUAGE DerivingVia          #-}"
   "{-# LANGUAGE UndecidableInstances #-}"
   ""
   "{-# OPTIONS_GHC -Wno-orphans          #-}"
   "{-# OPTIONS_GHC -Wno-unused-imports   #-}"
   "{-# OPTIONS_GHC -Wno-unused-top-binds #-}_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-options-ghc-pragmas-3a
    (haskell-align-options-ghc-pragmas (point))
  (tests-utils--multiline
   ""
   "{-# _|_OPTIONS_GHC -Wno-unused-imports      #-}"
   "")
  (tests-utils--multiline
   ""
   "{-# OPTIONS_GHC -Wno-unused-imports #-}_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-options-ghc-pragmas-3b
    (haskell-align-options-ghc-pragmas (point))
  (tests-utils--multiline
   ""
   "{-# OPTIONS_GHC -Wno-unused-imports      _|_#-}"
   "")
  (tests-utils--multiline
   ""
   "{-# OPTIONS_GHC -Wno-unused-imports #-}_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-options-ghc-pragmas-3c
    (haskell-align-options-ghc-pragmas (point))
  (tests-utils--multiline
   ""
   "_|_{-# OPTIONS_GHC -Wno-unused-imports      #-}"
   "")
  (tests-utils--multiline
   ""
   "{-# OPTIONS_GHC -Wno-unused-imports #-}_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-options-ghc-pragmas-4a
    (pcase major-mode
      (`haskell-ts-mode
       (haskell-ts-reindent-at-point))
      (`haskell-mode
       (haskell-reindent-at-point))
      (_
       (error "Unhandled major mode: %s" major-mode)))
  (tests-utils--multiline
   ""
   "{-# _|_OPTIONS_GHC -Wno-unused-imports      #-}"
   "")
  (tests-utils--multiline
   ""
   "{-# _|_OPTIONS_GHC -Wno-unused-imports #-}"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-options-ghc-pragmas-4b
    (pcase major-mode
      (`haskell-ts-mode
       (haskell-ts-reindent-at-point))
      (`haskell-mode
       (haskell-reindent-at-point))
      (_
       (error "Unhandled major mode: %s" major-mode)))
  (tests-utils--multiline
   ""
   "{-# OPTIONS_GHC -Wno-unused-imports      _|_#-}"
   "")
  (tests-utils--multiline
   ""
   "{-# OPTIONS_GHC -Wno-unused-imports #-}_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-options-ghc-pragmas-4c
    (pcase major-mode
      (`haskell-ts-mode
       (haskell-ts-reindent-at-point))
      (`haskell-mode
       (haskell-reindent-at-point))
      (_
       (error "Unhandled major mode: %s" major-mode)))
  (tests-utils--multiline
   ""
   "_|_{-# OPTIONS_GHC -Wno-unused-imports      #-}"
   "")
  (tests-utils--multiline
   ""
   "_|_{-# OPTIONS_GHC -Wno-unused-imports #-}"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-align-options-ghc-pragmas-4d
    (pcase major-mode
      (`haskell-ts-mode
       (haskell-ts-reindent-at-point))
      (`haskell-mode
       (haskell-reindent-at-point))
      (_
       (error "Unhandled major mode: %s" major-mode)))
  (tests-utils--multiline
   ""
   "{-# LANGUAGE DerivingVia #-}"
   ""
   "_|_{-# OPTIONS_GHC -Wno-unused-imports      #-}"
   "")
  (tests-utils--multiline
   ""
   "{-# LANGUAGE DerivingVia #-}"
   ""
   "_|_{-# OPTIONS_GHC -Wno-unused-imports #-}"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-reindent-at-point-1
    (haskell-reindent-at-point)
  (tests-utils--multiline
   ""
   "{-# LANGUAGE Safe #-}"
   "{-# LANGUAGE AlternativeLayoutRule_|_  #-}"
   "{-# LANGUAGE AllowAmbiguousTypes   #-}"
   "")
  (tests-utils--multiline
   ""
   "{-# LANGUAGE AllowAmbiguousTypes   #-}"
   "{-# LANGUAGE AlternativeLayoutRule_|_ #-}"
   "{-# LANGUAGE Safe                  #-}"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-reindent-at-point-2
    (haskell-reindent-at-point)
  (tests-utils--multiline
   ""
   "{-# OPTIONS_GHC -Wno-orphans #-}"
   "{-# OPTIONS_GHC -Wno-implicit-prelude_|_ #-}"
   "{-# OPTIONS_GHC -O0   #-}"
   "")
  (tests-utils--multiline
   ""
   "{-# OPTIONS_GHC -O0                   #-}"
   "{-# OPTIONS_GHC -Wno-implicit-prelude_|_ #-}"
   "{-# OPTIONS_GHC -Wno-orphans          #-}"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-reindent-at-point-3
    (haskell-reindent-at-point)
  (tests-utils--multiline
   ""
   "import Data.Text qualified as T"
   "import Data_|_.Bimap (Bimap)"
   "import Data.Bifunctors"
   "import qualified Data.Bimap as BM"
   "")
  (tests-utils--multiline
   ""
   "import Data.Bifunctors"
   "import Data_|_.Bimap (Bimap)"
   "import qualified Data.Bimap as BM"
   "import Data.Text qualified as T"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-reindent-at-point-4
    (haskell-reindent-at-point)
  (tests-utils--multiline
   ""
   "import{-#source#-}Data.Text qualified as T"
   "import Data_|_.Bimap (Bimap)"
   "import {-# SOURCE #-} Data.Bifunctors"
   "import qualified \"bimap\" Data.Bimap as BM"
   "")
  (tests-utils--multiline
   ""
   "import {-# SOURCE #-} Data.Bifunctors"
   "import Data_|_.Bimap (Bimap)"
   "import qualified \"bimap\" Data.Bimap as BM"
   "import{-#source#-}Data.Text qualified as T"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-reindent-at-point-5
    (haskell-reindent-at-point)
  (tests-utils--multiline
   ""
   "#ifdef FOO"
   "import Data.Text qualified as T"
   "import Data_|_.Bimap (Bimap)"
   "import Data.Bifunctors"
   "import qualified Data.Bimap as BM"
   "#endif"
   "")
  (tests-utils--multiline
   ""
   "#ifdef FOO"
   "import Data.Bifunctors"
   "import Data_|_.Bimap (Bimap)"
   "import qualified Data.Bimap as BM"
   "import Data.Text qualified as T"
   "#endif"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-reindent-at-point-6
    (haskell-reindent-at-point)
  (tests-utils--multiline
   ""
   "#ifdef FOO"
   "import Data.Text qualified as T"
   "import Data.Bimap"
   "  ( Bimap_|_"
   "  )"
   "import Data.Bifunctors"
   "import qualified Data.Bimap as BM"
   "#endif"
   "")
  (tests-utils--multiline
   ""
   "#ifdef FOO"
   "import Data.Bifunctors"
   "import Data.Bimap"
   "  ( Bimap_|_"
   "  )"
   "import qualified Data.Bimap as BM"
   "import Data.Text qualified as T"
   "#endif"
   ""))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-reindent-at-point-1
 :modes
 (haskell-ts-mode)
 :action
 (haskell-ts-reindent-at-point)
 :contents
 (concat
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo x ="
   "    _|_case x of")
  "\n"
  (mapconcat (lambda (i) (format "        %s -> %s + 1" i i))
             (cl-loop for i from 0 to treesit--indent-region-batch-size collect i)
             "\n")
  "\n")
 :expected-value
 (concat
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo x ="
   "  _|_case x of")
  "\n"
  (mapconcat (lambda (i) (format "    %s -> %s + 1" i i))
             (cl-loop for i from 0 to treesit--indent-region-batch-size collect i)
             "\n")
  "\n"))

(ert-deftest haskell-tests/haskell-indentation--add-to-sorted-list! ()
  (dolist (entry '((0 ()        (0))
                   (0 (2 4 6 8) (0 2 4 6 8))
                   (1 (2 4 6 8) (1 2 4 6 8))
                   (2 (2 4 6 8) (2 4 6 8))
                   (3 (2 4 6 8) (2 3 4 6 8))
                   (4 (2 4 6 8) (2 4 6 8))
                   (5 (2 4 6 8) (2 4 5 6 8))
                   (6 (2 4 6 8) (2 4 6 8))
                   (7 (2 4 6 8) (2 4 6 7 8))
                   (8 (2 4 6 8) (2 4 6 8))
                   (9 (2 4 6 8) (2 4 6 8 9))
                   (10 (2 4 6 8) (2 4 6 8 10))))
    (let ((elem (car entry))
          (input (copy-list (cadr entry)))
          (expected (caddr entry)))
      (should (equal (haskell-indentation--add-to-sorted-list! input elem)
                     expected)))))

(ert-deftest haskell-tests/haskell-regexen/ghci-info-definition-site ()
  (save-match-data
    (let ((str "type Range :: *\ndata Range = Range \{..., _end :: !Position}\n  	-- Defined in ‘lsp-types-1.4.0.1:Language.LSP.Types.Location’"))
      (should (string-match haskell-regexen/ghci-info-definition-site str))
      (should (equal (match-string 1 str)
                     "lsp-types-1.4.0.1:Language.LSP.Types.Location")))))

(ert-deftest haskell-tests/haskell-go-to-symbol-home--strip-ghci-packages-of-versions ()
  (let ((sample-input "active package flags:\n  -package-id base-4.15.1.0\n  -package-id aeson-2.0.3.0-e91573e5a9f0a74731f7cb1fe08486dfa1990213df0c4f864e51b791370cc73d"))
    (should (equal (haskell-go-to-symbol-home--strip-ghci-packages-of-versions sample-input)
                   '("base" "aeson")))))

(ert-deftest haskell-tests/haskell-regexen/ghci-name-not-in-scope-error ()
  (let ((sample-input "<interactive>:1:1: error: Not in scope: ‘locStart’"))
    (should (string-match-p haskell-regexen/ghci-name-not-in-scope-error
                            sample-input))))

(ert-deftest haskell-tests/haskell-regexen/ghci-src-span ()
  (let ((sample-input "X.hs:(8,7)-(8,9)")
        (sample-input2 "hls-plugin-api-1.3.0.0:hls-plugin-api-1.3.0.0:Ide.Types"))
    (should (string-match-p haskell-regexen/ghci-src-span sample-input))
    (should-not (string-match-p haskell-regexen/ghci-src-span sample-input2))))

(ert-deftest haskell-tests/haskell-regexen/ghci-loc-at-external-symbol ()
  (let ((sample-input "hls-plugin-api-1.3.0.0:hls-plugin-api-api-1.3.0.0:Ide.Types"))
    (save-match-data
      (should (string-match haskell-regexen/ghci-loc-at-external-symbol sample-input))
      (should (equal "hls-plugin-api" (match-string 1 sample-input)))
      (should (equal "hls-plugin-api-api" (match-string 2 sample-input)))
      (should (equal "Ide.Types" (match-string 3 sample-input))))))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-1a
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   "-- Glushkov construction of glob patterns."
   "data PatternF a"
   "  = Star"
   "  | Sym _|_!Char"
   "  | Or a a"
   "  | Seq a a"
   "  deriving (Eq, Ord, Show, Foldable, Traversable, Generic, Generic1)"
   "  deriving Pretty via PPGeneric (PatternF a)")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-1b
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   "-- Glushkov construction of glob patterns."
   "data PatternF a"
   "  = _|_Star"
   "  | Sym !Char"
   "  | Or a a"
   "  | Seq a a"
   "  deriving (Eq, Ord, Show, Foldable, Traversable, Generic, Generic1)"
   "  deriving Pretty via PPGeneric (PatternF a)")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-1c
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   "-- Glushkov construction of glob patterns."
   "data PatternF a"
   "  = Star"
   "  | Sym !Char"
   "  | Or a a"
   "  | Seq a a"
   "  deriving (Eq, _|_Ord, Show, Foldable, Traversable, Generic, Generic1)"
   "  deriving Pretty via PPGeneric (PatternF a)")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-1d
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   "-- Glushkov construction of glob patterns."
   "data PatternF a"
   "  = Star"
   "  | Sym !Char"
   "  | Or a a_|_"
   "  | Seq a a"
   "  deriving (Eq, Ord, Show, Foldable, Traversable, Generic, Generic1)"
   "  deriving Pretty via PPGeneric (PatternF a)")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-1e
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   "-- Glushkov construction of glob patterns."
   "data PatternF a"
   "  = Star"
   "  | Sym !Char"
   "  _|_| Or a a"
   "  | Seq a a"
   "  deriving (Eq, Ord, Show, Foldable, Traversable, Generic, Generic1)"
   "  deriving Pretty via PPGeneric (PatternF a)")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-1f
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   "-- Glushkov construction of glob patterns."
   "data PatternF a"
   "  _|_= Star"
   "  | Sym !Char"
   "  | Or a a"
   "  | Seq a a"
   "  deriving (Eq, Ord, Show, Foldable, Traversable, Generic, Generic1)"
   "  deriving Pretty via PPGeneric (PatternF a)")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-2
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   "-- Glushkov construction of glob patterns."
   "data PatternF a"
   "  = Star"
   "  | Sym ##_|_ !Char"
   "  | Or a a"
   "  | Seq a a"
   "  deriving (Eq, Ord, Show, Foldable, Traversable, Generic, Generic1)"
   "  deriving Pretty via PPGeneric (PatternF a)")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-3a
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   ""
   "data Foo = Foo"
   "  { bar :: _|_Int"
   "  , baz :: Map String Double"
   "  }"
   "")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-3b
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   ""
   "data Foo = Foo"
   "  { bar :: Int"
   "  , baz :: Map String _|_Double"
   "  }"
   "")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-3c
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   ""
   "data Foo = Foo"
   "  { bar :: Int"
   "  , _|_baz :: Map String Double"
   "  }"
   "")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-3d
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   ""
   "data Foo = Foo"
   "  { bar :: Int"
   "  _|_, baz :: Map String Double"
   "  }"
   "")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/haskell-abbrev+--within-data-type?-3e
  :action
  (not (null (haskell-abbrev+--within-data-type?)))
  :expected-value
  t
  :contents
  (tests-utils--multiline
   ""
   "data Foo = Foo"
   "  _|_{ bar :: Int"
   "  , baz :: Map String Double"
   "  }"
   "")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/forward-haskell-symbol-1
  :action
  (list
   (bounds-of-thing-at-point 'haskell-symbol)
   (thing-at-point 'haskell-symbol))
  :expected-value
  (list
   (cons 2 9)
   "fooobar")
  :contents
  " fooo_|_bar ")

(haskell-tests--test-result
    haskell-tests/forward-haskell-symbol-2
  :action
  (list
   (bounds-of-thing-at-point 'haskell-symbol)
   (thing-at-point 'haskell-symbol))
  :expected-value
  (list
   (cons 2 9)
   "Fooobar")
  :contents
  " Fooo_|_bar ")

(haskell-tests--test-result
    haskell-tests/forward-haskell-symbol-3
  :action
  (list
   (bounds-of-thing-at-point 'haskell-symbol)
   (thing-at-point 'haskell-symbol))
  :expected-value
  (list
   (cons 3 10)
   "Fooobar")
  :contents
  " 'Fooo_|_bar ")

(haskell-tests--test-result
    haskell-tests/forward-haskell-symbol-4
  :action
  (list
   (bounds-of-thing-at-point 'haskell-symbol)
   (thing-at-point 'haskell-symbol))
  :expected-value
  (list
   (cons 4 11)
   "Fooobar")
  :contents
  " ''Fooo_|_bar ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-1
  :action
  (thing-at-point 'haskell-symbol)
  :expected-value
  "Fooobar"
  :contents
  " ''Fooo_|_bar ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-2
  :action
  (thing-at-point 'haskell-symbol)
  :expected-value
  "Fooobar"
  :contents
  " ''Quux.Fooo_|_bar ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-2-qualified
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "Quux.Fooobar"
  :contents
  " ''Quux.Fooo_|_bar ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-2a-qualified
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "Żółć.Quux.Fooobar"
  :contents
  " ''Żółć.Quux.Fooo_|_bar ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-2b
  :action
  (thing-at-point 'haskell-symbol)
  :expected-value
  "fooobar"
  :contents
  " ''Quux.Żółć.fooo_|_bar ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-2b-qualified
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "Quux.Żółć.fooobar"
  :contents
  " ''Quux.Żółć.fooo_|_bar ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-2c
  :action
  (thing-at-point 'haskell-symbol)
  :expected-value
  "++"
  :contents
  " Quux.Żółć.+_|_+ ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-2c-qualified
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "Quux.Żółć.++"
  :contents
  " Quux.Żółć.+_|_+ ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-3
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  ".=?"
  :contents
  " .=_|_? ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-3a
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  ".=?"
  :contents
  " (.=_|_?) ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-4
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  ".=?"
  :contents
  " Quux..=_|_? ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-4-qualified
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "Quux..=?"
  :contents
  " Quux..=_|_? ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-5
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  ".=?"
  :contents
  " ''Quux..=_|_? ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-5-qualified
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "Quux..=?"
  :contents
  " ''Quux..=_|_? ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-6
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "Fooobar"
  :contents
  " `Fooo_|_bar` ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-6a
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "Fooobar"
  :contents
  " `Quux.Baz.Fooo_|_bar` ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-6a-qualified
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "Quux.Baz.Fooobar"
  :contents
  " `Quux.Baz.Fooo_|_bar` ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-7
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "myosstr"
  :contents
  " [_|_myosstr|test|] ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-7a
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "myosstr"
  :contents
  " [myos_|_str|test|] ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-7b
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "myosstr"
  :contents
  " [myosst_|_r|test|] ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-7c
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "myosstr"
  :contents
  " [Żółć.Foo.Bar.myos_|_str|test|] ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-7c-qualified
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "Żółć.Foo.Bar.myosstr"
  :contents
  " [Żółć.Foo.Bar.myos_|_str|test|] ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-8
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "test"
  :contents
  " _|_test++ ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-8a
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "test"
  :contents
  " ++_|_test ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-8b
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "test"
  :contents
  " ++tes_|_t ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-8c
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "test_prim#"
  :contents
  " ++Żółć.Foo.Bar.tes_|_t_prim# ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-8c-qualified
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "Żółć.Foo.Bar.test_prim#"
  :contents
  " ++Żółć.Foo.Bar.tes_|_t_prim# ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-9
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "."
  :contents
  " f ((Foo.Bar._|_.) g) ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-10-qualified
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "Foo.Bar.."
  :contents
  " f ((Foo._|_Bar..) g) ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-11
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  "|++|"
  :contents
  " f |+_|_+| ")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-12
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  ":"
  :contents
  " x _|_: xs")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-12a
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  ":"
  :contents
  " x _|_:xs")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-12b
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  ":"
  :contents
  " x_|_: xs")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-12c
  :action
  (thing-at-point 'qualified-haskell-symbol)
  :expected-value
  ":"
  :contents
  " x_|_:xs")

(haskell-tests--test-result
    haskell-tests/bounds-of-haskell-symbol-13
  :action
  (substring-no-properties (thing-at-point 'haskell-symbol))
  :expected-value
  "100"
  :contents
  " _|_100 ")

(haskell-tests--test-result
    haskell-tests/bounds-of-ghc-core-symbol-1
  :action
  (substring-no-properties (thing-at-point 'ghc-core-symbol))
  :expected-value
  "$test_prim#"
  :contents
  " ++$tes_|_t_prim# "
  :modes (haskell-ts-mode ghc-core-mode text-mode))

(haskell-tests--test-result
    haskell-tests/bounds-of-ghc-core-symbol-2
  :action
  (substring-no-properties (thing-at-point 'ghc-core-symbol))
  :expected-value
  "$w$sexecMatch"
  :contents
  " ++$w$sexecMa_|_tch "
  :modes (haskell-ts-mode ghc-core-mode text-mode))

(ert-deftest haskell-tests/haskell-cabal--yasnippet--main-module-from-main-file-1 ()
  (should
   (string=
    (haskell-cabal--yasnippet--main-module-from-main-file "Main.hs")
    "Main")))

(ert-deftest haskell-tests/haskell-cabal--yasnippet--main-module-from-main-file-2 ()
  (should
   (string=
    (haskell-cabal--yasnippet--main-module-from-main-file "Foo/Bar.hs")
    "Foo.Bar")))

(ert-deftest haskell-tests/haskell-cabal--yasnippet--main-module-from-main-file-3 ()
  (should
   (string=
    (haskell-cabal--yasnippet--main-module-from-main-file "tests/MyExe.hs")
    "MyExe")))

(ert-deftest haskell-tests/haskell-cabal--yasnippet--main-module-from-main-file-4 ()
  (should
   (string=
    (haskell-cabal--yasnippet--main-module-from-main-file "tests/Foo/Bar.hs")
    "Foo.Bar")))

(ert-deftest haskell-tests/haskell-cabal--yasnippet--main-module-from-main-file-5 ()
  (should
   (string=
    (haskell-cabal--yasnippet--main-module-from-main-file "Main")
    "Main")))

(ert-deftest haskell-tests/haskell-cabal--yasnippet--main-module-from-executable-name-1 ()
  (dolist (input '("foo-bar"
                   "foo-bar.exe"
                   "exe/foo-bar"
                   "exe/foo-bar.exe"
                   "Test/Bar/foo-bar"))
    (should
     (string=
      (haskell-cabal--yasnippet--main-module-from-executable-name input)
      "FooBar"))))

(ert-deftest haskell-tests/haskell-cabal--yasnippet--main-module-from-executable-name-2 ()
  (dolist (input '("foo-bar-baz---quux"
                   "foo-bar-baz---quux.exe"
                   "exe/foo-bar-baz---quux.exe"
                   "Test/Bar/foo-bar-baz---quux.exe"))
    (should
     (string=
      (haskell-cabal--yasnippet--main-module-from-executable-name input)
      "FooBarBazQuux"))))

(ert-deftest haskell-tests/haskell-cabal--yasnippet--main-module-from-executable-name-3 ()
  (dolist (input '("MyExe"
                   "MyExe.exe"
                   "exe/MyExe"
                   "exe/MyExe.exe"
                   "Test/Bar/MyExe.exe"
                   "myExe"
                   "myExe.exe"
                   "Test/Bar/myExe.exe"))
    (should
     (string=
      (haskell-cabal--yasnippet--main-module-from-executable-name input)
      "MyExe"))))

(ert-deftest haskell-tests/haskell-cabal--yasnippet--main-module-from-executable-name-4 ()
  (dolist (input '("test"
                   "test.exe"
                   "tests/test.exe"
                   "Exe/Bar/test.exe"
                   "Exe/Bar/test"))
    (should
     (string=
      (haskell-cabal--yasnippet--main-module-from-executable-name input)
      "Test"))))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--prepend-to-prev-operator-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  "x = 1 +_|_2"
  "x = 1 ++ _|_2")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--prepend-to-prev-operator-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  "x = 1 +           _|_ 2"
  "x = 1 ++_|_ 2")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--prepend-to-prev-operator-3
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
  "x = 1 +           _|_"
  "x = 1 +* _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--prepend-to-prev-operator-4
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  "x = 1_|_"
  "x = 1 + _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--prepend-to-prev-operator-5
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  "x = 1 _|_"
  "x = 1 + _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--prepend-to-prev-operator-6
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  "x = 1  _|_"
  "x = 1  + _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--prepend-to-prev-operator-7
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  "x = f \(_|_"
  "x = f \(+ _|_")

(haskell-tests--test-buffer-contents*
 :name haskell-tests/haskell-smart-operators--prepend-to-prev-operator-8
 :action (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
 :contents
 "x = [foo| 1 + _|_x|]"
 :expected-value
 "x = [foo| 1 + +_|_x|]"
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name haskell-tests/haskell-smart-operators--prepend-to-prev-operator-9
 :action (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
 :contents
 "x = [foo| 1 + x|]_|_y"
 :expected-value
 "x = [foo| 1 + x|] + _|_y"
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--inserting-@-avoid-spaces-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
  "x = x_|_y"
  "x = x@_|_y")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--inserting-@-avoid-spaces-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
  "x = x _|_y"
  "x = x @_|_y")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--inserting-@-avoid-spaces-3
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
  ;; If @ was appended to an operator then do insert a space after it!
  "x = x +_|_y"
  "x = x +@ _|_y")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--inserting-@-avoid-spaces-4
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
  ;; If @ was appended to an operator then do insert a space after it!
  "x = x +  _|_y"
  "x = x +@ _|_y")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--inserting-@-avoid-spaces-5
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
  ;; If @ was appended to an operator then do insert a space after it!
  "x = x +  _|_  y"
  "x = x +@_|_  y")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--@-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 _|_)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 @_|_)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--inserting-within-backtics-avoids-spaces-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
  "x = x `andI_|_` y"
  "x = x `andI#_|_` y")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--inserting-within-backtics-avoids-spaces-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
  "x = x `_|_` y"
  "x = x `#_|_` y")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--sections-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  "x = f \(_|_\)"
  "x = f \(+_|_\)")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--sections-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  "x = f \(     _|_\)"
  "x = f \(+_|_\)")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--space-after--lambdas-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?\\)
  "x = \(_|_\)"
  "x = \(\\_|_\)")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--space-after--lambdas-2a
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?$)
  "x = f _|_\\ x -> x"
  "x = f $ _|_\\ x -> x")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--space-after--lambdas-2b
    (haskell-smart-operators-$)
  "x = f _|_\\ x -> x"
  "x = f $ _|_\\ x -> x")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--oxford-brackets-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "x = \[_|_"
  "x = \[| _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--oxford-brackets-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "x = \[| foobar _|_\]"
  "x = \[| foobar |_|_\]")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--oxford-brackets-3
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "x = \[foo_|_"
  "x = \[foo| _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--oxford-brackets-4
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "x = \[fooBar_|_"
  "x = \[fooBar| _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--oxford-brackets-5
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "x = \[fooBar'_|_"
  "x = \[fooBar'| _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--oxford-brackets-6
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "x = \[_fooBar_|_"
  "x = \[_fooBar| _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--oxford-brackets-7
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "x = \[Quux.fooBar_|_"
  "x = \[Quux.fooBar| _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--oxford-brackets-8
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "x = \[a+b_|_"
  "x = \[a+b | _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--beginning-of-buffer
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
  "_|_ + bar"
  " *_|_ + bar")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--end-of-buffer
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
  "+ bar_|_"
  "+ bar * _|_")


(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "--_|_ foobar"
  "-- |_|_ foobar")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
  "--_|_ foobar"
  "-- ^_|_ foobar")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-3
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "--     _|_ foobar"
  "-- |_|_ foobar")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-4
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
  "--      _|_ foobar"
  "-- ^_|_ foobar")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-no-action-if-not-toplevel-comment-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  --      _|_ foobar"
   "  pure baz")
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  --      ^_|_ foobar"
   "  pure baz"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-no-action-if-not-toplevel-comment-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  --      _|_ foobar"
   "  pure baz")
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  --      |_|_ foobar"
   "  pure baz"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-no-action-if-not-toplevel-comment-3
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  --      _|_ foobar"
   "  pure baz")
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  --      +_|_ foobar"
   "  pure baz"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-insertion-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "--    _|_"
  "-- | _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-insertion-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
  "--    _|_"
  "-- ^ _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-insertion-3
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  "--    _|_"
  "--    +_|_")


(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-insertion-disabled-if-not-on-first-column-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  " --    _|_"
  " --    |_|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-insertion-disabled-if-not-on-first-column-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
  " --    _|_"
  " --    ^_|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--haddock-comments-insertion-disabled-if-not-on-first-column-3
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
  " --    _|_"
  " --    +_|_")


(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--almost-haddock-comments-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  "-       _|_"
  "-| _|_")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--almost-haddock-comments-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
  "-      _|_"
  "-^ _|_")


(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-1
    (haskell-smart-operators-$)
  "x = f _|_(xs ++ ys)"
  "x = f $ _|_xs ++ ys")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-2
    (haskell-smart-operators-$)
  "x = f_|_ (xs ++ ys)"
  "x = f $ _|_xs ++ ys")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-3
    (haskell-smart-operators-$)
  "x = f _|_ (xs ++ ys)"
  "x = f $ _|_xs ++ ys")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-4
    (haskell-smart-operators-$)
  "x = f _|_     (xs ++ ys)"
  "x = f $ _|_xs ++ ys")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-5
    (haskell-smart-operators-$)
  "x = f _|_(xs ++ ys)"
  "x = f $ _|_xs ++ ys")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-6
    (haskell-smart-operators-$)
  "x = f_|_(xs ++ ys)"
  "x = f $ _|_xs ++ ys")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$--keeps-parens-for-fmap-operator-1
    (haskell-smart-operators-$)
  "x = f <_|_(xs ++ ys)"
  "x = f <$ _|_(xs ++ ys)")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-2
    (haskell-smart-operators-$)
  "x = f<_|_ (xs ++ ys)"
  "x = f<$_|_ (xs ++ ys)")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-3
    (haskell-smart-operators-$)
  "x = f <_|_ (xs ++ ys)"
  "x = f <$_|_ (xs ++ ys)")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-4
    (haskell-smart-operators-$)
  "x = f <_|_  (xs ++ ys)"
  "x = f <$_|_  (xs ++ ys)")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-5
    (haskell-smart-operators-$)
  "x = f <  _|_  (xs ++ ys)"
  "x = f <$_|_  (xs ++ ys)")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-6
    (haskell-smart-operators-$)
  "x = f <_|_(xs ++ ys)"
  "x = f <$ _|_(xs ++ ys)")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-7
    (haskell-smart-operators-$)
  "x = f<_|_(xs ++ ys)"
  "x = f<$ _|_(xs ++ ys)")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-before-double-quotes-1
    (haskell-smart-operators-$)
  "x = error _|_\"foobar\" ++ show baz"
  "x = error $ _|_\"foobar\" ++ show baz")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-within-double-quotes-1
    (haskell-smart-operators-$)
  "x = error \"_|_foobar\""
  "x = error \"$_|_foobar\"")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-within-double-quotes-2
    (haskell-smart-operators-$)
  "x = error \"foobar_|_\""
  "x = error \"foobar$_|_\"")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-within-double-quotes-3
    (haskell-smart-operators-$)
  "x = error \"foobar_|_baz\""
  "x = error \"foobar$_|_baz\"")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-backtick-call-1
    (haskell-smart-operators-$)
  "x = decombobulate_|_(`foo` y) $ bar baz quux"
  "x = decombobulate $ _|_(`foo` y) $ bar baz quux")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-backtick-call-2
    (haskell-smart-operators-$)
  "x = decombobulate_|_( `foo` y) $ bar baz quux"
  "x = decombobulate $ _|_( `foo` y) $ bar baz quux")

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--guard-1
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  (tests-utils--multiline
   "foo x xs"
   "  | _|_x `elem` xs = xs"
   "  | otherwise   = []")
  (tests-utils--multiline
   "foo x xs"
   "  | | _|_x `elem` xs = xs"
   "  | otherwise   = []"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--guard-2
    (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
  (tests-utils--multiline
   "foo x xs"
   "  | x `elem` xs |_|_= xs"
   "  | otherwise   = []")
  (tests-utils--multiline
   "foo x xs"
   "  | x `elem` xs ||_|_= xs"
   "  | otherwise   = []"))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-0
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-magic-hash?))
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = (bar_|_, baz)"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = (bar#_|_, baz)"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-1
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-magic-hash?))
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = foo_|_ +# bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = foo#_|_ +# bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-2
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-magic-hash?))
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = foo _|_ +# bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = foo#_|_ +# bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-3
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-magic-hash?))
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = foo _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = foo# _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-4
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-magic-hash?))
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = foo #  _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = foo ## _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-5
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = foo _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = foo# # _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-6
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 1 _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 1# _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-6a
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45 _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45# _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-6b
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45e-2 _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45e-2# _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-7
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 1 _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 1## _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-7a
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45 _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45## _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-7b
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45e-2 _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45e-2## _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-8
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 1 _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 1## # _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-8a
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45 _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45## # _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-8b
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45e-11 _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  "foo x xs = 123.45e-11## # _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-and-equals-space-1
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators-hash)
   (should (haskell-ext-tracking-have-magic-hash?))
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?=))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  ""
  "  where"
  "    ptr_|_"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  ""
  "  where"
  "    ptr# = _|_"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--magic-hash-and-equals-space-2
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-magic-hash?))
   (haskell-smart-operators--insert-char-surrounding-with-spaces ?=))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  ""
  "  where"
  "    ptr#_|_"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MagicHash #-}"
  ""
  "  where"
  "    ptr# = _|_"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--dot-1
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators-dot))
 :contents
 (tests-utils--multiline
  ""
  "foo x xs = foo .+  _|_bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo x xs = foo .+. _|_bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--dot-2
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators-dot))
 :contents
 (tests-utils--multiline
  ""
  "foo x xs = foo .+  _|_ bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo x xs = foo .+._|_ bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--dot-3
 :action
 (progn
   (haskell-smart-operators-mode +1)
   (haskell-ext-tracking-mode +1)
   (haskell-smart-operators-dot))
 :contents
 (tests-utils--multiline
  ""
  "foo x xs = foo <_|_> bar"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo x xs = foo <._|_> bar"
  "")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--arrows-in-non-haddock-comment-1
    (progn
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
      (insert "value2"))
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  putDocLn $ ppDict \"foobar\""
   "    [ \"label1\" --> value1"
   "    , \"label2\" _|_"
   "    , \"label3\" --> value3"
   "    ]"
   "  pure baz")
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  putDocLn $ ppDict \"foobar\""
   "    [ \"label1\" --> value1"
   "    , \"label2\" --> value2_|_"
   "    , \"label3\" --> value3"
   "    ]"
   "  pure baz"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--arrows-in-non-haddock-comment-2
    (progn
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
      (insert "value2"))
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  putDocLn $ ppDict \"foobar\""
   "    [ \"label1\" --> value1"
   "    , \"label2\" _|_"
   "    , \"label3\" --> value3"
   "    ]"
   "  pure baz")
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  putDocLn $ ppDict \"foobar\""
   "    [ \"label1\" --> value1"
   "    , \"label2\" |-> value2_|_"
   "    , \"label3\" --> value3"
   "    ]"
   "  pure baz"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--arrows-in-non-haddock-comment-3
    (progn
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
      (insert "value2"))
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  putDocLn $ ppDict \"foobar\""
   "    [ \"label1\" --> value1"
   "    , \"label2\" _|_"
   "    , \"label3\" --> value3"
   "    ]"
   "  pure baz")
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  putDocLn $ ppDict \"foobar\""
   "    [ \"label1\" --> value1"
   "    , \"label2\" ---> value2_|_"
   "    , \"label3\" --> value3"
   "    ]"
   "  pure baz"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--arrows-in-non-haddock-comment-4
    (progn
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
      (insert "value2"))
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  putDocLn $ ppDict \"foobar\""
   "    [ \"label1\" --> value1"
   "    , \"label2\" _|_"
   "    , \"label3\" --> value3"
   "    ]"
   "  pure baz")
  (tests-utils--multiline
   "foo = do"
   "  bar"
   "  putDocLn $ ppDict \"foobar\""
   "    [ \"label1\" --> value1"
   "    , \"label2\" --->>> value2_|_"
   "    , \"label3\" --> value3"
   "    ]"
   "  pure baz"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--arrows-in-haddock-comment-1
    (progn
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
      (insert "test"))
  (tests-utils--multiline
   "-- _|_")
  (tests-utils--multiline
   "-- >test_|_"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--arrows-1
    (progn
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
      (insert "foo"))
  (tests-utils--multiline
   ""
   "writeTo :: Handle -> [(OsPath, [Token.Pos Tag.TagVal])] -> IO ()"
   "writeTo dest xs ="
   "    for_ xs $ \\(fn, tags) _|_"
   "")
  (tests-utils--multiline
   ""
   "writeTo :: Handle -> [(OsPath, [Token.Pos Tag.TagVal])] -> IO ()"
   "writeTo dest xs ="
   "    for_ xs $ \\(fn, tags) -> foo_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--arrows-2
    (progn
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
      (insert "foo"))
  (tests-utils--multiline
   ""
   "writeTo :: Handle -> [(OsPath, [Token.Pos Tag.TagVal])] -> IO ()"
   "writeTo dest xs ="
   "    for_ xs $ \\(fn, tags)_|_"
   "")
  (tests-utils--multiline
   ""
   "writeTo :: Handle -> [(OsPath, [Token.Pos Tag.TagVal])] -> IO ()"
   "writeTo dest xs ="
   "    for_ xs $ \\(fn, tags) -> foo_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--arrows-3
    (progn
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
      (insert "foo"))
  (tests-utils--multiline
   ""
   "writeTo :: Handle -> [(OsPath, [Token.Pos Tag.TagVal])] -> IO ()"
   "writeTo dest xs ="
   "    for_ xs $ \\(fn, tags)_|_"
   "    -- = map (uncurry formatFileTags)"
   "")
  (tests-utils--multiline
   ""
   "writeTo :: Handle -> [(OsPath, [Token.Pos Tag.TagVal])] -> IO ()"
   "writeTo dest xs ="
   "    for_ xs $ \\(fn, tags) -> foo_|_"
   "    -- = map (uncurry formatFileTags)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-hyphen--1
    (progn
      (haskell-smart-operators-hyphen)
      (insert "test"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {_|_}bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test_|_ -}bar x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-hyphen--2
    (progn
      (haskell-smart-operators-hyphen)
      (insert "test"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  { _|_}bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test_|_ -}bar x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-hyphen--3
    (progn
      (haskell-smart-operators-hyphen)
      (insert "test"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {_|_ }bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test_|_ -}bar x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-hyphen--4
    (progn
      (haskell-smart-operators-hyphen)
      (insert "test"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  { _|_ }bar x"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test_|_ -}bar x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-hyphen--5
    (progn
      (haskell-smart-operators-hyphen)
      (insert "test"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  { _|_ bar} x"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test_|_ bar} x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-hyphen--6
    (progn
      (haskell-smart-operators-hyphen)
      (insert "test"))
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {_|_ bar} x"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test_|_ bar} x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-hyphen--7
    (haskell-smart-operators-hyphen)
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test bar_|_} x"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test bar_|_ -} x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-hyphen--8
    (haskell-smart-operators-hyphen)
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test bar _|_} x"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test bar _|_ -} x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-hyphen--9
    (haskell-smart-operators-hyphen)
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test bar  _|_} x"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  {- test bar  _|_ -} x"
   ""))


(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-exclamation-mark--1
    (haskell-smart-operators-exclamation-mark)
  (tests-utils--multiline
   ""
   "foo x = x + _|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = x +! _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-exclamation-mark--2
    (haskell-smart-operators-exclamation-mark)
  (tests-utils--multiline
   ""
   "foo x = x !_|_y"
   "")
  (tests-utils--multiline
   ""
   "foo x = x !! _|_y"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-exclamation-mark--3
    (haskell-smart-operators-exclamation-mark)
  (tests-utils--multiline
   ""
   "foo x = x_|_y"
   "")
  (tests-utils--multiline
   ""
   "foo x = x !_|_y"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-exclamation-mark--4
    (haskell-smart-operators-exclamation-mark)
  (tests-utils--multiline
   ""
   "foo x = \\_|_y -> x + y"
   "")
  (tests-utils--multiline
   ""
   "foo x = \\ !_|_y -> x + y"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-exclamation-mark--5
    (haskell-smart-operators-exclamation-mark)
  (tests-utils--multiline
   ""
   "foo x = \\ _|_y -> x + y"
   "")
  (tests-utils--multiline
   ""
   "foo x = \\ !_|_y -> x + y"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-exclamation-mark--6
    (haskell-smart-operators-exclamation-mark)
  (tests-utils--multiline
   ""
   "foo _|_x = x + y"
   "")
  (tests-utils--multiline
   ""
   "foo !_|_x = x + y"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-exclamation-mark--7
    (haskell-smart-operators-exclamation-mark)
  (tests-utils--multiline
   ""
   "data Foo = Foo { bar :: _|_Int }"
   "")
  (tests-utils--multiline
   ""
   "data Foo = Foo { bar :: !_|_Int }"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators-exclamation-mark--8
    (haskell-smart-operators-exclamation-mark)
  (tests-utils--multiline
   ""
   "data Foo = Foo { bar ::_|_Int }"
   "")
  (tests-utils--multiline
   ""
   "data Foo = Foo { bar :: !_|_Int }"
   ""))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-1a
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo ::_|_ Set Int"
  "  , bar :: Map Int Double"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo ::_|_ !(Set Int)"
  "  , bar :: Map Int Double"
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-1b
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: _|_Set Int"
  "  , bar :: Map Int Double"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: !(_|_Set Int)"
  "  , bar :: Map Int Double"
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-1b-identity
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: !(_|_Set Int)"
  "  , bar :: Map Int Double"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: !(_|_Set Int)"
  "  , bar :: Map Int Double"
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-1c
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: Set _|_Int"
  "  , bar :: Map Int Double"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: !(Set _|_Int)"
  "  , bar :: Map Int Double"
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-1aa
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo ::_|_ (Set Int)"
  "  , bar :: Map Int Double"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo ::_|_ !(Set Int)"
  "  , bar :: Map Int Double"
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-1ba
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: _|_(Set Int)"
  "  , bar :: Map Int Double"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: !_|_(Set Int)"
  "  , bar :: Map Int Double"
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-1ca
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: (Set _|_Int)"
  "  , bar :: Map Int Double"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: !(Set _|_Int)"
  "  , bar :: Map Int Double"
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-2
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: _|_ Set Int"
  "  , bar :: Map Int Double"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: _|_ !(Set Int)"
  "  , bar :: Map Int Double"
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-3
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo ::      _|_      Set Int"
  "  , bar :: Map Int Double"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo ::      _|_      !(Set Int)"
  "  , bar :: Map Int Double"
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-4
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: Set Int"
  "  , bar :: Map Int Double"
  "  , baz :: _|_"
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: Set Int"
  "  , bar :: Map Int Double"
  "  , baz :: !_|_"
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-5
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: Set Int"
  "  , bar :: Map Int Double"
  "  , baz ::      _|_     "
  "  }")
 :expected-value
 (tests-utils--multiline
  "data Foo = Foo"
  "  { foo :: Set Int"
  "  , bar :: Map Int Double"
  "  , baz ::      !_|_     "
  "  }")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-6a
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Pattern a = Foo _|_Int"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Pattern a = Foo !_|_Int"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-6b
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Pattern a = Foo In_|_t"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Pattern a = Foo !In_|_t"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-7a
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Pattern a = Foo _|_(Bar Int)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Pattern a = Foo !_|_(Bar Int)"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-7b
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Pattern a = Foo (Bar In_|_t)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Pattern a = Foo !(Bar In_|_t)"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-8a
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Pattern a = Foo (Bar _|_Int) | String `Bar` [a] | a :+: a"
  "")
 :expected-value
(tests-utils--multiline
  ""
  "data Pattern a = Foo !(Bar _|_Int) | String `Bar` [a] | a :+: a"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-8b
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Pattern a = Foo !(Bar _|_Int) | String `Bar` [a] | a :+: a"
  "")
 :expected-value
(tests-utils--multiline
  ""
  "data Pattern a = Foo !(Bar _|_Int) | String `Bar` [a] | a :+: a"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-8c
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Pattern a = Foo (Bar Int) | Stri_|_ng `Bar` [a] | a :+: a"
  "")
 :expected-value
(tests-utils--multiline
  ""
  "data Pattern a = Foo (Bar Int) | !Stri_|_ng `Bar` [a] | a :+: a"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-8d
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Pattern a = Foo (Bar Int) | String `Bar` [a_|_] | a :+: a"
  "")
 :expected-value
(tests-utils--multiline
  ""
  "data Pattern a = Foo (Bar Int) | String `Bar` ![a_|_] | a :+: a"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-8e
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Pattern a = Foo (Bar Int) | String `Bar` [a] | a :+: (Bar _|_a)"
  "")
 :expected-value
(tests-utils--multiline
  ""
  "data Pattern a = Foo (Bar Int) | String `Bar` [a] | a :+: !(Bar _|_a)"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-9a
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Foo a where"
  "  Foo"
  "    :: Int"
  "    -> Ba_|_r a"
  "    -> Foo a"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Foo a where"
  "  Foo"
  "    :: Int"
  "    -> !(Ba_|_r a)"
  "    -> Foo a"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-9b
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Foo a where"
  "  Foo"
  "    :: In_|_t"
  "    -> Bar a"
  "    -> Foo a"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Foo a where"
  "  Foo"
  "    :: !In_|_t"
  "    -> Bar a"
  "    -> Foo a"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-9c
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Foo a where"
  "  Foo"
  "    :: Int_|_"
  "    -> Bar a"
  "    -> Foo a"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Foo a where"
  "  Foo"
  "    :: !Int_|_"
  "    -> Bar a"
  "    -> Foo a"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-9d
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Foo a where"
  "  Foo"
  "    :: _|_Int"
  "    -> Bar a"
  "    -> Foo a"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Foo a where"
  "  Foo"
  "    :: !_|_Int"
  "    -> Bar a"
  "    -> Foo a"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-field-strictness-10a
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "data Foo a where"
  "  Foo"
  "    :: Int"
  "#if _|_defined(FOO)"
  "    -> Bar a"
  "#endif"
  "    -> Foo a"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "data Foo a where"
  "  Foo"
  "    :: Int"
  "#if !_|_defined(FOO)"
  "    -> Bar a"
  "#endif"
  "    -> Foo a"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-1a
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    _|_TU.Iter c' delta ="
  "      TU.iterArray arr i"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    !(_|_TU.Iter c' delta) ="
  "      TU.iterArray arr i"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-1b
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    (_|_TU.Iter c' delta) ="
  "      TU.iterArray arr i"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    !(_|_TU.Iter c' delta) ="
  "      TU.iterArray arr i"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-1c
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    TU.I_|_ter c' delta ="
  "      TU.iterArray arr i"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    !(TU.I_|_ter c' delta) ="
  "      TU.iterArray arr i"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-1d
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    TU.Iter_|_ c' delta ="
  "      TU.iterArray arr i"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    !(TU.Iter_|_ c' delta) ="
  "      TU.iterArray arr i"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-1aa
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    !(TU.I_|_ter c' delta) ="
  "      TU.iterArray arr i"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    !(TU.I_|_ter c' delta) ="
  "      TU.iterArray arr i"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-1e
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    TU.Iter c' del_|_ta ="
  "      TU.iterArray arr i"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    TU.Iter c' !del_|_ta ="
  "      TU.iterArray arr i"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-1f
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    !(TU.Iter c' del_|_ta) ="
  "      TU.iterArray arr i"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str = undefined"
  "  where"
  "    !(TU.Iter c' !del_|_ta) ="
  "      TU.iterArray arr i"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-2a
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str ="
  "  let _|_TU.Iter c' delta ="
  "        TU.iterArray arr i"
  "  in foo"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str ="
  "  let !(_|_TU.Iter c' delta) ="
  "        TU.iterArray arr i"
  "  in foo"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-2b
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str ="
  "  let (TU.Ite_|_r c' delta) ="
  "        TU.iterArray arr i"
  "  in foo"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str ="
  "  let !(TU.Ite_|_r c' delta) ="
  "        TU.iterArray arr i"
  "  in foo"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-2c
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str ="
  "  let TU.Iter c' delt_|_a ="
  "        TU.iterArray arr i"
  "  in foo"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "parseGlobPattern :: Text -> Pattern"
  "parseGlobPattern str ="
  "  let TU.Iter c' !delt_|_a ="
  "        TU.iterArray arr i"
  "  in foo"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-3a
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "foo :: Text -> Pattern"
  "foo (TI_|_.Text arr off len) = undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Text -> Pattern"
  "foo !(TI_|_.Text arr off len) = undefined"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-3b
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "foo :: Text -> Pattern"
  "foo str@(TI_|_.Text arr off len) = undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Text -> Pattern"
  "foo !str@(TI_|_.Text arr off len) = undefined"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-3c
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "foo :: Text -> Pattern"
  "foo (TI.Text arr off len) (TI.Text arr2 off2 _|_len2) = undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Text -> Pattern"
  "foo (TI.Text arr off len) (TI.Text arr2 off2 !_|_len2) = undefined"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-3d
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "foo :: Text -> Pattern"
  "foo (TI.Text arr off len) (TI.Text arr2 off2 len_|_2) = undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Text -> Pattern"
  "foo (TI.Text arr off len) (TI.Text arr2 off2 !len_|_2) = undefined"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-3e
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "foo :: Text -> Pattern"
  "foo (TI.Text arr off len) (TI.Text arr2 off2 len2_|_) = undefined"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: Text -> Pattern"
  "foo (TI.Text arr off len) !(TI.Text arr2 off2 len2_|_) = undefined"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-4
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "foo = bar"
  "  where"
  "    en_|_d = last + 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = bar"
  "  where"
  "    !en_|_d = last + 1"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-5
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "foo = bar"
  "  where"
  "    (start, en_|_d) = quux 1"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = bar"
  "  where"
  "    (start, !en_|_d) = quux 1"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-6a
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "foo = do"
  "#if defined(FOO)"
  "  let xxx = Foo"
  "        { yyy = zzz"
  "        }"
  "  fst <$> bar"
  "    bar1"
  "    bar2"
  "    bar3"
  "#endif"
  "#if _|_defined(FOO)"
  "  let xxx = Foo"
  "        { yyy = zzz"
  "        }"
  "  fst <$> quux"
  "    quux1"
  "    quux2"
  "#endif"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = do"
  "#if defined(FOO)"
  "  let xxx = Foo"
  "        { yyy = zzz"
  "        }"
  "  fst <$> bar"
  "    bar1"
  "    bar2"
  "    bar3"
  "#endif"
  "#if !_|_defined(FOO)"
  "  let xxx = Foo"
  "        { yyy = zzz"
  "        }"
  "  fst <$> quux"
  "    quux1"
  "    quux2"
  "#endif"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators-exclamation-mark-pattern-strictness-6b
 :action
 (haskell-smart-operators-exclamation-mark)
 :contents
 (tests-utils--multiline
  ""
  "foo = do"
  "#if defined(FOO)"
  "  let xxx = Foo"
  "        { yyy = zzz"
  "        }"
  "  fst <$> bar"
  "    bar1"
  "    bar2"
  "    bar3"
  "#endif"
  "#if !defined(FOO)"
  "  let xxx = Foo"
  "        { yyy = zzz"
  "        }"
  "  fst <$> quux"
  "    quux1"
  "    quux2_|_"
  "#endif"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = do"
  "#if defined(FOO)"
  "  let xxx = Foo"
  "        { yyy = zzz"
  "        }"
  "  fst <$> bar"
  "    bar1"
  "    bar2"
  "    bar3"
  "#endif"
  "#if !defined(FOO)"
  "  let xxx = Foo"
  "        { yyy = zzz"
  "        }"
  "  fst <$> quux"
  "    quux1"
  "    quux2 !_|_"
  "#endif"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-backspace-with-block-dedent-1
    (haskell-backspace-with-block-dedent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    _|_(foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "   _|_(foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-backspace-with-block-dedent-not-at-exact-indentation-1
    (haskell-backspace-with-block-dedent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "   _|_ (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "   (ExactPrint.Types.mkAnnKey x)"
   "  _|_ (foldedAnnKeys x)"
   "   (let foo y = bar . baz"
   "          where"
   "            bar = (+1)"
   "            baz = (+y) . (*2)"
   "    in foo 100)"
   "   shouldAddComment"
   "   (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "   foo"
   "   bar"
   "#wat"
   ""
   "   baz"
   "#are"
   "   you"
   "   doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-backspace-with-block-dedent-not-at-exact-indentation-2
    (haskell-backspace-with-block-dedent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "  _|_  (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "   (ExactPrint.Types.mkAnnKey x)"
   " _|_  (foldedAnnKeys x)"
   "   (let foo y = bar . baz"
   "          where"
   "            bar = (+1)"
   "            baz = (+y) . (*2)"
   "    in foo 100)"
   "   shouldAddComment"
   "   (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "   foo"
   "   bar"
   "#wat"
   ""
   "   baz"
   "#are"
   "   you"
   "   doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-backspace-with-block-dedent-2
    (haskell-backspace-with-block-dedent 2)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    _|_(foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "  _|_(foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-backspace-with-block-dedent-3
    (haskell-backspace-with-block-dedent 3)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    _|_(foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   " _|_(foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-backspace-with-block-dedent-4
    (progn
      (haskell-backspace-with-block-dedent)
      (haskell-backspace-with-block-dedent)
      (haskell-backspace-with-block-dedent))
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    _|_(foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "  (ExactPrint.Types.mkAnnKey x)"
   " _|_(foldedAnnKeys x)"
   "  (let foo y = bar . baz"
   "         where"
   "           bar = (+1)"
   "           baz = (+y) . (*2)"
   "   in foo 100)"
   "  shouldAddComment"
   "  (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "  foo"
   "  bar"
   "#wat"
   ""
   "  baz"
   "#are"
   "  you"
   "  doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-backspace-with-block-dedent-not-at-indentation-1
    (haskell-backspace-with-block-dedent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = _|_bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y =_|_bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-space-with-block-indent-1
    (haskell-space-with-block-indent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    _|_(foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "     _|_(foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-space-with-block-indent-not-at-exact-indentation-1
    (haskell-space-with-block-indent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "   _|_ (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "     (ExactPrint.Types.mkAnnKey x)"
   "    _|_ (foldedAnnKeys x)"
   "     (let foo y = bar . baz"
   "            where"
   "              bar = (+1)"
   "              baz = (+y) . (*2)"
   "      in foo 100)"
   "     shouldAddComment"
   "     (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "     foo"
   "     bar"
   "#wat"
   ""
   "     baz"
   "#are"
   "     you"
   "     doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-space-with-block-indent-not-at-exact-indentation-2
    (haskell-space-with-block-indent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "  _|_  (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "     (ExactPrint.Types.mkAnnKey x)"
   "   _|_  (foldedAnnKeys x)"
   "     (let foo y = bar . baz"
   "            where"
   "              bar = (+1)"
   "              baz = (+y) . (*2)"
   "      in foo 100)"
   "     shouldAddComment"
   "     (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "     foo"
   "     bar"
   "#wat"
   ""
   "     baz"
   "#are"
   "     you"
   "     doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-space-with-block-indent-not-at-exact-indentation-affects-where-block-1
    (haskell-space-with-block-indent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   " _|_   (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "     (ExactPrint.Types.mkAnnKey x)"
   "  _|_   (foldedAnnKeys x)"
   "     (let foo y = bar . baz"
   "            where"
   "              bar = (+1)"
   "              baz = (+y) . (*2)"
   "      in foo 100)"
   "     shouldAddComment"
   "     (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "     foo"
   "     bar"
   "#wat"
   ""
   "     baz"
   "#are"
   "     you"
   "     doing"
   "   where"
   "     hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-space-with-block-indent-2
    (haskell-space-with-block-indent 2)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             _|_bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "               _|_bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-space-with-block-indent-not-at-indentation-1
    (haskell-space-with-block-indent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = _|_bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y =  _|_bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-space-with-block-indent-from-zeroth-column-1
    (haskell-space-with-block-indent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "_|_docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   " _|_docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "     (ExactPrint.Types.mkAnnKey x)"
   "     (foldedAnnKeys x)"
   "     (let foo y = bar . baz"
   "            where"
   "              bar = (+1)"
   "              baz = (+y) . (*2)"
   "      in foo 100)"
   "     shouldAddComment"
   "     (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "     foo"
   "     bar"
   "#wat"
   ""
   "     baz"
   "#are"
   "     you"
   "     doing"
   "   where"
   "     hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-space-with-block-indent-from-zeroth-column-2
    (haskell-space-with-block-indent)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "_|_docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   " _|_docExt"
   "   :: (ExactPrint.Annotate.Annotate ast)"
   "   => Located ast"
   "   -> ExactPrint.Types.Anns"
   "   -> Bool"
   "   -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-space-with-block-indent--inside-module-export-list-1
    (haskell-space-with-block-indent)
  (tests-utils--multiline
   ""
   "module Foo"
   "_|_"
   "where"
   "")
  (tests-utils--multiline
   ""
   "module Foo"
   " _|_"
   "where"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int_|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int"
   "foo _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-ws-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int_|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int"
   "foo _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-2
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int    _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int"
   "foo _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-ws-2
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int    _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int"
   "foo _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-3
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int -> Int_|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int -> Int"
   "foo _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-ws-3
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int -> Int_|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int -> Int"
   "foo _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-4
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int -> Int   _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int -> Int"
   "foo _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-ws-4
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int -> Int   _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int -> Int"
   "foo _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-5
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int -> Int_|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int -> Int"
   "foo _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-ws-5
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int -> Int_|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int -> Int"
   "foo _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-6
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int -> Int   _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int -> Int"
   "foo _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-ws-6
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int -> Int   _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int -> Int"
   "foo _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-7
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int"
   "  -> Int_|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int"
   "  -> Int"
   "foo _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-ws-7
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int"
   "  -> Int_|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int"
   "  -> Int"
   "foo _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-8
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int"
   "  -> Int   _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int"
   "  -> Int"
   "foo _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-ws-8
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int"
   "  -> Int   _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int"
   "  -> Int"
   "foo _|_"
   "    "
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-9
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "(.!=) :: Functor m => m (Maybe a) -> a -> m a_|_"
   ""
   "")
  (tests-utils--multiline
   ""
   "(.!=) :: Functor m => m (Maybe a) -> a -> m a"
   "(.!=) _|_"
   ""
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-ws-9
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "(.!=) :: Functor m => m (Maybe a) -> a -> m a_|_"
   "    "
   "")
  (tests-utils--multiline
   ""
   "(.!=) :: Functor m => m (Maybe a) -> a -> m a"
   "(.!=) _|_"
   "    "
   ""))

;; Cannot distinguish unfinished but indented body from dangling type signature.
(haskell-tests--test-buffer-contents-expect-failed
    haskell-tests/haskell-newline-with-signature-expansion-ws-10
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "processInput :: Env -> IO ()_|_"
   "    res <- processInputResultFromNum <$> Env.processInput env"
   "")
  (tests-utils--multiline
   ""
   "processInput :: Env -> IO ()_|_"
   "processInput _|_"
   "    res <- processInputResultFromNum <$> Env.processInput env"
   ""
   ""))

(haskell-tests--make-multiple-input-test-buffer-contents*
 :action
 (haskell-newline-with-signature-expansion)
 :entries
 ((haskell-tests/haskell-newline-with-signature-expansion-11a
   (tests-utils--multiline
    ""
    "pFoo :: Mega.Parsec Void Text (Foo, Bar)"
    "pFoo = do"
    "  x <- Mega.anySingle"
    "  case x of"
    "_|_"
    "    'F' -> Foo  <$ Mega.chunk \"oo\""
    "    'B' -> Bar  <$ Mega.chunk \"ar\""
    "    'Q' -> Quux <$ Mega.chunk \"uux\""
    "    y   -> do"
    "      ys <- Mega.takeWhileP Nothing isAlphaNum"
    "      fail $ \"Invalid foo: \" ++ show (T.cons y ys)"
    ""))
  (haskell-tests/haskell-newline-with-signature-expansion-11b
   (tests-utils--multiline
    ""
    "pFoo :: Mega.Parsec Void Text (Foo, Bar)"
    "pFoo = do"
    "  x <- Mega.anySingle"
    "  case x of"
    "    _|_"
    "    'F' -> Foo  <$ Mega.chunk \"oo\""
    "    'B' -> Bar  <$ Mega.chunk \"ar\""
    "    'Q' -> Quux <$ Mega.chunk \"uux\""
    "    y   -> do"
    "      ys <- Mega.takeWhileP Nothing isAlphaNum"
    "      fail $ \"Invalid foo: \" ++ show (T.cons y ys)"
    "")))
 :expected-value
 (tests-utils--multiline
  ""
  "pFoo :: Mega.Parsec Void Text (Foo, Bar)"
  "pFoo = do"
  "  x <- Mega.anySingle"
  "  case x of"
  ""
  "_|_"
  "    'F' -> Foo  <$ Mega.chunk \"oo\""
  "    'B' -> Bar  <$ Mega.chunk \"ar\""
  "    'Q' -> Quux <$ Mega.chunk \"uux\""
  "    y   -> do"
  "      ys <- Mega.takeWhileP Nothing isAlphaNum"
  "      fail $ \"Invalid foo: \" ++ show (T.cons y ys)"
  "")
 :modes (haskell-mode haskell-ts-mode))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-12
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "class GenericDiff a where"
   "  genericDiff :: a -> a -> [FieldDiff]"
   "  default genericDiff :: (Generic a, GGenericDiff (Rep a)) => a -> a -> [FieldDiff]_|_"
   "")
  (tests-utils--multiline
   ""
   "class GenericDiff a where"
   "  genericDiff :: a -> a -> [FieldDiff]"
   "  default genericDiff :: (Generic a, GGenericDiff (Rep a)) => a -> a -> [FieldDiff]"
   "  genericDiff _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-13
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "instance GenericDiff foo where"
   "  genericDiff :: a -> a -> [FieldDiff]_|_"
   "")
  (tests-utils--multiline
   ""
   "instance GenericDiff foo where"
   "  genericDiff :: a -> a -> [FieldDiff]"
   "  genericDiff _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--within-where-block-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int"
   "foo = go"
   "  where"
   "    go :: a -> a_|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int"
   "foo = go"
   "  where"
   "    go :: a -> a"
   "    go _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--within-where-block-2
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int"
   "foo = go"
   "  where"
   "    go :: a -> a  _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo :: Int -> Int"
   "foo = go"
   "  where"
   "    go :: a -> a"
   "    go _|_"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--does-not-expand-if-not-at-the-end-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int_|_"
   "  -> Int"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int"
   "  _|_"
   "  -> Int"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--does-not-expand-if-not-at-the-end-2
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int    _|_"
   "  -> Int"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo"
   "  :: Int"
   "  _|_"
   "  -> Int"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--does-not-expand-if-not-at-the-end-3
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int -> _|_Int"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int ->"
   "    _|_Int"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--does-not-expand-if-not-at-the-end-4
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int ->  _|_Int"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int ->"
   "    _|_Int"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--does-not-expand-if-not-at-the-end-5
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int ->  _|_ Int"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   "")
  (tests-utils--multiline
   ""
   "bar1 :: a -> x"
   "bar1 x = x"
   ""
   "foo ::"
   "  Int ->"
   "    _|_ Int"
   ""
   "bar2 :: a -> x"
   "bar2 x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--deep-within-do-block-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "generateGrafts :: HasCallStack => GenerateGraftsConfig -> IO ()"
   "generateGrafts GenerateGraftsConfig{ggcOutputFile, ggcOverwriteOutput} = do"
   "  mrepo <- findRepoMaybe"
   "  repo  <- case mrepo of"
   "    Nothing ->"
   "      error \"Failed to find git repository starting at current directory\""
   "    Just x  -> pure x"
   "  withRepo repo $ \\git -> do"
   "    let branches = [RefName \"dev\"] -- [, RefName \"master\"]    -- <- toList <$> branchList git"
   "    branchesRefs <- for branches $ \branchRefName -> do"
   "      let rev = Revision.fromString $ refNameRaw branchRefName"
   "      mref <- resolveRevision git rev"
   "      case mref of"
   "        Nothing -> error $ \"Failed to resolve revision: \" ++ show rev"
   "        Just x  -> pure x"
   "    putStrLn $ \"Known branches: \" ++ show (map refNameRaw branches)"
   "    (_, entries) <- runWriterT $ traverseAllCommitsTransitively git processCommit branchesRefs"
   "    let graftsContents_|_graftsContents = formatGraftEntries entries"
   "    outFile <- makeAbsolute ggcOutputFile"
   "    exists  <- doesFileExist outFile"
   "    TLIO.putStrLn $ TL.replicate 40 $ TL.singleton '-'"
   "    TLIO.putStrLn \"New grafts contents:\""
   "    TLIO.putStrLn graftsContents"
   "    TLIO.putStrLn $ TL.replicate 40 $ TL.singleton '-'"
   "    case (exists, ggcOverwriteOutput) of"
   "      (False, _)     -> TLIO.writeFile (toFilePath outFile) graftsContents"
   "      (True,  True)  -> do"
   "        outFileBak <- outFile <.> \"bak\""
   "        let outFile'    = toFilePath outFile"
   "            outFileBak' = toFilePath outFileBak"
   "        putStrLn $ \"Creating backup of \" ++ outFile' ++ \" at \" ++ outFileBak'"
   "        renameFile outFile outFileBak"
   "        TLIO.writeFile outFile' graftsContents"
   "      (True,  False) -> error $"
   "        \"Target file '\" ++ toFilePath outFile ++ \"' already exists. Refusing to overwrite (specify --force to override).\""
   "    putStrLn $ \"New grafts written to \" ++ toFilePath outFile"
   "  where"
   "    processCommit :: Ref -> Commit -> WriterT [GraftEntry] IO ()"
   "    processCommit commitRef commit ="
   "      when (\"Merge commit\" `T.isInfixOf` msg) $"
   "        case commitParents commit of"
   "          [parentRef] ->"
   "            case parseMergeCommit msg of"
   "              Left err          -> error $"
   "                \"Malformed merge commit: \" ++ T.unpack msg ++ \"\nError: \" ++ err"
   "              Right mergeCommit -> do"
   "                liftIO $ TIO.putStrLn $ \"Found merge commit \" <> T.pack (show commitRef) <> \": \" <> T.pack (show mergeCommit)"
   "                tell $ (:[]) GraftEntry"
   "                  { geTargetHash     = TE.decodeLatin1 $ Ref.toHex commitRef"
   "                  , geRealParentHash = TE.decodeLatin1 $ Ref.toHex parentRef"
   "                  , geFakeParentHash = mcMergedSHA mergeCommit"
   "                  , geMergeCommit    = mergeCommit"
   "                  }"
   "          unexpected -> error $"
   "            \"Merge commit '\" ++ show commitRef ++ \"' has unexpected number of parents: \" ++ show unexpected"
   "      where"
   "        msg = T.strip $ TE.decodeUtf8 $ commitMessage commit"
   "")
  (tests-utils--multiline
   ""
   "generateGrafts :: HasCallStack => GenerateGraftsConfig -> IO ()"
   "generateGrafts GenerateGraftsConfig{ggcOutputFile, ggcOverwriteOutput} = do"
   "  mrepo <- findRepoMaybe"
   "  repo  <- case mrepo of"
   "    Nothing ->"
   "      error \"Failed to find git repository starting at current directory\""
   "    Just x  -> pure x"
   "  withRepo repo $ \\git -> do"
   "    let branches = [RefName \"dev\"] -- [, RefName \"master\"]    -- <- toList <$> branchList git"
   "    branchesRefs <- for branches $ \branchRefName -> do"
   "      let rev = Revision.fromString $ refNameRaw branchRefName"
   "      mref <- resolveRevision git rev"
   "      case mref of"
   "        Nothing -> error $ \"Failed to resolve revision: \" ++ show rev"
   "        Just x  -> pure x"
   "    putStrLn $ \"Known branches: \" ++ show (map refNameRaw branches)"
   "    (_, entries) <- runWriterT $ traverseAllCommitsTransitively git processCommit branchesRefs"
   "    let graftsContents"
   "        _|_graftsContents = formatGraftEntries entries"
   "    outFile <- makeAbsolute ggcOutputFile"
   "    exists  <- doesFileExist outFile"
   "    TLIO.putStrLn $ TL.replicate 40 $ TL.singleton '-'"
   "    TLIO.putStrLn \"New grafts contents:\""
   "    TLIO.putStrLn graftsContents"
   "    TLIO.putStrLn $ TL.replicate 40 $ TL.singleton '-'"
   "    case (exists, ggcOverwriteOutput) of"
   "      (False, _)     -> TLIO.writeFile (toFilePath outFile) graftsContents"
   "      (True,  True)  -> do"
   "        outFileBak <- outFile <.> \"bak\""
   "        let outFile'    = toFilePath outFile"
   "            outFileBak' = toFilePath outFileBak"
   "        putStrLn $ \"Creating backup of \" ++ outFile' ++ \" at \" ++ outFileBak'"
   "        renameFile outFile outFileBak"
   "        TLIO.writeFile outFile' graftsContents"
   "      (True,  False) -> error $"
   "        \"Target file '\" ++ toFilePath outFile ++ \"' already exists. Refusing to overwrite (specify --force to override).\""
   "    putStrLn $ \"New grafts written to \" ++ toFilePath outFile"
   "  where"
   "    processCommit :: Ref -> Commit -> WriterT [GraftEntry] IO ()"
   "    processCommit commitRef commit ="
   "      when (\"Merge commit\" `T.isInfixOf` msg) $"
   "        case commitParents commit of"
   "          [parentRef] ->"
   "            case parseMergeCommit msg of"
   "              Left err          -> error $"
   "                \"Malformed merge commit: \" ++ T.unpack msg ++ \"\nError: \" ++ err"
   "              Right mergeCommit -> do"
   "                liftIO $ TIO.putStrLn $ \"Found merge commit \" <> T.pack (show commitRef) <> \": \" <> T.pack (show mergeCommit)"
   "                tell $ (:[]) GraftEntry"
   "                  { geTargetHash     = TE.decodeLatin1 $ Ref.toHex commitRef"
   "                  , geRealParentHash = TE.decodeLatin1 $ Ref.toHex parentRef"
   "                  , geFakeParentHash = mcMergedSHA mergeCommit"
   "                  , geMergeCommit    = mergeCommit"
   "                  }"
   "          unexpected -> error $"
   "            \"Merge commit '\" ++ show commitRef ++ \"' has unexpected number of parents: \" ++ show unexpected"
   "      where"
   "        msg = T.strip $ TE.decodeUtf8 $ commitMessage commit"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--inside-string-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   "quux = "
   "  [ foobar"
   "  , \"T.makeInstances [2..6]_|_\" ==> []"
   "  , baz "
   "  ]")
  (tests-utils--multiline
   "quux = "
   "  [ foobar"
   "  , \"T.makeInstances [2..6]\\"
   "    \\_|_\" ==> []"
   "  , baz "
   "  ]"))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-newline-with-signature-expansion--inside-string-2
 :action
 (haskell-newline-with-signature-expansion)
 :contents
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  , \"\"\"T.makeInstances [2..6]_|_\"\"\" ==> []"
  "  , baz "
  "  ]")
 :expected-value
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  , \"\"\"T.makeInstances [2..6]"
  "    _|_\"\"\" ==> []"
  "  , baz "
  "  ]")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--outside-string-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   "quote :: Char -> ByteString"
   "quote = \\case"
   "  '\"'  -> \"\\\\\\\"\"_|_"
   "  '\\\\' -> \"\\\\\\\\\""
   "  c    -> C8.singleton c")
  (tests-utils--multiline
   "quote :: Char -> ByteString"
   "quote = \\case"
   "  '\"'  -> \"\\\\\\\"\""
   "  _|_"
   "  '\\\\' -> \"\\\\\\\\\""
   "  c    -> C8.singleton c"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--outside-string-2a
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   "quux = "
   "  [ foobar"
   "  , \"T.makeInstances [2..6]\"_|_ ==> []"
   "  , baz "
   "  ]")
  (tests-utils--multiline
   "quux = "
   "  [ foobar"
   "  , \"T.makeInstances [2..6]\""
   "  _|_ ==> []"
   "  , baz "
   "  ]"))

(haskell-tests--test-buffer-contents
 haskell-tests/haskell-newline-with-signature-expansion--outside-string-3a
 (haskell-newline-with-signature-expansion)
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  , _|_\"T.makeInstances [2..6]\" ==> []"
  "  , baz "
  "  ]")
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  ,"
  "  _|_\"T.makeInstances [2..6]\" ==> []"
  "  , baz "
  "  ]"))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-newline-with-signature-expansion--outside-string-2ba
 :action
 (haskell-newline-with-signature-expansion)
 :contents
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  , \"\"\"T.makeInstances [2..6]\"\"\"_|_ ==> []"
  "  , baz "
  "  ]")
 :expected-value
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  , \"\"\"T.makeInstances [2..6]\"\"\""
  "  _|_ ==> []"
  "  , baz "
  "  ]")
 :modes (haskell-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-newline-with-signature-expansion--outside-string-3ba
 :action
 (haskell-newline-with-signature-expansion)
 :contents
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  , _|_\"\"\"T.makeInstances [2..6]\"\"\" ==> []"
  "  , baz "
  "  ]")
 :expected-value
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  ,"
  "  _|_\"\"\"T.makeInstances [2..6]\"\"\" ==> []"
  "  , baz "
  "  ]")
 :modes (haskell-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-newline-with-signature-expansion--outside-string-2bb
 :action
 (haskell-newline-with-signature-expansion)
 :contents
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  , \"\"\"T.makeInstances [2..6]\"\"\"_|_ ==> []"
  "  , baz "
  "  ]")
 :expected-value
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  , \"\"\"T.makeInstances [2..6]\"\"\""
  "    _|_ ==> []"
  "  , baz "
  "  ]")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-newline-with-signature-expansion--outside-string-3bb
 :action
 (haskell-newline-with-signature-expansion)
 :contents
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  , _|_\"\"\"T.makeInstances [2..6]\"\"\" ==> []"
  "  , baz "
  "  ]")
 :expected-value
 (tests-utils--multiline
  "quux = "
  "  [ foobar"
  "  ,"
  "    _|_\"\"\"T.makeInstances [2..6]\"\"\" ==> []"
  "  , baz "
  "  ]")
 :modes (haskell-ts-mode))


(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--inside-quasiquote-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "quux ="
   "  [foo| "
   "    frobnicate _|_decombobulator"
   "  |]"
   "")
  (tests-utils--multiline
   ""
   "quux ="
   "  [foo| "
   "    frobnicate"
   "    _|_decombobulator"
   "  |]"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--inside-quasiquote-2
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "quux ="
   "  [foo| "
   "    \"frobnicate _|_decombobulator\""
   "  |]"
   "")
  (tests-utils--multiline
   ""
   "quux ="
   "  [foo| "
   "    \"frobnicate"
   "    _|_decombobulator\""
   "  |]"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--inside-quasiquote-3
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "quux ="
   "  [| "
   "    \"frobnicate _|_decombobulator\""
   "  |]"
   "")
  (tests-utils--multiline
   ""
   "quux ="
   "  [| "
   "    \"frobnicate\\"
   "    \\_|_decombobulator\""
   "  |]"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--inside-quasiquote-4
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "quux ="
   "  [e| "
   "    \"frobnicate _|_decombobulator\""
   "  |]"
   "")
  (tests-utils--multiline
   ""
   "quux ="
   "  [e| "
   "    \"frobnicate\\"
   "    \\_|_decombobulator\""
   "  |]"
   ""))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-newline-with-signature-expansion--inside-quasiquote-5
 :action
 (haskell-newline-with-signature-expansion)
 :contents
 (tests-utils--multiline
  ""
  "quux ="
  "  [e| "
  "    \"\"\"frobnicate _|_decombobulator\"\"\""
  "  |]"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "quux ="
  "  [e| "
  "    \"\"\"frobnicate"
  "    _|_decombobulator\"\"\""
  "  |]"
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--at-beginning-of-buffer-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   "_|_"
   "module Foo where"
   ""
   "makeFunction"
   "  :: MonadBase IO m => Env -> CPtrdiff -> CPtrDiff")
  (tests-utils--multiline
   ""
   "_|_"
   "module Foo where"
   ""
   "makeFunction"
   "  :: MonadBase IO m => Env -> CPtrdiff -> CPtrDiff"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--at-end-of-buffer-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   "makeFunction"
   "  :: MonadBase IO m _|_=> Env -> CPtrdiff -> CPtrDiff")
  (tests-utils--multiline
   "makeFunction"
   "  :: MonadBase IO m"
   "  _|_=> Env -> CPtrdiff -> CPtrDiff"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--does-not-insert-redundant-function-name-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   "makeFunction"
   "  :: MonadBase IO m"
   "  => Env"
   "  -> CPtrdiff -- ^ Minimum arity"
   "  -> CPtrDiff -- ^ Maximum arity"
   "  -> FunPtr (FunctionType a)_|_"
   "makeFunction")
  (tests-utils--multiline
   "makeFunction"
   "  :: MonadBase IO m"
   "  => Env"
   "  -> CPtrdiff -- ^ Minimum arity"
   "  -> CPtrDiff -- ^ Maximum arity"
   "  -> FunPtr (FunctionType a)"
   "_|_"
   "makeFunction"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion--does-not-insert-redundant-function-name-1a
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   "makeFunction"
   "  :: MonadBase IO m"
   "  => Env"
   "  -> CPtrdiff -- ^ Minimum arity"
   "  -> CPtrDiff -- ^ Maximum arity"
   "  -> FunPtr (FunctionType a)_|_"
   "makeFunction = _")
  (tests-utils--multiline
   "makeFunction"
   "  :: MonadBase IO m"
   "  => Env"
   "  -> CPtrdiff -- ^ Minimum arity"
   "  -> CPtrDiff -- ^ Maximum arity"
   "  -> FunPtr (FunctionType a)"
   "_|_"
   "makeFunction = _"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-indent-1
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "foo x = do_|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-indent-2
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "foo x = case x of_|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = case x of"
   "  _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-indent-3
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "foo x =_|_"
   "")
  (tests-utils--multiline
   ""
   "foo x ="
   "  _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-indent-4
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "foo x = \\y -> _|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = \\y ->"
   "  _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-indent-5
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "foo x = \\case_|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = \\case"
   "  _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-indent-6
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "foo x = do"
   "  let y = bar x_|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = do"
   "  let y = bar x"
   "      _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-newline-with-signature-expansion-indent-7
    (haskell-newline-with-signature-expansion)
  (tests-utils--multiline
   ""
   "foo x = bar y"
   "  where_|_"
   "")
  (tests-utils--multiline
   ""
   "foo x = bar y"
   "  where"
   "    _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-move-to-topmost-start-1
    (haskell-move-to-topmost-start)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . _|_(*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "_|_docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-move-to-topmost-start-2
    (haskell-move-to-topmost-start)
  (tests-utils--multiline
   ""
   "module Foo"
   "  ( foo"
   "  , bar_|_"
   "  )"
   "  where"
   "")
  (tests-utils--multiline
   ""
   "_|_module Foo"
   "  ( foo"
   "  , bar"
   "  )"
   "  where"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-move-to-topmost-start-3
    (haskell-move-to-topmost-start)
  (tests-utils--multiline
   ""
   "module Foo (foo, _|_bar) where"
   ""
   "foo :: Int -> Int"
   "foo x = x + 1"
   "")
  (tests-utils--multiline
   ""
   "_|_module Foo (foo, bar) where"
   ""
   "foo :: Int -> Int"
   "foo x = x + 1"
   ""))
(haskell-tests--test-buffer-contents
    haskell-tests/haskell-move-to-topmost-end-1
    (haskell-move-to-topmost-end)
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . _|_(*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world"
   ""
   "bar :: a -> x"
   "bar x = x"
   "")
  (tests-utils--multiline
   "foo :: a -> x"
   "foo x = x"
   ""
   "docExt"
   "  :: (ExactPrint.Annotate.Annotate ast)"
   "  => Located ast"
   "  -> ExactPrint.Types.Anns"
   "  -> Bool"
   "  -> ToBriDocM BriDocNumbered"
   "docExt x anns shouldAddComment = allocateNode $ BDFExternal"
   "    (ExactPrint.Types.mkAnnKey x)"
   "    (foldedAnnKeys x)"
   "    (let foo y = bar . baz"
   "           where"
   "             bar = (+1)"
   "             baz = (+y) . (*2)"
   "     in foo 100)"
   "    shouldAddComment"
   "    (Text.pack $ ExactPrint.exactPrint x anns)"
   ""
   "    foo"
   "    bar"
   "#wat"
   ""
   "    baz"
   "#are"
   "    you"
   "    doing"
   "  where"
   "    hello = world_|_"
   ""
   "bar :: a -> x"
   "bar x = x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-move-to-topmost-end-2
    (haskell-move-to-topmost-end)
  (tests-utils--multiline
   ""
   "_|_module Foo"
   "  ( foo"
   "  , bar"
   "  )"
   "  where"
   "")
  (tests-utils--multiline
   ""
   "module Foo"
   "  ( foo"
   "  , bar"
   "  )"
   "  where_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-move-to-topmost-end-3
    (haskell-move-to-topmost-end)
  (tests-utils--multiline
   ""
   "_|_module Foo (foo, bar) where"
   ""
   "foo :: Int -> Int"
   "foo x = x + 1"
   "")
  (tests-utils--multiline
   ""
   "module Foo (foo, bar) where_|_"
   ""
   "foo :: Int -> Int"
   "foo x = x + 1"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-move-to-topmost-end-4a
    (haskell-move-to-topmost-end)
  (tests-utils--multiline
   ""
   "_|_foo :: Int -> IO Int"
   "foo x ="
   "  let bar = Quux ( x)"
   "  in bar"
   "")
  (tests-utils--multiline
   ""
   "foo :: Int -> IO Int"
   "foo x ="
   "  let bar = Quux ( x)"
   "  in bar_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-move-to-topmost-end-4b
    (haskell-move-to-topmost-end)
  (tests-utils--multiline
   ""
   "_|_foo :: Int -> IO Int"
   "foo x ="
   "  let bar = Quux ("
   "             x)"
   "  in bar"
   "")
  (tests-utils--multiline
   ""
   "foo :: Int -> IO Int"
   "foo x ="
   "  let bar = Quux ("
   "             x)"
   "  in bar_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-move-to-topmost-end-5a
    (haskell-move-to-topmost-end)
  (tests-utils--multiline
   "_|_{-# INLINE alexGetByte #-}"
   "alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)"
   "alexGetByte input@AlexInput{aiPtr} ="
   "  case nextChar aiPtr of"
   "    (# c#, n, cs #) ->"
   "      case fixChar c# of"
   "        0##  -> Nothing -- Abort on an unknown character"
   "        -- '\\n'"
   "        10## -> Just (10, input')"
   "          where"
   "            !input' ="
   "              over aiLineL increaseLine $"
   "                set aiLineLengthL 0 $"
   "                  input { aiPtr = cs }"
   "        c    -> Just (b, input')"
   "          where"
   "            !b     = W8# (wordToWord8# c)"
   "            !input' ="
   "              over aiLineLengthL (+ I# n) $"
   "                input { aiPtr = cs }")
  (tests-utils--multiline
   "{-# INLINE alexGetByte #-}"
   "alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)"
   "alexGetByte input@AlexInput{aiPtr} ="
   "  case nextChar aiPtr of"
   "    (# c#, n, cs #) ->"
   "      case fixChar c# of"
   "        0##  -> Nothing -- Abort on an unknown character"
   "        -- '\\n'"
   "        10## -> Just (10, input')"
   "          where"
   "            !input' ="
   "              over aiLineL increaseLine $"
   "                set aiLineLengthL 0 $"
   "                  input { aiPtr = cs }"
   "        c    -> Just (b, input')"
   "          where"
   "            !b     = W8# (wordToWord8# c)"
   "            !input' ="
   "              over aiLineLengthL (+ I# n) $"
   "                input { aiPtr = cs }_|_"))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-move-to-topmost-end-5b
    (haskell-move-to-topmost-end)
  (tests-utils--multiline
   "_|_{-# INLINE alexGetByte #-}"
   "alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)"
   "alexGetByte input@AlexInput{aiPtr} ="
   "  case nextChar aiPtr of"
   "    (# c#, n, cs #) ->"
   "      case fixChar c# of"
   "        0##  -> Nothing -- Abort on an unknown character"
   "        -- '\\n'"
   "        10## -> Just (10, input')"
   "          where"
   "            !input' ="
   "              over aiLineL increaseLine $"
   "                set aiLineLengthL 0 $"
   "                  input { aiPtr = cs }"
   "        c    -> Just (b, input')"
   "          where"
   "            !b     = W8# (wordToWord8# c)"
   "            !input' ="
   "              over aiLineLengthL (+ I# n) $"
   "                input { aiPtr = cs }"
   "")
  (tests-utils--multiline
   "{-# INLINE alexGetByte #-}"
   "alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)"
   "alexGetByte input@AlexInput{aiPtr} ="
   "  case nextChar aiPtr of"
   "    (# c#, n, cs #) ->"
   "      case fixChar c# of"
   "        0##  -> Nothing -- Abort on an unknown character"
   "        -- '\\n'"
   "        10## -> Just (10, input')"
   "          where"
   "            !input' ="
   "              over aiLineL increaseLine $"
   "                set aiLineLengthL 0 $"
   "                  input { aiPtr = cs }"
   "        c    -> Just (b, input')"
   "          where"
   "            !b     = W8# (wordToWord8# c)"
   "            !input' ="
   "              over aiLineLengthL (+ I# n) $"
   "                input { aiPtr = cs }_|_"
   ""))

(ert-deftest haskell-tests/haskell-regexen/pre-post-qualified-import-line-1 ()
  (dolist (str '("import qualified Data.Ord  "
                 "import Data.Ord  qualified   "
                 "import A.B.C"
                 "import qualified A.B.C"
                 "import qualified AAA.Bc.Cx as Something"
                 "import Żółć"
                 "\t import\t qualified \t\tM\tas G"
                 "import Module_1.S_3_3_"
                 "import q.Module...qwerqwe..."
                 "import \"package-1.2.3\" B"
                 "import safe B"
                 "import safe qualified \"unicode-7.0\" Data.Char.Unicode_v_7 as U (func)"))
    (save-match-data
      (should (string-match haskell-regexen/pre-post-qualified-import-line str))
      (let ((end (match-end 0)))
        (should (equal end (length str)))))))

(ert-deftest haskell-tests/haskell-regexen/core-1 ()
  (with-syntax-table ghc-core-symbol--identifier-syntax-table
    (dolist (str '("$test_prim#"
                   "$w$sexecMatch"))
      (save-match-data
        (should (posix-string-match (concat "^\\(?:" haskell-regexen/core/opt-q/varid-or-conid-or-operator-or-number/posix-only "\\)$")
                                    str
                                    nil
                                    t))
        (should (posix-string-match haskell-regexen/core/opt-q/varid-or-conid-or-operator-or-number/posix-only str))
        (let ((beginning (match-beginning 0))
              (end (match-end 0)))
          (should (equal beginning 0))
          (should (equal end (length str))))))))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-1
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should-not (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "import Data.List"
  "import Data.Ord_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "import Data.List"
  "import qualified Data.Ord_|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-1a
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord_|_ qualified"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-2
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should-not (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "import Data.List"
  "import      Data.Ord_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "import Data.List"
  "import qualified Data.Ord_|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-2a
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import      Data.Ord_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import      Data.Ord_|_ qualified"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-3
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should-not (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "import Data.List"
  "import \"foo\"     Data.Ord_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "import Data.List"
  "import qualified \"foo\"     Data.Ord_|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-3a
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import \"foo\"     Data.Ord_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import \"foo\"     Data.Ord_|_ qualified"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-4
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should-not (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "import Data.List"
  "import   qualified   Data.Ord_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "import Data.List"
  "import   Data.Ord_|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-4a
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import   qualified   Data.Ord_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import   Data.Ord_|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-4b
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord qualified_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord_|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-4c
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord      qualified   _|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord   _|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-4d
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import)
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord      qualified   _|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord qualified   _|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-5
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should-not (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "import Data.List"
  "import  qualified   \"foo\"  Data.Ord_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "import Data.List"
  "import  \"foo\"  Data.Ord_|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-5a
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import  qualified   \"foo\"    Data.Ord_|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import  \"foo\"    Data.Ord_|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-5b
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import  \"foo\"  Data.Ord   qualified   _|_"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import  \"foo\"  Data.Ord   _|_"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-6
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should-not (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "import Data.List"
  "import qualified Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "import Data.List"
  "import Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-6a
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should-not (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import)
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "import Data.List"
  "import    qualified    Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "import Data.List"
  "import qualified Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-6b
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import qualified Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-6c
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import)
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import qualified Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord qualified as Ord (Down_|_)"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-6d
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import)
   (haskell-qualify-import)
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import qualified Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-6e
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import)
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord   qualified   as Ord (Down_|_)"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE ImportQualifiedPost #-}"
  "import Data.List"
  "import Data.Ord qualified as Ord (Down_|_)"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-7
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should-not (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "import Data.List"
  "import {-# SOURCE #-} qualified Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "import Data.List"
  "import {-# SOURCE #-} Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-qualify-import-7a
 :action
 (progn
   (haskell-ext-tracking-mode +1)
   (should-not (haskell-ext-tracking-have-import-qualified-post?))
   (haskell-qualify-import))
 :contents
 (tests-utils--multiline
  "import Data.List"
  "import {-# SOURCE #-} Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :expected-value
 (tests-utils--multiline
  "import Data.List"
  "import {-# SOURCE #-} qualified Data.Ord as Ord (Down_|_)"
  "import Data.Set (Set)")
 :fresh-buffer t)

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-back-up-indent-level-1
    (should (haskell-misc--back-up-indent-level))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  let bar x = do"
   "        baz x"
   "        quux _|_x"
   "  bar 10 ")
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  let bar x = do"
   "        baz x"
   "        _|_quux x"
   "  bar 10 "))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-back-up-indent-level-2
    (should (haskell-misc--back-up-indent-level))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  let bar x = do"
   "        baz x"
   "        _|_quux x"
   "  bar 10 ")
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  _|_let bar x = do"
   "        baz x"
   "        quux x"
   "  bar 10 "))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-back-up-indent-level-3
    (should (haskell-misc--back-up-indent-level))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  _|_let bar x = do"
   "        baz x"
   "        quux x"
   "  bar 10 ")
  (tests-utils--multiline
   "foo :: Int -> Int"
   "_|_foo = do"
   "  let bar x = do"
   "        baz x"
   "        quux x"
   "  bar 10 "))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-back-up-indent-level-4
    (should-not (haskell-misc--back-up-indent-level))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "_|_foo = do"
   "  let bar x = do"
   "        baz x"
   "        quux x"
   "  bar 10 ")
  (tests-utils--multiline
   "foo :: Int -> Int"
   "_|_foo = do"
   "  let bar x = do"
   "        baz x"
   "        quux x"
   "  bar 10 "))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-back-up-indent-level-5
    (should (haskell-misc--back-up-indent-level))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  let bar x = do"
   "        baz x"
   "  _|_      quux x"
   "  bar 10 ")
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  _|_let bar x = do"
   "        baz x"
   "        quux x"
   "  bar 10 "))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-back-up-indent-level-6
    (should (haskell-misc--back-up-indent-level))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  let bar x = do"
   "        baz x"
   "    _|_    quux x"
   "  bar 10 ")
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  _|_let bar x = do"
   "        baz x"
   "        quux x"
   "  bar 10 "))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-back-up-indent-level-7
    (should (haskell-misc--back-up-indent-level))
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  let bar x = do"
   "        baz x"
   "    _|_    quux x"
   "  bar 10 ")
  (tests-utils--multiline
   "foo :: Int -> Int"
   "foo = do"
   "  _|_let bar x = do"
   "        baz x"
   "        quux x"
   "  bar 10 "))

(haskell-tests--make-multiple-output-test-buffer-contents
    (tests-utils--multiline
     "_|_foo :: Int -> Int"
     "foo = do -- a comment"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 ")
  ((haskell-tests/haskell-forward-sexp-1a
    (haskell-forward-sexp)
    (tests-utils--multiline
     "foo_|_ :: Int -> Int"
     "foo = do -- a comment"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-1b
    (haskell-forward-sexp 1)
    (tests-utils--multiline
     "foo_|_ :: Int -> Int"
     "foo = do -- a comment"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-2
    (haskell-forward-sexp 2)
    (tests-utils--multiline
     "foo ::_|_ Int -> Int"
     "foo = do -- a comment"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-3
    (haskell-forward-sexp 3)
    (tests-utils--multiline
     "foo :: Int_|_ -> Int"
     "foo = do -- a comment"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-4
    (haskell-forward-sexp 4)
    (tests-utils--multiline
     "foo :: Int ->_|_ Int"
     "foo = do -- a comment"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-5
    (haskell-forward-sexp 5)
    (tests-utils--multiline
     "foo :: Int -> Int_|_"
     "foo = do -- a comment"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-6
    (haskell-forward-sexp 6)
    (tests-utils--multiline
     "foo :: Int -> Int"
     "foo_|_ = do -- a comment"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-7
    (haskell-forward-sexp 7)
    (tests-utils--multiline
     "foo :: Int -> Int"
     "foo =_|_ do -- a comment"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-8
    (haskell-forward-sexp 8)
    (tests-utils--multiline
     "foo :: Int -> Int"
     "foo = do_|_ -- a comment"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-9a
    (let ((parse-sexp-ignore-comments t))
      (haskell-forward-sexp 9))
    (tests-utils--multiline
     "foo :: Int -> Int"
     "foo = do -- a comment"
     "  let_|_ bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-9b
    (let ((parse-sexp-ignore-comments nil))
      (haskell-forward-sexp 9))
    (tests-utils--multiline
     "foo :: Int -> Int"
     "foo = do -- a comment_|_"
     "  let bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-10a
    (let ((parse-sexp-ignore-comments t))
      (haskell-forward-sexp 10))
    (tests-utils--multiline
     "foo :: Int -> Int"
     "foo = do -- a comment"
     "  let bar_|_ x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))
   (haskell-tests/haskell-forward-sexp-10b
    (let ((parse-sexp-ignore-comments nil))
      (haskell-forward-sexp 10))
    (tests-utils--multiline
     "foo :: Int -> Int"
     "foo = do -- a comment"
     "  let_|_ bar x = do"
     "        baz x"
     "        quux x"
     "  bar 10 "))))

(haskell-tests--make-multiple-output-test-buffer-contents
    " (foo + _|_((*) bar $ f baz) - g [quux, fizz, frob] `min` xxx)"
  ((haskell-tests/haskell-forward-nested-sexp-1
    (haskell-forward-sexp)
    " (foo + ((*) bar $ f baz)_|_ - g [quux, fizz, frob] `min` xxx)")
   (haskell-tests/haskell-forward-nested-sexp-2
    (haskell-forward-sexp 2)
    " (foo + ((*) bar $ f baz) -_|_ g [quux, fizz, frob] `min` xxx)")
   (haskell-tests/haskell-forward-nested-sexp-3
    (haskell-forward-sexp 3)
    " (foo + ((*) bar $ f baz) - g_|_ [quux, fizz, frob] `min` xxx)")
   (haskell-tests/haskell-forward-nested-sexp-4
    (haskell-forward-sexp 4)
    " (foo + ((*) bar $ f baz) - g [quux, fizz, frob]_|_ `min` xxx)")
   ;; This one is not quite what I’d like it to be but ok, let’s fix the behaviour.
   (haskell-tests/haskell-forward-nested-sexp-5
    (haskell-forward-sexp 5)
    " (foo + ((*) bar $ f baz) - g [quux, fizz, frob] `_|_min` xxx)")
   ;; This one is not quite what I’d like it to be but ok, let’s fix the behaviour.
   (haskell-tests/haskell-forward-nested-sexp-6
    (haskell-forward-sexp 6)
    " (foo + ((*) bar $ f baz) - g [quux, fizz, frob] `min_|_` xxx)")))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-1
  :action (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value nil
  :contents
  (tests-utils--multiline
   "_|_"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-2
  :action (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value nil
  :contents
  (tests-utils--multiline
   "module Foo where"
   "_|_"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-3
  :action (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value '("RankNTypes")
  :contents
  (tests-utils--multiline
   "{-#language RankNTypes#-}"
   "module Foo where"
   "_|_"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-4
  :action (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value '("RankNTypes")
  :contents
  (tests-utils--multiline
   "{-#lAnGuAgE RankNTypes#-}"
   "module Foo where"
   "_|_"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-5
  :action (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value '("RankNTypes")
  :contents
  (tests-utils--multiline
   "{-#      lAnGuAgE   "
   "RankNTypes"
   "#-}"
   "module Foo where"
   "_|_"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-6
  :action (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value '("RankNTypes" "UndecidableInstances")
  :contents
  (tests-utils--multiline
   "{-#      lAnGuAgE   "
   "RankNTypes, UndecidableInstances"
   "#-}"
   "module Foo where"
   "_|_"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-7
  :action (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value '("RankNTypes" "UndecidableInstances")
  :contents
  (tests-utils--multiline
   "{-#      lAnGuAgE   "
   "\tRankNTypes \t "
   " , \t\t  "
   "  UndecidableInstances\t"
   "#-}"
   "module Foo where"
   "_|_"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-8
  :action (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value '("LambdaCase" "RankNTypes" "UndecidableInstances")
  :contents
  (tests-utils--multiline
   ""
   "{-#      lAnGuAgE   "
   "\tRankNTypes \t "
   " , \t\t  "
   "  Undecidabl_|_eInstances\t"
   "#-}"
   "{-# LANGUAGE LambdaCase #-}"
   "module Foo where"
   ""))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-9
  :action
  (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value
  '("FlexibleContexts" "AllowAmbiguousTypes" "AlternativeLayoutRule" "Safe")
  :contents
  (tests-utils--multiline
   "_|_"
   "{-# LANGUAGE Safe #-}"
   "{-#LANGUAGE AlternativeLayoutRule #-}"
   "{-# LANGUAGE AllowAmbiguousTypes#-}"
   "{-#LANGUAGE FlexibleContexts#-}"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-10
  :action
  (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value
  '("Safe" "AlternativeLayoutRule" "AllowAmbiguousTypes" "FlexibleContexts")
  :contents
  (tests-utils--multiline
   "_|_"
   "{-# LANGUAGE Safe,AlternativeLayoutRule, AllowAmbiguousTypes,"
   "FlexibleContexts #-}"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-11
  :action
  (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value
  '("FlexibleContexts" "AllowAmbiguousTypes" "AlternativeLayoutRule" "Safe")
  :contents
  (tests-utils--multiline
   "_|_"
   "{-# language Safe #-}"
   "{-#language AlternativeLayoutRule #-}"
   "{-# language AllowAmbiguousTypes#-}"
   "{-#language FlexibleContexts#-}"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-12
  :action
  (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value
  '("FlexibleContexts" "AllowAmbiguousTypes" "FlexibleInstances" "AlternativeLayoutRule" "Safe")
  :contents
  (tests-utils--multiline
   "_|_"
   "{-# LANGUAGE Safe "
   ""
   "#-}"
   "{-#"
   "LANGUAGE AlternativeLayoutRule #-}"
   "{-# LANGUAGE AllowAmbiguousTypes, "
   "FlexibleInstances #-}"
   "{-#LANGUAGE"
   " FlexibleContexts#-}"))

(haskell-tests--test-result
    haskell-tests/haskell-format--get-language-extensions-13
  :action
  (haskell-format--get-language-extensions (current-buffer) t)
  :expected-value
  '("AllowAmbiguousTypes" "RecordWildCards" "FlexibleInstances" "FlexibleContexts")
  :contents
  (tests-utils--multiline
   "----------------------------------------------------------------------------"
   "-- |"
   "-- Module      :  Test"
   "--"
   "--"
   "----------------------------------------------------------------------------"
   ""
   "{-# LANGUAGE FlexibleContexts    #-}"
   "{-# LANGUAGE FlexibleInstances   #-}"
   "{-# LANGUAGE RecordWildCards     #-}"
   "{-# LANGUAGE AllowAmbiguousTypes #-}"
   ""
   ""
   "module Test where"
   ""
   "data Frob = Frob"
   "  { frob1 :: Int"
   "  , frob2 :: Double"
   "  }"
   "_|_"
   "foo Frob{..} ="
   " frob1 * 2"
   ""))


(haskell-tests--test-buffer-contents
    haskell-tests/dante--insert-type--1
    (dante--insert-type "foo :: Int -> Int -> Int")
  (tests-utils--multiline
   ""
   "_|_foo x y = x + y"
   "")
  (tests-utils--multiline
   ""
   "foo :: Int -> Int -> Int"
   "_|_foo x y = x + y"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/dante--insert-type--2
    (dante--insert-type "foo :: Int -> Int -> Int")
  (tests-utils--multiline
   ""
   "bar z = foo z z"
   "  where"
   "    _|_foo x y = x + y"
   "")
  (tests-utils--multiline
   ""
   "bar z = foo z z"
   "  where"
   "    foo :: Int -> Int -> Int"
   "    _|_foo x y = x + y"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/dante--insert-type--3
    (dante--insert-type "foo :: Int -> Int -> Int")
  (tests-utils--multiline
   ""
   "bar z = do"
   "  let _|_foo x y = x + y"
   "  pure $ foo z z"
   "")
  (tests-utils--multiline
   ""
   "bar z = do"
   "  let foo :: Int -> Int -> Int"
   "      _|_foo x y = x + y"
   "  pure $ foo z z"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/dante--insert-type--4
    (dante--insert-type "foo :: Int -> Int -> Int")
  (tests-utils--multiline
   ""
   "bar z = do"
   "  let"
   "    _|_foo x y = x + y"
   "  pure $ foo z z"
   "")
  (tests-utils--multiline
   ""
   "bar z = do"
   "  let"
   "    foo :: Int -> Int -> Int"
   "    _|_foo x y = x + y"
   "  pure $ foo z z"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/dante--insert-type--5
    (dante--insert-type "foo :: Int -> Int -> Int")
  (tests-utils--multiline
   ""
   "bar z = do let _|_foo x y = x + y"
   "           pure $ foo z z"
   "")
  (tests-utils--multiline
   ""
   "bar z = do let foo :: Int -> Int -> Int"
   "               _|_foo x y = x + y"
   "           pure $ foo z z"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/dante--insert-type--6
    (dante--insert-type "foo :: Int -> Int -> Int")
  (tests-utils--multiline
   ""
   "bar z = do"
   "  let fo_|_o x y = x + y"
   "  pure $ foo z z"
   "")
  (tests-utils--multiline
   ""
   "bar z = do"
   "  let foo :: Int -> Int -> Int"
   "      fo_|_o x y = x + y"
   "  pure $ foo z z"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/dante--insert-type--7
    (dante--insert-type "foo :: Int -> Int -> Int")
  (tests-utils--multiline
   ""
   "bar z = do"
   "  let !fo_|_o x y = x + y"
   "  pure $ foo z z"
   "")
  (tests-utils--multiline
   ""
   "bar z = do"
   "  let foo :: Int -> Int -> Int"
   "      !fo_|_o x y = x + y"
   "  pure $ foo z z"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/dante--insert-type--8
    (dante--insert-type "foo :: Int -> Int -> Int")
  (tests-utils--multiline
   ""
   "bar z = do"
   "  let !_|_foo x y = x + y"
   "  pure $ foo z z"
   "")
  (tests-utils--multiline
   ""
   "bar z = do"
   "  let foo :: Int -> Int -> Int"
   "      !_|_foo x y = x + y"
   "  pure $ foo z z"
   ""))


(ert-deftest haskell-tests/haskell-misc-cabal-align-and-sort-subsection-1 ()
  (haskell-tests--cabal--test-buffer-contents
      (haskell-misc-cabal-align-and-sort-subsection)
    (tests-utils--multiline
     ""
     "  build-depends:_|_"
     "    , base ^>= 4.14"
     "    , async"
     "")
    (tests-utils--multiline
     ""
     "  build-depends:_|_"
     "    , async"
     "    , base ^>= 4.14"
     "")))

(ert-deftest haskell-tests/haskell-misc-cabal-align-and-sort-subsection-2 ()
  (haskell-tests--cabal--test-buffer-contents
      (haskell-misc-cabal-align-and-sort-subsection)
    (tests-utils--multiline
     ""
     "_|_  build-depends:"
     "    , base ^>= 4.14"
     "    , async"
     "")
    (tests-utils--multiline
     ""
     "_|_  build-depends:"
     "    , async"
     "    , base ^>= 4.14"
     "")))

(ert-deftest haskell-tests/haskell-misc-cabal-align-and-sort-subsection-3 ()
  (haskell-tests--cabal--test-buffer-contents
      (haskell-misc-cabal-align-and-sort-subsection)
    (tests-utils--multiline
     ""
     "  build-depends:"
     "_|_    , base ^>= 4.14"
     "    , async"
     "")
    (tests-utils--multiline
     ""
     "  build-depends:_|_"
     "    , async"
     "    , base ^>= 4.14"
     "")))

(ert-deftest haskell-tests/haskell-misc-cabal-align-and-sort-subsection-4 ()
  (haskell-tests--cabal--test-buffer-contents
      (haskell-misc-cabal-align-and-sort-subsection)
    (tests-utils--multiline
     ""
     "  build-depends:"
     "      base ^>= 4.14_|_"
     "    , async"
     "")
    (tests-utils--multiline
     ""
     "  build-depends:"
     "      async_|_"
     "    , base ^>= 4.14"
     "")))

(ert-deftest haskell-tests/haskell-misc-cabal-align-and-sort-subsection-5 ()
  (haskell-tests--cabal--test-buffer-contents
      (haskell-misc-cabal-align-and-sort-subsection)
    (tests-utils--multiline
     ""
     "  build-depends:"
     "    base ^>= 4.14,_|_"
     "    async"
     "")
    (tests-utils--multiline
     ""
     "  build-depends:"
     "    async,_|_"
     "    base ^>= 4.14"
     "")))


(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--open-paren-1
    (haskell-smart-operators-open-paren)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5_|_)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 (_|_))"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--open-paren-2
    (haskell-smart-operators-open-paren)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 @_|_)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 @(_|_))"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--open-paren-3
    (haskell-smart-operators-open-paren)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"_|_\")"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"(_|_)\")"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--open-paren-4
    (haskell-smart-operators-open-paren)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"foo_|_bar\")"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"foo(_|_)bar\")"
   ""))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-smart-operators--open-paren-5
 :action
 (haskell-smart-operators-open-paren)
 :contents
 (tests-utils--multiline
  ""
  "import Foo (Bar_|_)"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "import Foo (Bar(_|_))"
  "")
 :modes (haskell-ts-mode)
 :fresh-buffer t)

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--open-bracket-1
    (haskell-smart-operators-open-bracket)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5_|_)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 [_|_])"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--open-bracket-2
    (haskell-smart-operators-open-bracket)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 @_|_)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 @[_|_])"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--open-bracket-3
    (haskell-smart-operators-open-bracket)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"_|_\")"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"[_|_]\")"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--open-bracket-4
    (haskell-smart-operators-open-bracket)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"foo_|_bar\")"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"foo[_|_]bar\")"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--open-brace-1
    (haskell-smart-operators-open-brace)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"_|_\")"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"{_|_}\")"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-smart-operators--open-brace-2
    (haskell-smart-operators-open-brace)
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"foo_|_bar\")"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = foo (Median3or5 \"foo{_|_}bar\")"
   ""))

(defconst haskell-tests/cabal-test-data
  (concat +emacs-config-path+ "/tests/test-data/cabal"))

(ert-deftest haskell-tests/haskell-misc--configure-dante--find-cabal-component-for-file-1 ()
  (let* ((cabal-file (expand-file-name (concat haskell-tests/cabal-test-data "/test-components.cabal")))
         (config (flycheck-haskell-read-cabal-configuration cabal-file nil))
         (components (-map #'parse-cabal-component (cdr (assq 'components config))))
         (root "/home/user/proj1")
         (results nil))
    (should-not (null components))
    (dolist (entry '(("src/Foo.hs" "lib:test-components")
                     ("src/Bar.hs" "lib:test-components")
                     ("src/Baz/Quux.hs" "lib:test-components")
                     ("src/Frob/Decombobulate.hs" "lib:test-components")
                     ("exe/Main1.hs" "exe:test-components-exe1")
                     ("exe/Main2.hs" "exe:test-components-exe2")
                     ("exe/Main3.hs" "exe:test-components-exe3")
                     ("test/TestMain1.hs" "test:test-components-test1")
                     ("test/TestMain2.hs" "test:test-components-test2")
                     ("test/TestMain3.hs" "test:test-components-test3")
                     ("bench/TestComponentsBench1.hs" "bench:test-components-bench1")
                     ("bench/TestComponentsBench2.hs" "bench:test-components-bench2")
                     ("bench/TestComponentsBench3.hs" "bench:test-components-bench3")))
      (let ((file (car entry))
            (expected-component (cadr entry)))
        (dolist (path (list file (concat root "/" file)))
          (should (equal
                   (haskell-misc--configure-dante--find-cabal-component-for-file components path)
                   (cons expected-component nil))))))))

(ert-deftest haskell-tests/haskell-misc--configure-dante--find-cabal-component-for-file-2 ()
  (should (equal (haskell-misc--configure-dante--find-cabal-component-for-file
                  (-map #'parse-cabal-component
                        '(("exe" "tg" "exe/TG.hs" nil ("exe"))))
                  "/home/sergey/projects/haskell/projects/tg/exe/TG.hs")
                 (cons nil
                       '("Component ‘exe:tg’ specifies main file with slash (exe/TG.hs) but doesn’t put ‘.’ in source dirs: ‘exe’. Possible fix: remove slash or put ‘.’ into source dirs.")))))

(ert-deftest haskell-tests/haskell-misc--configure-dante--find-cabal-component-for-file-3 ()
  (should (equal (haskell-misc--configure-dante--find-cabal-component-for-file
                  (-map #'parse-cabal-component
                        '(("exe" "tg" "exe/TG.hs" nil ("exe" "."))))
                  "/home/sergey/projects/haskell/projects/tg/exe/TG.hs")
                 (cons "exe:tg"
                       nil))))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-misc--ensure-language-pragma-1
    (haskell-misc--ensure-language-pragma "OverloadedStrings")
  (tests-utils--multiline
   ""
   "foo x xs = foo _|_"
   "")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "foo x xs = foo _|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-misc--ensure-language-pragma-2
    (haskell-misc--ensure-language-pragma "OverloadedStrings")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "foo x xs = foo _|_"
   "")
  (tests-utils--multiline
   "{-# LANGUAGE OverloadedStrings #-}"
   ""
   "foo x xs = foo _|_"
   ""))

;; (ert "haskell-tests/.*")

;; (setf haskell-tests/tests
;;       '(haskell-tests/abbrev+-extract-module-name))

(ert-deftest haskell-tests/haskell-dante--strip-instances-from-ghci-info-1 ()
  (let ((str
         (tests-utils--multiline
          "Foo in `Foo.hs'"
          ""
          "type Foo :: * -> *"
          "data Foo a = Foo | Bar a a"
          "  	-- Defined in ‘Data.Foo’")))
    (should (equal (haskell-dante--strip-instances-from-ghci-info str)
                   str))))

(ert-deftest haskell-tests/haskell-dante--strip-instances-from-ghci-info-2 ()
  (let ((str
         (tests-utils--multiline
          "Foo in `Foo.hs'"
          ""
          "type Foo :: * -> *"
          "data Foo a = Foo | Bar a a"
          "  	-- Defined in ‘Data.Foo’"
          "instance Pretty a => Pretty (Foo a)"
          "  -- Defined at packages/pretty-instances/src/Prettyprinter/Instances.hs")))
    (should (equal (haskell-dante--strip-instances-from-ghci-info str)
                   (tests-utils--multiline
                    "Foo in `Foo.hs'"
                    ""
                    "type Foo :: * -> *"
                    "data Foo a = Foo | Bar a a"
                    "  	-- Defined in ‘Data.Foo’")))))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-1-paren
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = (bar _|_ baz)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-2-paren
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = (bar `quux` _|_baz)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-3-paren
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = ((bar) `quux` _|_baz)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-4-paren
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = (bar \"quux\" _|_baz)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-5-paren
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = ((bar) \"quux\" _|_baz)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-6-paren
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = (bar [|quux|] _|_baz)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-7-paren
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = ((bar) [|quux|] _|_baz)"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-1-bracket
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = [bar _|_ baz]"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-2-bracket
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = [bar `quux` _|_baz]"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-3-bracket
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = [[bar] `quux` _|_baz]"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-4-bracket
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = [bar \"quux\" _|_baz]"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-5-bracket
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = [[bar] \"quux\" _|_baz]"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-6-bracket
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = [bar [|quux|] _|_baz]"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-7-bracket
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = [[bar] [|quux|] _|_baz]"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-1-brace
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = {bar _|_ baz}"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-2-brace
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = {bar `quux` _|_baz}"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-3-brace
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = {{bar} `quux` _|_baz}"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-4-brace
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = {bar \"quux\" _|_baz}"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-5-brace
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = {{bar} \"quux\" _|_baz}"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-6-brace
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = {bar [|quux|] _|_baz}"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:splice-sexp-killing-backward-7-brace
    (vim:splice-sexp-killing-backward:wrapper)
  (tests-utils--multiline
   ""
   "foo x xs = {{bar} [|quux|] _|_baz}"
   "")
  (tests-utils--multiline
   ""
   "foo x xs = _|_baz"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:raise-sexp-1
    (vim:raise-sexp:wrapper)
  (tests-utils--multiline
   ""
   "foo = whnf (_|_M.withoutKeys m) m_odd_keys"
   "")
  (tests-utils--multiline
   ""
   "foo = whnf _|_M.withoutKeys m_odd_keys"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:raise-sexp-2
    (vim:raise-sexp:wrapper)
  (tests-utils--multiline
   ""
   "foo = whnf (M.witho_|_utKeys m) m_odd_keys"
   "")
  (tests-utils--multiline
   ""
   "foo = whnf _|_M.withoutKeys m_odd_keys"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:raise-sexp-3
    (vim:raise-sexp:wrapper)
  (tests-utils--multiline
   ""
   "foo = whnf (M._|_withoutKeys m) m_odd_keys"
   "")
  (tests-utils--multiline
   ""
   "foo = whnf _|_M.withoutKeys m_odd_keys"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/vim:raise-sexp-4
    (vim:raise-sexp:wrapper)
  (tests-utils--multiline
   ""
   "foo = whnf (M_|_.withoutKeys m) m_odd_keys"
   "")
  (tests-utils--multiline
   ""
   "foo = whnf _|_M.withoutKeys m_odd_keys"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-1
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "import Foo_|_"
   "")
  (tests-utils--multiline
   ""
   "import Foo_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-2
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "import Foo_|_"
   "import Bar"
   "")
  (tests-utils--multiline
   ""
   "import Bar"
   "import Foo_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-3
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "import Bar"
   "import Foo_|_"
   "import Bar"
   "")
  (tests-utils--multiline
   ""
   "import Bar"
   "import Foo_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-4
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "import Bar (foo)"
   "import Foo_|_"
   "import Bar"
   "")
  (tests-utils--multiline
   ""
   "import Bar"
   "import Foo_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-5
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "import Bar (foo)"
   "import Foo_|_"
   "import Bar (bar)"
   "")
  (tests-utils--multiline
   ""
   "import Bar (bar, foo)"
   "import Foo_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-5a
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "import Bar (foo)"
   "import Foo_|_"
   "import Bar ()"
   "")
  (tests-utils--multiline
   ""
   "import Bar (foo)"
   "import Foo_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-5b
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "import Bar ()"
   "import Foo_|_"
   "import Bar (bar)"
   "")
  (tests-utils--multiline
   ""
   "import Bar (bar)"
   "import Foo_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-6
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "import Bar "
   "  ( foo"
   "  , bar_|_"
   "  )"
   "import Foo"
   "import Bar (baz)"
   "")
  (tests-utils--multiline
   ""
   "_|_import Bar "
   "  ( baz"
   "  , foo"
   "  , bar"
   "  )"
   "import Foo"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-7
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "_|_import Data.Text (Text)"
   "import Data.Text qualified as T"
   "import Control.Monad"
   "import Data.Bits"
   "import Data.Bytestring qualified as BS"
   "import Data.Bytestring.Internal qualified as BS"
   "import Data.Bytestring.Unsafe qualified as BS"
   "import Data.Bytestring.Unsafe qualified as BSU"
   "import Data.Foldable"
   "import Data.List.Ext qualified as L"
   "import Data.Map.Strict (Map)"
   "import Data.Map.Strict qualified as M"
   "import Data.Ratio"
   "import Data.Text (Text)"
   "import Data.Text.Encoding qualified as T"
   "import Data.Word"
   "import Foreign (peek)"
   "import Foreign.Ptr (castPtr)"
   "import Utils.Pretty"
   "import Foreign.Ptr"
   "")
  (tests-utils--multiline
   ""
   "import Control.Monad"
   "import Data.Bits"
   "import Data.Bytestring qualified as BS"
   "import Data.Bytestring.Internal qualified as BS"
   "import Data.Bytestring.Unsafe qualified as BSU"
   "import Data.Bytestring.Unsafe qualified as BS"
   "import Data.Foldable"
   "import Data.List.Ext qualified as L"
   "import Data.Map.Strict (Map)"
   "import Data.Map.Strict qualified as M"
   "import Data.Ratio"
   "_|_import Data.Text (Text)"
   "import Data.Text qualified as T"
   "import Data.Text.Encoding qualified as T"
   "import Data.Word"
   "import Foreign (peek)"
   "import Foreign.Ptr"
   "import Utils.Pretty"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-8a
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "_|_import GHC.Generics (Generic)"
   "import Algebra.Lattice ((/\))"
   "import Data.ByteString qualified as BS"
   "import Sasha.TTH"
   "")
  (tests-utils--multiline
   ""
   "import Algebra.Lattice ((/\))"
   "import Data.ByteString qualified as BS"
   "_|_import GHC.Generics (Generic)"
   "import Sasha.TTH"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-8b
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "import GHC.Generics (Generic)"
   "import Algebra.Lattice ((/\))"
   "_|_import Data.ByteString qualified as BS"
   "import Sasha.TTH"
   "")
  (tests-utils--multiline
   ""
   "import Algebra.Lattice ((/\))"
   "_|_import Data.ByteString qualified as BS"
   "import GHC.Generics (Generic)"
   "import Sasha.TTH"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/sort-imports-8c
    (haskell-sort-imports)
  (tests-utils--multiline
   ""
   "import GHC.Generics (Generic)"
   "_|_import Algebra.Lattice ((/\))"
   "import Data.ByteString qualified as BS"
   "import Sasha.TTH"
   "")
  (tests-utils--multiline
   ""
   "_|_import Algebra.Lattice ((/\))"
   "import Data.ByteString qualified as BS"
   "import GHC.Generics (Generic)"
   "import Sasha.TTH"
   ""))

(ert-deftest haskell-tests/haskell-sort-imports--group-imports-1 ()
  (should (equal (haskell-sort-imports--group-imports
                  '("import Foo"
                    "import Bar"
                    "import Bar"))
                 '("import Bar"
                   "import Foo"))))

(ert-deftest haskell-tests/haskell-sort-imports--group-imports-2 ()
  (should (equal (haskell-sort-imports--group-imports
                  '("import Foo"
                    "import Bar (quux)"
                    "import Bar"))
                 '("import Bar"
                   "import Foo"))))

(ert-deftest haskell-tests/haskell-sort-imports--group-imports-3 ()
  (should (equal (haskell-sort-imports--group-imports
                  '("import Foo"
                    "import Bar hiding (foo)"
                    "import Bar hiding (bar)"))
                 '("import Bar hiding (bar, foo)"
                   "import Foo"))))

(ert-deftest haskell-tests/haskell-sort-imports--group-imports-4 ()
  (should (equal (haskell-sort-imports--group-imports
                  '("import Foo"
                    "import Bar hiding (foo)"
                    "import Bar"))
                 '("import Bar"
                   "import Foo"))))

(ert-deftest haskell-tests/haskell-sort-imports--group-imports-4a ()
  (should (equal (haskell-sort-imports--group-imports
                  '("import Foo"
                    "import Bar hiding (foo, baz)"
                    "import Bar"))
                 '("import Bar"
                   "import Foo"))))

(ert-deftest haskell-tests/haskell-sort-imports--group-imports-5 ()
  (should (equal (haskell-sort-imports--group-imports
                  '("import Foo"
                    "import Bar hiding (foo)"
                    "import Bar (bar)"))
                 '("import Bar hiding (foo)"
                   "import Foo"))))

(ert-deftest haskell-tests/haskell-sort-imports--group-imports-6 ()
  (should (equal (haskell-sort-imports--group-imports
                  '("import Foo"
                    "import Bar hiding (foo, bar)"
                    "import Bar (bar)"))
                 '("import Bar hiding (foo)"
                   "import Foo"))))

(ert-deftest haskell-tests/haskell-sort-imports--group-imports-6a ()
  (should (equal (haskell-sort-imports--group-imports
                  '("import Foo"
                    "import Bar hiding (bar, foo)"
                    "import Bar (bar)"))
                 '("import Bar hiding (foo)"
                   "import Foo"))))

(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-0 ()
  (should (equal (haskell-sort-imports--parse-import-list "()")
                 (make-haskell-import-list
                  :start-str "("
                  :sep nil
                  :end-str ")"
                  :entries '()))))

(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-0a ()
  (should (equal (haskell-sort-imports--parse-import-list "(    )")
                 (make-haskell-import-list
                  :start-str "(    "
                  :sep nil
                  :end-str ")"
                  :entries '()))))

(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-1 ()
  (should (equal (haskell-sort-imports--parse-import-list "(foo)")
                 (make-haskell-import-list
                  :start-str "("
                  :sep nil
                  :end-str ")"
                  :entries '("foo")))))

(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-1a ()
  (should (equal (haskell-sort-imports--parse-import-list "( foo )")
                 (make-haskell-import-list
                  :start-str "( "
                  :sep nil
                  :end-str " )"
                  :entries '("foo")))))

(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-2 ()
  (should (equal (haskell-sort-imports--parse-import-list "(foo, bar)")
                 (make-haskell-import-list
                  :start-str "("
                  :sep ", "
                  :end-str ")"
                  :entries '("foo" ", " "bar")))))

(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-3 ()
  (should (equal (haskell-sort-imports--parse-import-list "  (  foo, bar  )  ")
                 (make-haskell-import-list
                  :start-str "  (  "
                  :sep ", "
                  :end-str "  )  "
                  :entries '("foo" ", " "bar")))))

(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-4 ()
  (should (equal (haskell-sort-imports--parse-import-list "  (  foo, bar, pattern Foo, (:+:), Foo(quux, frobnicator, ..))  ")
                 (make-haskell-import-list
                  :start-str "  (  "
                  :sep ", "
                  :end-str ")  "
                  :entries '("foo" ", " "bar" ", " "pattern Foo" ", " "(:+:)" ", " "Foo(quux, frobnicator, ..)")))))


(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-5 ()
  (should (equal (haskell-sort-imports--parse-import-list "(bar, baz, Żółć (..) ) ")
                 (make-haskell-import-list
                  :start-str "("
                  :sep ", "
                  :end-str " ) "
                  :entries '("bar" ", " "baz" ", " "Żółć (..)")))))

(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-6 ()
  (should (equal (haskell-sort-imports--parse-import-list
                  (tests-utils--multiline
                   ""
                   "  ( foo"
                   "  , bar"
                   "  , baz"
                   "  )"))
                 (make-haskell-import-list
                  :start-str "\n  ( "
                  :sep "\n  , "
                  :end-str "\n  )"
                  :entries '("foo" "\n  , " "bar" "\n  , " "baz")))))

(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-7 ()
  (should (equal (haskell-sort-imports--parse-import-list
                  (tests-utils--multiline
                   ""
                   "  ( foo"
                   "  , pattern Bar"
                   " ,   Decombobulator (..)  "
                   "  , baz"
                   "  )"))
                 (make-haskell-import-list
                  :start-str "\n  ( "
                  :sep "\n  , "
                  :end-str "\n  )"
                  :entries '("foo" "\n  , " "pattern Bar" "\n ,   " "Decombobulator (..)" "  \n  , " "baz")))))

(ert-deftest haskell-tests/haskell-sort-imports--parse-import-list-8 ()
  (should (equal (haskell-sort-imports--parse-import-list
                  " ((/\\))")
                 (make-haskell-import-list
                  :start-str " ("
                  :sep nil
                  :end-str ")"
                  :entries '("(/\\)")))))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-misc--add-new-import-1
    (haskell-misc--add-new-import "Foo" nil nil nil)
  (tests-utils--multiline
   ""
   "import Bar"
   ""
   "foo x = x_|_"
   "")
  (tests-utils--multiline
   ""
   "import Bar"
   "import Foo"
   ""
   "foo x = x_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-misc--add-new-import-2
    (haskell-misc--add-new-import "Foo" "quux" nil nil)
  (tests-utils--multiline
   ""
   "import Bar"
   ""
   "foo x = x_|_"
   "")
  (tests-utils--multiline
   ""
   "import Bar"
   "import Foo (quux)"
   ""
   "foo x = x_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-misc--add-new-import-3
    (haskell-misc--add-new-import "Data.Foo" "quux" nil nil)
  (tests-utils--multiline
   ""
   "import Control.Applicative"
   "import Control.Monad"
   ""
   "import Data.List qualified as L"
   ""
   "foo x = x_|_"
   "")
  (tests-utils--multiline
   ""
   "import Control.Applicative"
   "import Control.Monad"
   ""
   "import Data.Foo (quux)"
   "import Data.List qualified as L"
   ""
   "foo x = x_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-misc--add-new-import-4
    (haskell-misc--add-new-import "Data.Foo" "quux" nil nil)
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "import Control.Applicative"
   "import Control.Monad"
   ""
   "import Data.List qualified as L"
   ""
   "foo x = x_|_"
   "")
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "import Control.Applicative"
   "import Control.Monad"
   ""
   "import Data.Foo (quux)"
   "import Data.List qualified as L"
   ""
   "foo x = x_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-misc--add-new-import-5
    (haskell-misc--add-new-import "Data.Foo" "quux" nil nil)
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "import Control.Applicative"
   "import Control.Monad"
   ""
   "import Data.List qualified as L"
   "import Data.Map qualified as M"
   ""
   "import Data.AAA.ProjectSpecific qualified as PS"
   "import Project.Decombobulate"
   ""
   "foo x = x_|_"
   "")
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "import Control.Applicative"
   "import Control.Monad"
   ""
   "import Data.Foo (quux)"
   "import Data.List qualified as L"
   "import Data.Map qualified as M"
   ""
   "import Data.AAA.ProjectSpecific qualified as PS"
   "import Project.Decombobulate"
   ""
   "foo x = x_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-misc--add-new-import-5a
    (haskell-misc--add-new-import "Data.Foo" "Quux" nil "Frobnicator")
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "import Control.Applicative"
   "import Control.Monad"
   ""
   "import Data.List qualified as L"
   "import Data.Map qualified as M"
   ""
   "import Data.AAA.ProjectSpecific qualified as PS"
   "import Project.Decombobulate"
   ""
   "foo x = x_|_"
   "")
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "import Control.Applicative"
   "import Control.Monad"
   ""
   "import Data.Foo (Frobnicator(Quux))"
   "import Data.List qualified as L"
   "import Data.Map qualified as M"
   ""
   "import Data.AAA.ProjectSpecific qualified as PS"
   "import Project.Decombobulate"
   ""
   "foo x = x_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-misc--add-new-import-6
    (haskell-misc--add-new-import "Data.Foo" "quux" t nil)
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "import Control.Applicative"
   "import Control.Monad"
   ""
   "import Data.List qualified as L"
   "import Data.Map qualified as M"
   ""
   "import Data.AAA.ProjectSpecific qualified as PS"
   "import Project.Decombobulate"
   ""
   "foo x = x_|_"
   "")
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "import Control.Applicative"
   "import Control.Monad"
   ""
   "import Data.List qualified as L"
   "import Data.Map qualified as M"
   ""
   "import Data.AAA.ProjectSpecific qualified as PS"
   "import Data.Foo (quux)"
   "import Project.Decombobulate"
   ""
   "foo x = x_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-abbrev+--ensure-debug-trace-available-1
    (haskell-abbrev+--ensure-debug-trace-available)
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "import qualified Debug.Trace"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-abbrev+--ensure-debug-trace-available-2
    (haskell-abbrev+--ensure-debug-trace-available)
  (tests-utils--multiline
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   "import qualified Debug.Trace"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-abbrev+--ensure-prettyprinter-combinators-available-1
    (haskell-abbrev+--ensure-prettyprinter-combinators-available)
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "import Prettyprinter.Combinators"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-abbrev+--ensure-prettyprinter-combinators-available-2
    (haskell-abbrev+--ensure-prettyprinter-combinators-available)
  (tests-utils--multiline
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   "import Prettyprinter.Combinators"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-abbrev+/ensure-debug-trace-and-prettyprinter-combinators-available-1
    (progn
      (haskell-abbrev+--ensure-debug-trace-available)
      (haskell-abbrev+--ensure-prettyprinter-combinators-available))
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "import qualified Debug.Trace"
   "import Prettyprinter.Combinators"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-abbrev+/ensure-debug-trace-and-prettyprinter-combinators-available-2
    (progn
      (haskell-abbrev+--ensure-debug-trace-available)
      (haskell-abbrev+--ensure-prettyprinter-combinators-available))
  (tests-utils--multiline
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   "import qualified Debug.Trace"
   "import Prettyprinter.Combinators"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-abbrev+/ensure-debug-trace-and-prettyprinter-combinators-available-3
    (progn
      (haskell-abbrev+--ensure-debug-trace-available)
      (haskell-abbrev+--ensure-prettyprinter-combinators-available))
  (tests-utils--multiline
   "import Data.List"
   ""
   "foo x = do"
   "  _|_"
   "  bar x"
   "")
  (tests-utils--multiline
   "import qualified Debug.Trace"
   "import Prettyprinter.Combinators"
   ""
   "import Data.List"
   ""
   "foo x = do"
   "  _|_"
   "  bar x"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-navigate-imports-1
    (haskell-navigate-imports)
  (tests-utils--multiline
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "_|_import System.IO"
   ""
   "main = undefined"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-navigate-imports-2
    (haskell-navigate-imports)
  (tests-utils--multiline
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   "_|_import System.IO"
   ""
   "main = undefined"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--export-ident-1
    (haskell--export-ident "foo")
  (tests-utils--multiline
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--export-ident-2
    (haskell--export-ident "foo")
  (tests-utils--multiline
   ""
   "module Foo where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "module Foo where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--export-ident-3
    (haskell--export-ident "foo")
  (tests-utils--multiline
   ""
   "module Foo (bar) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "module Foo (bar, foo) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--export-ident-4
    (haskell--export-ident "foo")
  (tests-utils--multiline
   ""
   "module Foo (bar, quux) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "module Foo (bar, quux, foo) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--export-ident-5
    (haskell--export-ident "foo")
  (tests-utils--multiline
   ""
   "module Foo"
   "  ( bar"
   "  , quux"
   "  ) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "module Foo"
   "  ( bar"
   "  , quux"
   "  , foo"
   "  ) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--export-ident-6
    (haskell--export-ident "foo")
  (tests-utils--multiline
   ""
   "module Foo"
   "  ( bar"
   "  , quux"
   "  -- * Reexports"
   "  , frobnicate"
   "  ) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "module Foo"
   "  ( bar"
   "  , quux"
   "  -- * Reexports"
   "  , frobnicate"
   "  , foo"
   "  ) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--export-ident-7
    (haskell--export-ident "foo")
  (tests-utils--multiline
   ""
   "module Foo"
   "  ( bar"
   "  , Foo(Bar, Baz)"
   "  ) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "module Foo"
   "  ( bar"
   "  , Foo(Bar, Baz)"
   "  , foo"
   "  ) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--export-ident-8
    (haskell--export-ident "foo")
  (tests-utils--multiline
   ""
   "module Foo () where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   "")
  (tests-utils--multiline
   ""
   "module Foo (foo) where"
   ""
   "import System.IO"
   ""
   "main = undefined_|_"
   ""))

(ert-deftest haskell-tests/haskel-misc--is-operator?-1 ()
  (should (haskel-misc--is-operator? "+")))

(ert-deftest haskell-tests/haskel-misc--is-operator?-2 ()
  (should (haskel-misc--is-operator? "++")))

(ert-deftest haskell-tests/haskel-misc--is-operator?-3 ()
  (should (haskel-misc--is-operator? "##")))

(ert-deftest haskell-tests/haskel-misc--is-operator?-4 ()
  (should (haskel-misc--is-operator? "<=<")))

(ert-deftest haskell-tests/haskel-misc--is-operator?-5 ()
  (should-not (haskel-misc--is-operator? "abc")))

(ert-deftest haskell-tests/haskel-misc--is-operator?-6 ()
  (should-not (haskel-misc--is-operator? "a++")))

(ert-deftest haskell-tests/haskel-misc--is-operator?-7 ()
  (should-not (haskel-misc--is-operator? "")))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--search-for-haskell-symbol-at-point-1
    (re-search-forward "\\_<Node")
  (tests-utils--multiline
   "_|_"
   "foo x = x:Node (x - 1)"
   "")
  (tests-utils--multiline
   ""
   "foo x = x:Node_|_ (x - 1)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--search-for-haskell-symbol-at-point-1a
    (re-search-forward "\\_<Node")
  (tests-utils--multiline
   "_|_"
   "foo x = x :Node (x - 1)"
   "")
  (tests-utils--multiline
   ""
   "foo x = x :Node_|_ (x - 1)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--search-for-haskell-symbol-at-point-1b
    (re-search-forward "\\_<Node")
  (tests-utils--multiline
   "_|_"
   "foo x = x: Node (x - 1)"
   "")
  (tests-utils--multiline
   ""
   "foo x = x: Node_|_ (x - 1)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--search-for-haskell-symbol-at-point-1c
    (re-search-forward "\\_<Node")
  (tests-utils--multiline
   "_|_"
   "foo x = x : Node (x - 1)"
   "")
  (tests-utils--multiline
   ""
   "foo x = x : Node_|_ (x - 1)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--search-for-haskell-symbol-at-point-1d
    (re-search-forward "\\_<Node")
  (tests-utils--multiline
   "_|_"
   "foo x=x:Node(x-1)"
   "")
  (tests-utils--multiline
   ""
   "foo x=x:Node_|_(x-1)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--search-for-haskell-symbol-at-point-2
    (re-search-forward "\\_<x\\_>")
  (tests-utils--multiline
   "_|_"
   "foo x=x:Node(x-1)"
   "")
  (tests-utils--multiline
   ""
   "foo x_|_=x:Node(x-1)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--search-for-haskell-symbol-at-point-2a
    (dotimes (_ 2)
      (re-search-forward "\\_<x\\_>"))
  (tests-utils--multiline
   "_|_"
   "foo x=x:Node(x-1)"
   "")
  (tests-utils--multiline
   ""
   "foo x=x_|_:Node(x-1)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--search-for-haskell-symbol-at-point-2b
    (dotimes (_ 3)
      (re-search-forward "\\_<x\\_>"))
  (tests-utils--multiline
   "_|_"
   "foo x=x:Node(x-1)"
   "")
  (tests-utils--multiline
   ""
   "foo x=x:Node(x_|_-1)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--search-for-haskell-symbol-at-point-3
    (search-for-haskell-symbol-at-point-forward 1)
  (tests-utils--multiline
   ""
   "foo x = Foo.Bar.de_|_combobulate x + y"
   "  where"
   "    y = Foo.Bar.decombobulate (x + x)"
   "")
  (tests-utils--multiline
   ""
   "foo x = Foo.Bar.decombobulate x + y"
   "  where"
   "    y = Foo.Bar.decombobulate_|_ (x + x)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell--search-for-haskell-symbol-at-point-3a
    (with-syntax-table haskell-search-fixed-syntax-table
      (re-search-forward "\\_<decombobulate\\_>"))
  (tests-utils--multiline
   ""
   "foo x = Foo.Bar.de_|_combobulate x + y"
   "  where"
   "    y = Foo.Bar.decombobulate (x + x)"
   "")
  (tests-utils--multiline
   ""
   "foo x = Foo.Bar.decombobulate x + y"
   "  where"
   "    y = Foo.Bar.decombobulate_|_ (x + x)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-backward-up-indentation-or-sexp-1
    (haskell-backward-up-indentation-or-sexp)
  (tests-utils--multiline
   ""
   "foo x = Foo.Bar.decombobulate x + y"
   ""
   "  where"
   "_|_"
   "    y = Foo.Bar.decombobulate (x + x)"
   "")
  (tests-utils--multiline
   ""
   "foo x = Foo.Bar.decombobulate x + y"
   ""
   "  _|_where"
   ""
   "    y = Foo.Bar.decombobulate (x + x)"
   ""))

(haskell-tests--test-buffer-contents
    haskell-tests/haskell-backward-up-indentation-or-sexp-2
    (haskell-backward-up-indentation-or-sexp)
  (tests-utils--multiline
   ""
   "foo x = Foo.Bar.decombobulate x + y"
   ""
   "  _|_where"
   ""
   "    y = Foo.Bar.decombobulate (x + x)"
   "")
  (tests-utils--multiline
   ""
   "_|_foo x = Foo.Bar.decombobulate x + y"
   ""
   "  where"
   ""
   "    y = Foo.Bar.decombobulate (x + x)"
   ""))

;; This test may infilitely loop if ‘poly-alex-happy-find-front’ is not implemented correctly.
(haskell-tests--make-multiple-test-result-tests
    haskell-tests/poly-alex-happy-find-front-1
  :entries
  ((:subname
    a
    :action (poly-alex-happy-find-front -1 nil)
    :expected-value (cons 1 3))
   (:subname
    b
    :action (poly-alex-happy-find-front -1 t)
    :expected-value (cons 1 3))
   (:subname
    c
    :action (poly-alex-find-front -1)
    :expected-value (cons 1 3))
   (:subname
    d
    :action (poly-happy-find-front -1)
    :expected-value (cons 1 3)))
  :contents
  (tests-utils--multiline
   "{"
   "-_|_- {-# LANGUAGE LambdaCase          #-}"
   "{-# LANGUAGE OverloadedStrings   #-}"
   "{-# LANGUAGE ScopedTypeVariables #-}"
   ""
   "module Data.Elisp.Parser.Parser (parseSexp, parseProgram) where"
   ""
   "import Control.Applicative"
   ""
   "}"))

(haskell-tests--test-result
    haskell-tests/syntax-propertize-1
  :action
  (list (nth 3 (parse-partial-sexp (line-beginning-position) (point)))
        (haskell-smart-operators--in-string-syntax?))
  :expected-value
  (list nil nil)
  :contents
  (tests-utils--multiline
   ""
   "isWord :: Int# -> Bool#"
   "isWord x = case chr# x of"
   "  ' '# -> _|_False#"
   ""))

(haskell-tests--test-result
    haskell-tests/syntax-propertize-2
  :action
  (list (nth 3 (parse-partial-sexp (line-beginning-position) (point)))
        (haskell-smart-operators--in-string-syntax?))
  :expected-value
  (list nil nil)
  :contents
  (tests-utils--multiline
   ""
   "isWord :: Int# -> Bool#"
   "isWord x = case chr# x of"
   "  '/'#  -> False#"
   "  '\\\\'# -> _|_False#"
   ""))

(haskell-tests--test-result
    haskell-tests/syntax-propertize-3
  :action
  (list (nth 3 (parse-partial-sexp (line-beginning-position -1) (point)))
        (haskell-smart-operators--in-string-syntax?))
  :expected-value
  (list nil nil)
  :contents
  (tests-utils--multiline
   ""
   "isWord :: Int# -> Bool#"
   "isWord x = case chr# x of"
   "  '/'#  -> False#"
   "  '\\\\'# -> False#"
   "  _     -> _|_True#"
   ""))

(haskell-tests--test-result
    haskell-tests/syntax-propertize-4
  :action
  (list (nth 3 (parse-partial-sexp (line-beginning-position -1) (point)))
        (haskell-smart-operators--in-string-syntax?))
  :expected-value
  (list t t)
  :contents
  (tests-utils--multiline
   ""
   "test :: Int -> Bool"
   "test x = todo [foo| bar _|_ baz |] x"
   "")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/syntax-propertize-4a
  :action
  (list (nth 3 (parse-partial-sexp (line-beginning-position -1) (point)))
        (haskell-smart-operators--in-string-syntax?))
  :expected-value
  (list nil nil)
  :contents
  (tests-utils--multiline
   ""
   "test :: Int -> Bool"
   "test x = todo [e| bar _|_ baz |] x"
   "")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/syntax-propertize-4b
  :action
  (list (nth 3 (parse-partial-sexp (line-beginning-position -1) (point)))
        (haskell-smart-operators--in-string-syntax?))
  :expected-value
  (list nil nil)
  :contents
  (tests-utils--multiline
   ""
   "test :: Int -> Bool"
   "test x = todo [t| bar _|_ baz |] x"
   "")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
    haskell-tests/syntax-propertize-4c
  :action
  (list (nth 3 (parse-partial-sexp (line-beginning-position -1) (point)))
        (haskell-smart-operators--in-string-syntax?))
  :expected-value
  (list nil nil)
  :contents
  (tests-utils--multiline
   ""
   "test :: Int -> Bool"
   "test x = todo [d| bar _|_ baz |] x"
   "")
  :modes (haskell-ts-mode))

(haskell-tests--test-result
 haskell-tests/syntax-propertize-5
 :action
 (list (nth 3 (parse-partial-sexp (line-beginning-position -1) (point)))
       (haskell-smart-operators--in-string-syntax?))
 :expected-value
 (list t t)
 :contents
 (tests-utils--multiline
  ""
  "test :: Int -> Bool"
  "test x = todo [foo| bar $(_|_) baz |] x"
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-1
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-2
 :action
 (haskell-ts-convert-to-multiline-string t)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar\\"
  "  \\\"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-3
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar\\nbaz\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar"
  "  baz"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-4a
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar\\n\\"
  "  \\baz\\n\\"
  "  \\quux\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar"
  "  baz"
  "  quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-4b
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"bar\\n\\"
  "  \\baz\\n\\"
  "  _|_\\quux\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  bar"
  "  baz"
  "  _|_quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-5a
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar\\n\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-5b
 :action
 (haskell-ts-convert-to-multiline-string t)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar\\n\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-6a
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"bar\\n\\"
  "  _|_\\baz\\n\\"
  "  \\quux\\n\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  bar"
  "  _|_baz"
  "  quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-6b
 :action
 (haskell-ts-convert-to-multiline-string t)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"bar\\n\\"
  "  _|_\\baz\\n\\"
  "  \\quux\\n\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  bar"
  "  _|_baz"
  "  quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-7a
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"bar\\n\\"
  "  _|_\\baz1\\nbaz2\\n\\"
  "  \\quux\\n\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  bar"
  "  _|_baz1"
  "  baz2"
  "  quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-7b
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"bar\\n\\"
  "  \\baz1\\nba_|_z2\\n\\"
  "  \\quux\\n\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  bar"
  "  baz1"
  "  ba_|_z2"
  "  quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-8a
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo :: String"
  "foo ="
  "  [hereDoc|"
  "  foo"
  "  _|_bar"
  "  baz"
  "  |]"
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo :: String"
  "foo ="
  "  \"\"\""
  "  foo"
  "  _|_bar"
  "  baz"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-9a
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\"baz\\\" quux\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \"baz\" quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-9b
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\"\\\"baz\\\"\\\" quux\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \"\"baz\"\" quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-9bb
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\\\\\"\\\"baz\\\"\\\" quux\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \\\\\"\"baz\"\" quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-9bbb
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\\\\\"\\\"baz\\\"\\\"\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \\\\\"\"baz\"\""
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-9c
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\"\\\"\\\"baz\\\"\\\"\\\" quux\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \\\"\\\"\\\"baz\\\"\\\"\\\" quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-9d
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\"\\\"\\\"\\\"baz\\\"\\\"\\\"\\\" quux\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \\\"\\\"\\\"\\\"baz\\\"\\\"\\\"\\\" quux"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-10
 :action
 (haskell-ts-convert-to-multiline-string t)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\"baz\\\"\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \"baz\"\\"
  "  \\\"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-11a
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\\n baz\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \\\\n baz"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-11b
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\\\\n baz\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \\\\"
  "   baz"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-11c
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\\\\\\n baz\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \\\\\\\\n baz"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-11d
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  \"_|_bar \\\\\\\\\\n baz\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  \"\"\""
  "  _|_bar \\\\\\\\"
  "   baz"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-12
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "bar ="
  "  \"_|_foo = \\\"foo\\\\n\\\\\\n\\"
  "  \\  \\\\x\\\" bar\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "bar ="
  "  \"\"\""
  "  _|_foo = \"foo\\\\n\\\\"
  "    \\\\x\" bar"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(haskell-tests--test-buffer-contents*
 :name
 haskell-tests/haskell-ts-convert-to-multiline-string-13
 :action
 (haskell-ts-convert-to-multiline-string)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  _|_\"This is a factorial function\\n\\"
  "  \\\\n\\"
  "  \\> f :: Integer -> Integer\\n\\"
  "  \\> f 0 = 1\\n\\"
  "  \\> f n = \\n\\"
  "  \\>   n * (f $ n - 1)\\n\\"
  "  \\\\n\\"
  "  \\And that's it !\""
  "")
 :expected-value
 (tests-utils--multiline
  "{-# LANGUAGE MultilineStrings #-}"
  ""
  "foo ="
  "  _|_\"\"\""
  "  This is a factorial function"
  "  "
  "  > f :: Integer -> Integer"
  "  > f 0 = 1"
  "  > f n = "
  "  >   n * (f $ n - 1)"
  "  "
  "  And that's it !"
  "  \"\"\""
  "")
 :modes (haskell-ts-mode))

(provide 'haskell-tests)

;; (let ((ert-debug-on-error nil))
;;   (eproj-reset-projects)
;;   (ert
;;    ;; (join-lines (-map (comp #'regexp-quote #'symbol->string)
;;    ;;                   haskell-tests/tests)
;;    ;;             "\\|")
;;    "haskell-tests/.*")
;;   nil)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; haskell-tests.el ends here
