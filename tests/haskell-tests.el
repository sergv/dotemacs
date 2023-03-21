;; haskell-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 26 November 2013
;; Description:

(eval-when-compile
  (require 'cl-lib))

(require 'dante)
(require 'haskell-abbrev+)
(require 'haskell-block-indent)
(require 'haskell-format-setup)
(require 'haskell-misc)
(require 'haskell-regexen)
(require 'haskell-smart-operators-mode)

(require 'common)
(require 'ert)
(require 'tests-utils)

(ert-deftest haskell-tests/abbrev+-extract-module-name ()
  (should (string= (haskell-abbrev+-extract-mod-name "Foo.Bar")
                   "Bar"))
  (should (string= (haskell-abbrev+-extract-mod-name "Foo.Bar.Baz")
                   "Baz"))
  (should (string= (haskell-abbrev+-extract-mod-name "Foo'.Bar2.Baz_3.Quux")
                   "Quux")))

(defmacro haskell-tests--with-temp-buffer (action contents)
  (declare (indent 1))
  `(tests-utils--with-temp-buffer
    :action ,action
    :contents ,contents
    :initialisation (haskell-mode)))

(defmacro haskell-tests--test-buffer-contents (action contents expected-value)
  (declare (indent 1))
  `(tests-utils--test-buffer-contents
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    :initialisation (haskell-mode)
    :buffer-id haskell))

(defmacro haskell-tests--cabal--test-buffer-contents (action contents expected-value)
  (declare (indent 1))
  `(tests-utils--test-buffer-contents
    :action ,action
    :contents ,contents
    :expected-value ,expected-value
    :initialisation (haskell-cabal-mode)
    :buffer-id haskell-cabal))

(defmacro haskell-tests--make-multiple-test-buffer-contents (initial entries)
  "Define a set of tests that share initial buffer state but
execute diffent actions and reach different buffer states in the
end."
  (declare (indent 1))
  `(progn
     ,@(cl-loop
        for entry in entries
        collect
        `(ert-deftest ,(cl-first entry) ()
             (haskell-tests--test-buffer-contents
                 ,(cl-second entry)
               ,initial
               ,(cl-third entry))))))

(cl-defmacro haskell-tests--test-result (&key action expected-value contents)
  `(haskell-tests--with-temp-buffer
       (should (equal ,action ,expected-value))
     ,contents))

(defmacro haskell-tests--test-evaluate (action contents expected-value)
  (declare (indent 1))
  `(haskell-tests--with-temp-buffer
       ,action
     ,contents))


(ert-deftest haskell-tests/haskell-align-language-pragmas-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-align-language-pragmas-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-align-language-pragmas-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-align-language-pragmas-3 ()
  (haskell-tests--test-buffer-contents
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
     "-- bar")))

(ert-deftest haskell-tests/haskell-align-language-pragmas-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-align-language-pragmas (point))
    (tests-utils--multiline
     "{-# LANGUAGE Safe #-}"
     "{-# LANGUAGE AlternativeLayoutRule _|_ #-}"
     "{-# LANGUAGE AllowAmbiguousTypes   #-}")
    (tests-utils--multiline
     "{-# LANGUAGE AllowAmbiguousTypes   #-}"
     "{-# LANGUAGE AlternativeLayoutRule #-}"
     "{-# LANGUAGE Safe                  #-}_|_")))

;; Nothing should happen, no errors and point should not move.
(ert-deftest haskell-tests/haskell-align-options-ghc-pragmas-0 ()
  (haskell-tests--test-buffer-contents
      (haskell-align-language-pragmas (point))
    (tests-utils--multiline
     "{-# OPTIONS_GHC -Wno-orphans #-}"
     "{-# OPTIONS_GHC -Wno-implicit-prelude_|_ #-}"
     "{-# OPTIONS_GHC -O0   #-}")
    (tests-utils--multiline
     "{-# OPTIONS_GHC -Wno-orphans #-}"
     "{-# OPTIONS_GHC -Wno-implicit-prelude_|_ #-}"
     "{-# OPTIONS_GHC -O0   #-}")))

(ert-deftest haskell-tests/haskell-align-options-ghc-pragmas-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-align-options-ghc-pragmas (point))
    (tests-utils--multiline
     "{-# OPTIONS_GHC -Wno-orphans #-}"
     "{-# OPTIONS_GHC -Wno-implicit-prelude_|_ #-}"
     "{-# OPTIONS_GHC -O0   #-}")
    (tests-utils--multiline
     "{-# OPTIONS_GHC -O0                   #-}"
     "{-# OPTIONS_GHC -Wno-implicit-prelude #-}"
     "{-# OPTIONS_GHC -Wno-orphans          #-}_|_")))

(ert-deftest haskell-tests/haskell-reindent-at-point-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-reindent-at-point-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-reindent-at-point-3 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-reindent-at-point-4 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-reindent-at-point-5 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-1 ()
  (haskell-tests--test-result
    :action
    (haskell-format--get-language-extensions (current-buffer) t)
    :expected-value
    '("Safe" "AlternativeLayoutRule" "AllowAmbiguousTypes" "FlexibleContexts")
    :contents
    (tests-utils--multiline
     "_|_"
     "{-# LANGUAGE Safe #-}"
     "{-#LANGUAGE AlternativeLayoutRule #-}"
     "{-# LANGUAGE AllowAmbiguousTypes#-}"
     "{-#LANGUAGE FlexibleContexts#-}")))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-2 ()
  (haskell-tests--test-result
    :action
    (haskell-format--get-language-extensions (current-buffer) t)
    :expected-value
    '("Safe" "AlternativeLayoutRule" "AllowAmbiguousTypes" "FlexibleContexts")
    :contents
    (tests-utils--multiline
     "_|_"
     "{-# LANGUAGE Safe,AlternativeLayoutRule, AllowAmbiguousTypes,"
     "FlexibleContexts #-}")))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-3 ()
  (haskell-tests--test-result
    :action
    (haskell-format--get-language-extensions (current-buffer) t)
    :expected-value
    '("Safe" "AlternativeLayoutRule" "AllowAmbiguousTypes" "FlexibleContexts")
    :contents
    (tests-utils--multiline
     "_|_"
     "{-# language Safe #-}"
     "{-#language AlternativeLayoutRule #-}"
     "{-# language AllowAmbiguousTypes#-}"
     "{-#language FlexibleContexts#-}")))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-4 ()
  (haskell-tests--test-result
    :action
    (haskell-format--get-language-extensions (current-buffer) t)
    :expected-value
    '("Safe" "AlternativeLayoutRule" "AllowAmbiguousTypes" "FlexibleInstances" "FlexibleContexts")
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
     " FlexibleContexts#-}")))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-5 ()
  (haskell-tests--test-result
    :action
    (haskell-format--get-language-extensions (current-buffer) t)
    :expected-value
    '("FlexibleContexts" "FlexibleInstances" "RecordWildCards" "AllowAmbiguousTypes")
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
     "")))


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


(ert-deftest haskell-tests/forward-haskell-symbol-1 ()
  (haskell-tests--test-result
    :action
    (list
     (bounds-of-thing-at-point 'haskell-symbol)
     (thing-at-point 'haskell-symbol))
    :expected-value
    (list
     (cons 2 9)
     "fooobar")
    :contents
    " fooo_|_bar "))

(ert-deftest haskell-tests/forward-haskell-symbol-2 ()
  (haskell-tests--test-result
    :action
    (list
     (bounds-of-thing-at-point 'haskell-symbol)
     (thing-at-point 'haskell-symbol))
    :expected-value
    (list
     (cons 2 9)
     "Fooobar")
    :contents
    " Fooo_|_bar "))

(ert-deftest haskell-tests/forward-haskell-symbol-3 ()
  (haskell-tests--test-result
    :action
    (list
     (bounds-of-thing-at-point 'haskell-symbol)
     (thing-at-point 'haskell-symbol))
    :expected-value
    (list
     (cons 3 10)
     "Fooobar")
    :contents
    " 'Fooo_|_bar "))

(ert-deftest haskell-tests/forward-haskell-symbol-4 ()
  (haskell-tests--test-result
    :action
    (list
     (bounds-of-thing-at-point 'haskell-symbol)
     (thing-at-point 'haskell-symbol))
    :expected-value
    (list
     (cons 4 11)
     "Fooobar")
    :contents
    " ''Fooo_|_bar "))

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

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 +_|_2"
    "x = 1 ++ _|_2"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 +           _|_ 2"
    "x = 1 ++_|_ 2"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
    "x = 1 +           _|_"
    "x = 1 +* _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1_|_"
    "x = 1 + _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-5 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 _|_"
    "x = 1 + _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-6 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1  _|_"
    "x = 1  + _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-7 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = f \(_|_"
    "x = f \(+ _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--inserting-@-avoid-spaces-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
    "x = x_|_y"
    "x = x@_|_y"))

(ert-deftest haskell-tests/haskell-smart-operators--inserting-@-avoid-spaces-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
    "x = x _|_y"
    "x = x @_|_y"))

(ert-deftest haskell-tests/haskell-smart-operators--inserting-@-avoid-spaces-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
    ;; If @ was appended to an operator then do insert a space after it!
    "x = x +_|_y"
    "x = x +@ _|_y"))

(ert-deftest haskell-tests/haskell-smart-operators--inserting-@-avoid-spaces-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
    ;; If @ was appended to an operator then do insert a space after it!
    "x = x +  _|_y"
    "x = x +@ _|_y"))

(ert-deftest haskell-tests/haskell-smart-operators--inserting-@-avoid-spaces-5 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
    ;; If @ was appended to an operator then do insert a space after it!
    "x = x +  _|_  y"
    "x = x +@_|_  y"))

(ert-deftest haskell-tests/haskell-smart-operators--@-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?@)
    (tests-utils--multiline
     ""
     "foo x xs = foo (Median3or5 _|_)"
     "")
    (tests-utils--multiline
     ""
     "foo x xs = foo (Median3or5 @_|_)"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--inserting-within-backtics-avoids-spaces-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
    "x = x `andI_|_` y"
    "x = x `andI#_|_` y"))

(ert-deftest haskell-tests/haskell-smart-operators--inserting-within-backtics-avoids-spaces-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
    "x = x `_|_` y"
    "x = x `#_|_` y"))

(ert-deftest haskell-tests/haskell-smart-operators--sections-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = f \(_|_\)"
    "x = f \(+_|_\)"))

(ert-deftest haskell-tests/haskell-smart-operators--sections-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = f \(     _|_\)"
    "x = f \(+_|_\)"))

(ert-deftest haskell-tests/haskell-smart-operators--space-after--lambdas-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?\\)
    "x = \(_|_\)"
    "x = \(\\_|_\)"))

(ert-deftest haskell-tests/haskell-smart-operators--space-after--lambdas-2a ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?$)
    "x = f _|_\\ x -> x"
    "x = f $ _|_\\ x -> x"))

(ert-deftest haskell-tests/haskell-smart-operators--space-after--lambdas-2b ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f _|_\\ x -> x"
    "x = f $ _|_\\ x -> x"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "x = \[_|_"
    "x = \[| _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "x = \[| foobar _|_\]"
    "x = \[| foobar |_|_\]"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "x = \[foo_|_"
    "x = \[foo| _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "x = \[fooBar_|_"
    "x = \[fooBar| _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-5 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "x = \[fooBar'_|_"
    "x = \[fooBar'| _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-6 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "x = \[_fooBar_|_"
    "x = \[_fooBar| _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-7 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "x = \[Quux.fooBar_|_"
    "x = \[Quux.fooBar| _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-8 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "x = \[a+b_|_"
    "x = \[a+b | _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--beginning-of-buffer ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
    "_|_ + bar"
    " *_|_ + bar"))

(ert-deftest haskell-tests/haskell-smart-operators--end-of-buffer ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
    "+ bar_|_"
    "+ bar * _|_"))


(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "--_|_ foobar"
    "-- |_|_ foobar"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    "--_|_ foobar"
    "-- ^_|_ foobar"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "--     _|_ foobar"
    "-- |_|_ foobar"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    "--      _|_ foobar"
    "-- ^_|_ foobar"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-no-action-if-not-toplevel-comment-1 ()
  (haskell-tests--test-buffer-contents
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
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-no-action-if-not-toplevel-comment-2 ()
  (haskell-tests--test-buffer-contents
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
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-no-action-if-not-toplevel-comment-3 ()
  (haskell-tests--test-buffer-contents
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
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "--    _|_"
    "-- | _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    "--    _|_"
    "-- ^ _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "--    _|_"
    "--    +_|_"))


(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-disabled-if-not-on-first-column-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    " --    _|_"
    " --    |_|_"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-disabled-if-not-on-first-column-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    " --    _|_"
    " --    ^_|_"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-disabled-if-not-on-first-column-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    " --    _|_"
    " --    +_|_"))


(ert-deftest haskell-tests/haskell-smart-operators--almost-haddock-comments-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "-       _|_"
    "-| _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--almost-haddock-comments-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    "-      _|_"
    "-^ _|_"))


(ert-deftest haskell-tests/haskell-smart-operators--operator-$-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f _|_(xs ++ ys)"
    "x = f $ _|_xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f_|_ (xs ++ ys)"
    "x = f $ _|_xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f _|_ (xs ++ ys)"
    "x = f $ _|_xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f _|_     (xs ++ ys)"
    "x = f $ _|_xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-5 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f _|_(xs ++ ys)"
    "x = f $ _|_xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-6 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f_|_(xs ++ ys)"
    "x = f $ _|_xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$--keeps-parens-for-fmap-operator-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f <_|_(xs ++ ys)"
    "x = f <$ _|_(xs ++ ys)"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f<_|_ (xs ++ ys)"
    "x = f<$_|_ (xs ++ ys)"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f <_|_ (xs ++ ys)"
    "x = f <$_|_ (xs ++ ys)"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f <_|_  (xs ++ ys)"
    "x = f <$_|_  (xs ++ ys)"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-5 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f <  _|_  (xs ++ ys)"
    "x = f <$_|_  (xs ++ ys)"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-6 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f <_|_(xs ++ ys)"
    "x = f <$ _|_(xs ++ ys)"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-keeps-parens-for-fmap-operator-7 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f<_|_(xs ++ ys)"
    "x = f<$ _|_(xs ++ ys)"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-before-double-quotes-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = error _|_\"foobar\" ++ show baz"
    "x = error $ _|_\"foobar\" ++ show baz"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-within-double-quotes-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = error \"_|_foobar\""
    "x = error \"$_|_foobar\""))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-within-double-quotes-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = error \"foobar_|_\""
    "x = error \"foobar$_|_\""))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-within-double-quotes-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = error \"foobar_|_baz\""
    "x = error \"foobar$_|_baz\""))

(ert-deftest haskell-tests/haskell-smart-operators--guard-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    (tests-utils--multiline
     "foo x xs"
     "  | _|_x `elem` xs = xs"
     "  | otherwise   = []")
    (tests-utils--multiline
     "foo x xs"
     "  | | _|_x `elem` xs = xs"
     "  | otherwise   = []")))

(ert-deftest haskell-tests/haskell-smart-operators--guard-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    (tests-utils--multiline
     "foo x xs"
     "  | x `elem` xs |_|_= xs"
     "  | otherwise   = []")
    (tests-utils--multiline
     "foo x xs"
     "  | x `elem` xs ||_|_= xs"
     "  | otherwise   = []")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-0 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-magic-hash?))
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
   (tests-utils--multiline
    "{-# LANGUAGE MagicHash #-}"
    "foo x xs = (bar_|_, baz)"
    "")
   (tests-utils--multiline
    "{-# LANGUAGE MagicHash #-}"
    "foo x xs = (bar#_|_, baz)"
    "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-1 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-magic-hash?))
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
   (tests-utils--multiline
    "{-# LANGUAGE MagicHash #-}"
    "foo x xs = foo_|_ +# bar"
    "")
   (tests-utils--multiline
    "{-# LANGUAGE MagicHash #-}"
    "foo x xs = foo#_|_ +# bar"
    "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-2 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-magic-hash?))
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = foo _|_ +# bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = foo#_|_ +# bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-3 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-magic-hash?))
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = foo _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = foo# _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-4 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-magic-hash?))
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = foo #  _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = foo ## _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-5 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = foo _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = foo# # _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-6 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 1 _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 1# _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-6a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45 _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45# _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-6b ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45e-2 _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45e-2# _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-7 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 1 _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 1## _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-7a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45 _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45## _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-7a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45e-2 _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45e-2## _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-8 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 1 _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 1## # _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-8a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45 _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45## # _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-8b ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?#))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45e-11 _|_bar"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     "foo x xs = 123.45e-11## # _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-and-equals-space-1 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators-hash)
        (should (haskell-ext-tracking-have-magic-hash?))
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?=))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     ""
     "  where"
     "    ptr_|_"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     ""
     "  where"
     "    ptr# = _|_"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--magic-hash-and-equals-space-2 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-magic-hash?))
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?=))
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     ""
     "  where"
     "    ptr#_|_"
     "")
    (tests-utils--multiline
     "{-# LANGUAGE MagicHash #-}"
     ""
     "  where"
     "    ptr# = _|_"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--dot-1 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators-dot))
    (tests-utils--multiline
     ""
     "foo x xs = foo .+  _|_bar"
     "")
    (tests-utils--multiline
     ""
     "foo x xs = foo .+. _|_bar"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--dot-2 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators-mode +1)
        (haskell-ext-tracking-mode +1)
        (haskell-smart-operators-dot))
    (tests-utils--multiline
     ""
     "foo x xs = foo .+  _|_ bar"
     "")
    (tests-utils--multiline
     ""
     "foo x xs = foo .+._|_ bar"
     "")))


;; (ert-deftest haskell-tests/shm/!-1 ()
;;   (haskell-tests--test-buffer-contents
;;       (shm/!)
;;     (tests-utils--multiline
;;      "data Foo = Foo"
;;      "  { foo ::_|_ Set Int"
;;      "  , bar :: Map Int Double"
;;      "  }")
;;     (tests-utils--multiline
;;      "data Foo = Foo"
;;      "  { foo :: !_|_(Set Int)"
;;      "  , bar :: Map Int Double"
;;      "  }")))
;;
;; (ert-deftest haskell-tests/shm/!-2 ()
;;   (haskell-tests--test-buffer-contents
;;       (shm/!)
;;     (tests-utils--multiline
;;      "data Foo = Foo"
;;      "  { foo :: _|_Set Int"
;;      "  , bar :: Map Int Double"
;;      "  }")
;;     (tests-utils--multiline
;;      "data Foo = Foo"
;;      "  { foo :: !_|_(Set Int)"
;;      "  , bar :: Map Int Double"
;;      "  }")))
;;
;; (ert-deftest haskell-tests/shm/!-3 ()
;;   (haskell-tests--test-buffer-contents
;;       (shm/!)
;;     (tests-utils--multiline
;;      "data Foo = Foo"
;;      "  { foo :: _|_ Set Int"
;;      "  , bar :: Map Int Double"
;;      "  }")
;;     (tests-utils--multiline
;;      "data Foo = Foo"
;;      "  { foo :: !_|_(Set Int)"
;;      "  , bar :: Map Int Double"
;;      "  }")))
;;
;; (ert-deftest haskell-tests/shm/!-4 ()
;;   (haskell-tests--test-buffer-contents
;;       (shm/!)
;;     (tests-utils--multiline
;;      "data Foo = Foo"
;;      "  { foo ::      _|_      Set Int"
;;      "  , bar :: Map Int Double"
;;      "  }")
;;     (tests-utils--multiline
;;      "data Foo = Foo"
;;      "  { foo :: !_|_(Set Int)"
;;      "  , bar :: Map Int Double"
;;      "  }")))

(ert-deftest haskell-tests/haskell-smart-operators--arrows-in-non-haddock-comment-1 ()
  (haskell-tests--test-buffer-contents
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
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--arrows-in-non-haddock-comment-2 ()
  (haskell-tests--test-buffer-contents
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
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--arrows-in-non-haddock-comment-3 ()
  (haskell-tests--test-buffer-contents
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
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--arrows-in-non-haddock-comment-4 ()
  (haskell-tests--test-buffer-contents
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
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--arrows-in-haddock-comment-1 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
        (insert "test"))
    (tests-utils--multiline
     "-- _|_")
    (tests-utils--multiline
     "-- >test_|_")))


(ert-deftest haskell-tests/haskell-smart-operators-hyphen--1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-hyphen--2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-hyphen--3 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-hyphen--4 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-hyphen--5 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-hyphen--6 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-hyphen--7 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-hyphen--8 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-hyphen--9 ()
  (haskell-tests--test-buffer-contents
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
     "")))


(ert-deftest haskell-tests/haskell-smart-operators-exclamation-mark--1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-exclamation-mark)
    (tests-utils--multiline
     ""
     "foo x = x + _|_"
     "")
    (tests-utils--multiline
     ""
     "foo x = x +! _|_"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-exclamation-mark--2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-exclamation-mark)
    (tests-utils--multiline
     ""
     "foo x = x !_|_y"
     "")
    (tests-utils--multiline
     ""
     "foo x = x !! _|_y"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-exclamation-mark--3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-exclamation-mark)
    (tests-utils--multiline
     ""
     "foo x = x_|_y"
     "")
    (tests-utils--multiline
     ""
     "foo x = x !_|_y"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-exclamation-mark--4 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-exclamation-mark)
    (tests-utils--multiline
     ""
     "foo x = \\_|_y -> x + y"
     "")
    (tests-utils--multiline
     ""
     "foo x = \\ !_|_y -> x + y"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-exclamation-mark--5 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-exclamation-mark)
    (tests-utils--multiline
     ""
     "foo x = \\ _|_y -> x + y"
     "")
    (tests-utils--multiline
     ""
     "foo x = \\ !_|_y -> x + y"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-exclamation-mark--6 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-exclamation-mark)
    (tests-utils--multiline
     ""
     "foo _|_x = x + y"
     "")
    (tests-utils--multiline
     ""
     "foo !_|_x = x + y"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-exclamation-mark--7 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-exclamation-mark)
    (tests-utils--multiline
     ""
     "data Foo = Foo { bar :: _|_Int }"
     "")
    (tests-utils--multiline
     ""
     "data Foo = Foo { bar :: !_|_Int }"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators-exclamation-mark--8 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-exclamation-mark)
    (tests-utils--multiline
     ""
     "data Foo = Foo { bar ::_|_Int }"
     "")
    (tests-utils--multiline
     ""
     "data Foo = Foo { bar :: !_|_Int }"
     "")))


(ert-deftest haskell-tests/haskell-backspace-with-block-dedent-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-backspace-with-block-dedent-not-at-exact-indentation-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-backspace-with-block-dedent-not-at-exact-indentation-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-backspace-with-block-dedent-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-backspace-with-block-dedent-3 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-backspace-with-block-dedent-4 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-backspace-with-block-dedent-not-at-indentation-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-space-with-block-indent-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-space-with-block-indent-not-at-exact-indentation-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-space-with-block-indent-not-at-exact-indentation-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-space-with-block-indent-not-at-exact-indentation-affects-where-block-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-space-with-block-indent-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-space-with-block-indent-not-at-indentation-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-space-with-block-indent-from-zeroth-column-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-space-with-block-indent-from-zeroth-column-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-space-with-block-indent--inside-module-export-list-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-ws-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-ws-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-3 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-ws-3 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-4 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-ws-4 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-5 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-ws-5 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-6 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-ws-6 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-7 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-ws-7 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-8 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-ws-8 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-9 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-ws-9 ()
  (haskell-tests--test-buffer-contents
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
     ""
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-ws-10 ()
  ;; Cannot distinguish unfinished byt indented body from dangling type signature.
  :expected-result :failed
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--within-where-block-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--within-where-block-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--does-not-expand-if-not-at-the-end-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--does-not-expand-if-not-at-the-end-2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--does-not-expand-if-not-at-the-end-3 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--does-not-expand-if-not-at-the-end-4 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--does-not-expand-if-not-at-the-end-5 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--deep-within-do-block-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--inside-string-1 ()
  (haskell-tests--test-buffer-contents
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
     "  ]")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--at-end-of-buffer-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-newline-with-signature-expansion)
    (tests-utils--multiline
     "makeFunction"
     "  :: MonadBase IO m _|_=> Env -> CPtrdiff -> CPtrDiff")
    (tests-utils--multiline
     "makeFunction"
     "  :: MonadBase IO m"
     "  _|_=> Env -> CPtrdiff -> CPtrDiff")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion--does-not-insert-redundant-function-name-1 ()
  (haskell-tests--test-buffer-contents
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
     "makeFunction")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-indent-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-newline-with-signature-expansion)
    (tests-utils--multiline
     ""
     "foo x = do_|_"
     "")
    (tests-utils--multiline
     ""
     "foo x = do"
     "  _|_"
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-indent-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-newline-with-signature-expansion)
    (tests-utils--multiline
     ""
     "foo x = case x of_|_"
     "")
    (tests-utils--multiline
     ""
     "foo x = case x of"
     "  _|_"
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-indent-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-newline-with-signature-expansion)
    (tests-utils--multiline
     ""
     "foo x =_|_"
     "")
    (tests-utils--multiline
     ""
     "foo x ="
     "  _|_"
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-indent-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-newline-with-signature-expansion)
    (tests-utils--multiline
     ""
     "foo x = \\y -> _|_"
     "")
    (tests-utils--multiline
     ""
     "foo x = \\y ->"
     "  _|_"
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-indent-5 ()
  (haskell-tests--test-buffer-contents
      (haskell-newline-with-signature-expansion)
    (tests-utils--multiline
     ""
     "foo x = \\case_|_"
     "")
    (tests-utils--multiline
     ""
     "foo x = \\case"
     "  _|_"
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-indent-6 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-newline-with-signature-expansion-indent-7 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-move-to-topmost-start-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-move-to-topmost-end-1 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/haskell-regexen/pre-post-qualified-import-line-1 ()
  (should (string-match-p haskell-regexen/pre-post-qualified-import-line "import qualified Data.Ord  "))
  (should (string-match-p haskell-regexen/pre-post-qualified-import-line "import Data.Ord  qualified   ")))


(ert-deftest haskell-tests/haskell-qualify-import-1 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should-not (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "import Data.List"
     "import Data.Ord_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "import Data.List"
     "import qualified Data.Ord_|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-1a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord_|_ qualified"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-2 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should-not (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "import Data.List"
     "import      Data.Ord_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "import Data.List"
     "import qualified Data.Ord_|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-2a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import      Data.Ord_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import      Data.Ord_|_ qualified"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-3 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should-not (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "import Data.List"
     "import \"foo\"     Data.Ord_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "import Data.List"
     "import qualified \"foo\"     Data.Ord_|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-3a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import \"foo\"     Data.Ord_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import \"foo\"     Data.Ord_|_ qualified"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-4 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should-not (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "import Data.List"
     "import   qualified   Data.Ord_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "import Data.List"
     "import   Data.Ord_|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-4a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import   qualified   Data.Ord_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import   Data.Ord_|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-4b ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord qualified_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord_|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-4c ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord      qualified   _|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord   _|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-4d ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import)
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord      qualified   _|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord qualified   _|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-5 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should-not (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "import Data.List"
     "import  qualified   \"foo\"  Data.Ord_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "import Data.List"
     "import  \"foo\"  Data.Ord_|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-5a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import  qualified   \"foo\"    Data.Ord_|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import  \"foo\"    Data.Ord_|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-5b ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import  \"foo\"  Data.Ord   qualified   _|_"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import  \"foo\"  Data.Ord   _|_"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-6 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should-not (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "import Data.List"
     "import qualified Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "import Data.List"
     "import Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-6a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should-not (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import)
        (haskell-qualify-import))
    (tests-utils--multiline
     "import Data.List"
     "import    qualified    Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "import Data.List"
     "import qualified Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-6b ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import qualified Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-6c ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import)
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import qualified Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord qualified as Ord (Down_|_)"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-6d ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import)
        (haskell-qualify-import)
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import qualified Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-6e ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import)
        (haskell-qualify-import))
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord   qualified   as Ord (Down_|_)"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "{-# LANGUAGE ImportQualifiedPost #-}"
     "import Data.List"
     "import Data.Ord qualified as Ord (Down_|_)"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-7 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should-not (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "import Data.List"
     "import {-# SOURCE #-} qualified Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "import Data.List"
     "import {-# SOURCE #-} Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-qualify-import-7a ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-ext-tracking-mode +1)
        (should-not (haskell-ext-tracking-have-import-qualified-post?))
        (haskell-qualify-import))
    (tests-utils--multiline
     "import Data.List"
     "import {-# SOURCE #-} Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")
    (tests-utils--multiline
     "import Data.List"
     "import {-# SOURCE #-} qualified Data.Ord as Ord (Down_|_)"
     "import Data.Set (Set)")))

(ert-deftest haskell-tests/haskell-back-up-indent-level-1 ()
  (haskell-tests--test-buffer-contents
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
     "  bar 10 ")))

(ert-deftest haskell-tests/haskell-back-up-indent-level-2 ()
  (haskell-tests--test-buffer-contents
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
     "  bar 10 ")))

(ert-deftest haskell-tests/haskell-back-up-indent-level-3 ()
  (haskell-tests--test-buffer-contents
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
     "  bar 10 ")))

(ert-deftest haskell-tests/haskell-back-up-indent-level-4 ()
  (haskell-tests--test-buffer-contents
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
     "  bar 10 ")))

(ert-deftest haskell-tests/haskell-back-up-indent-level-5 ()
  (haskell-tests--test-buffer-contents
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
     "  bar 10 ")))

(ert-deftest haskell-tests/haskell-back-up-indent-level-6 ()
  (haskell-tests--test-buffer-contents
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
     "  bar 10 ")))

(ert-deftest haskell-tests/haskell-back-up-indent-level-6 ()
  (haskell-tests--test-buffer-contents
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
     "  bar 10 ")))

(haskell-tests--make-multiple-test-buffer-contents
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

(haskell-tests--make-multiple-test-buffer-contents
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

(ert-deftest haskell-tests/haskell-format--get-language-extensions-1 ()
  (should
   (null
    (haskell-tests--with-temp-buffer
        (haskell-format--get-language-extensions (current-buffer) t)
      (tests-utils--multiline
       "_|_")))))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-2 ()
  (should
   (null
    (haskell-tests--with-temp-buffer
        (haskell-format--get-language-extensions (current-buffer) t)
      (tests-utils--multiline
       "module Foo where"
       "_|_")))))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-3 ()
  (should
   (equal '("RankNTypes")
          (haskell-tests--with-temp-buffer
              (haskell-format--get-language-extensions (current-buffer) t)
            (tests-utils--multiline
             "{-#language RankNTypes#-}"
             "module Foo where"
             "_|_")))))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-4 ()
  (should
   (equal '("RankNTypes")
          (haskell-tests--with-temp-buffer
              (haskell-format--get-language-extensions (current-buffer) t)
            (tests-utils--multiline
             "{-#lAnGuAgE RankNTypes#-}"
             "module Foo where"
             "_|_")))))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-5 ()
  (should
   (equal '("RankNTypes")
          (haskell-tests--with-temp-buffer
              (haskell-format--get-language-extensions (current-buffer) t)
            (tests-utils--multiline
             "{-#      lAnGuAgE   "
             "RankNTypes"
             "#-}"
             "module Foo where"
             "_|_")))))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-6 ()
  (should
   (equal '("RankNTypes" "UndecidableInstances")
          (haskell-tests--with-temp-buffer
              (haskell-format--get-language-extensions (current-buffer) t)
            (tests-utils--multiline
             "{-#      lAnGuAgE   "
             "RankNTypes, UndecidableInstances"
             "#-}"
             "module Foo where"
             "_|_")))))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-7 ()
  (should
   (equal '("RankNTypes" "UndecidableInstances")
          (haskell-tests--with-temp-buffer
              (haskell-format--get-language-extensions (current-buffer) t)
            (tests-utils--multiline
             "{-#      lAnGuAgE   "
             "\tRankNTypes \t "
             " , \t\t  "
             "  UndecidableInstances\t"
             "#-}"
             "module Foo where"
             "_|_")))))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-8 ()
  (should
   (equal '("LambdaCase" "RankNTypes" "UndecidableInstances")
          (haskell-tests--with-temp-buffer
              (haskell-format--get-language-extensions (current-buffer) t)
            (tests-utils--multiline
             ""
             "{-#      lAnGuAgE   "
             "\tRankNTypes \t "
             " , \t\t  "
             "  Undecidabl_|_eInstances\t"
             "#-}"
             "{-# LANGUAGE LambdaCase #-}"
             "module Foo where"
             "")))))


(ert-deftest haskell-tests/dante--insert-type--1 ()
  (haskell-tests--test-buffer-contents
      (dante--insert-type "foo :: Int -> Int -> Int")
    (tests-utils--multiline
     ""
     "_|_foo x y = x + y"
     "")
    (tests-utils--multiline
     ""
     "foo :: Int -> Int -> Int"
     "_|_foo x y = x + y"
     "")))

(ert-deftest haskell-tests/dante--insert-type--2 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/dante--insert-type--3 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/dante--insert-type--4 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/dante--insert-type--5 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/dante--insert-type--6 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/dante--insert-type--7 ()
  (haskell-tests--test-buffer-contents
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
     "")))

(ert-deftest haskell-tests/dante--insert-type--8 ()
  (haskell-tests--test-buffer-contents
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
     "")))


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


(ert-deftest haskell-tests/haskell-smart-operators--open-paren-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-open-paren)
    (tests-utils--multiline
     ""
     "foo x xs = foo (Median3or5_|_)"
     "")
    (tests-utils--multiline
     ""
     "foo x xs = foo (Median3or5 (_|_))"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--open-paren-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-open-paren)
    (tests-utils--multiline
     ""
     "foo x xs = foo (Median3or5 @_|_)"
     "")
    (tests-utils--multiline
     ""
     "foo x xs = foo (Median3or5 @(_|_))"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--open-bracket-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-open-bracket)
    (tests-utils--multiline
     ""
     "foo x xs = foo (Median3or5_|_)"
     "")
    (tests-utils--multiline
     ""
     "foo x xs = foo (Median3or5 [_|_])"
     "")))

(ert-deftest haskell-tests/haskell-smart-operators--open-bracket-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-open-bracket)
    (tests-utils--multiline
     ""
     "foo x xs = foo (Median3or5 @_|_)"
     "")
    (tests-utils--multiline
     ""
     "foo x xs = foo (Median3or5 @[_|_])"
     "")))


(defconst haskell-tests/cabal-test-data
  (concat +emacs-config-path+ "/tests/test-data/cabal"))

(ert-deftest haskell-tests/haskell-misc--configure-dante--find-cabal-component-for-file-1 ()
  (let* ((cabal-file (expand-file-name (concat haskell-tests/cabal-test-data "/test-components.cabal")))
         (config (flycheck-haskell-read-cabal-configuration cabal-file nil))
         (components (cdr (assq 'components config)))
         (root "/home/user/proj1")
         (results nil))
    (should-not (null components))
    (dolist (entry '(("src/Foo.hs" "lib:test-components" )
                     ("src/Bar.hs" "lib:test-components" )
                     ("src/Baz/Quux.hs" "lib:test-components" )
                     ("src/Frob/Decombobulate.hs" "lib:test-components" )
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
                   expected-component)))))))

;; (ert "haskell-tests/.*")

;; (setf haskell-tests/tests
;;       '(haskell-tests/abbrev+-extract-module-name))

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
