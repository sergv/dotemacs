;; haskell-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 26 November 2013
;; Description:

(eval-when-compile (require 'cl-lib))

(require 'haskell-abbrev+)
(require 'haskell-format-setup)
(require 'haskell-misc)
(require 'haskell-smart-operators-mode)
(require 'common)
(require 'ert)

(require 'shm)

(ert-deftest haskell-tests/abbrev+-extract-module-name ()
  (should (string= (haskell-abbrev+-extract-mod-name "Foo.Bar")
                   "Bar"))
  (should (string= (haskell-abbrev+-extract-mod-name "Foo.Bar.Baz")
                   "Baz"))
  (should (string= (haskell-abbrev+-extract-mod-name "Foo'.Bar2.Baz_3.Quux")
                   "Quux")))

(defun haskell-tests--multiline (&rest lines)
  (mapconcat #'identity lines "\n"))

(defvar haskell-tests--temp-buffer nil)

(defmacro haskell-tests--with-temp-buffer (action contents)
  (declare (indent 1))
  `(save-match-data
     (unless haskell-tests--temp-buffer
       (setf haskell-tests--temp-buffer (get-buffer-create " haskell-tests-buffer"))
       (with-current-buffer haskell-tests--temp-buffer
         (haskell-mode)))
     (with-current-buffer haskell-tests--temp-buffer
       (erase-buffer)
       (insert ,contents)
       (goto-char (point-min))
       (if (re-search-forward "_|_" nil t)
           (replace-match "")
         (error "No _|_ marker for point position within contents:\n%s" ,contents))
       (font-lock-fontify-buffer)
       ;; Refresh shm mode so that it resets its caches and parses buffer from
       ;; scratch.
       (structured-haskell-mode -1)
       (structured-haskell-mode +1)
       ,action)))

(defmacro haskell-tests--test-buffer-contents (action contents expected-value)
  (declare (indent 1))
  `(haskell-tests--with-temp-buffer
       (progn
         ,action
         (should (equal (buffer-substring-no-properties (point-min) (point-max))
                        ,expected-value)))
     ,contents))

(defmacro* haskell-tests--test-result (&key action expected-value contents)
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
    (haskell-tests--multiline
     ""
     ""
     "{-# language"
     "             Safe, FlexibleContexts _|_ #-}"
     "")
    (haskell-tests--multiline
     ""
     ""
     "{-# LANGUAGE FlexibleContexts #-}"
     "{-# LANGUAGE Safe             #-}"
     "")))

(ert-deftest haskell-tests/haskell-align-language-pragmas-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-align-language-pragmas (point))
    (haskell-tests--multiline
     ""
     "-- foobar"
     "{-# language"
     "             Safe, FlexibleContexts _|_ #-}"
     "")
    (haskell-tests--multiline
     ""
     "-- foobar"
     "{-# LANGUAGE FlexibleContexts #-}"
     "{-# LANGUAGE Safe             #-}"
     "")))

(ert-deftest haskell-tests/haskell-align-language-pragmas-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-align-language-pragmas (point))
    (haskell-tests--multiline
     ""
     ""
     "{-# language"
     "   Safe _|_"
     " , FlexibleContexts"
     " #-}"
     "")
    (haskell-tests--multiline
     ""
     ""
     "{-# LANGUAGE FlexibleContexts #-}"
     "{-# LANGUAGE Safe             #-}"
     "")))

(ert-deftest haskell-tests/haskell-align-language-pragmas-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-align-language-pragmas (point))
    (haskell-tests--multiline
     ""
     "-- foo"
     "{-# language"
     "   Safe _|_"
     " , FlexibleContexts"
     " #-}"
     "-- bar")
    (haskell-tests--multiline
     ""
     "-- foo"
     "{-# LANGUAGE FlexibleContexts #-}"
     "{-# LANGUAGE Safe             #-}"
     "-- bar")))

(ert-deftest haskell-tests/haskell-align-language-pragmas-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-align-language-pragmas (point))
    (haskell-tests--multiline
     "{-# LANGUAGE Safe #-}"
     "{-# LANGUAGE AlternativeLayoutRule _|_ #-}"
     "{-# LANGUAGE AllowAmbiguousTypes   #-}")
    (haskell-tests--multiline
     "{-# LANGUAGE AllowAmbiguousTypes   #-}"
     "{-# LANGUAGE AlternativeLayoutRule #-}"
     "{-# LANGUAGE Safe                  #-}")))


(ert-deftest haskell-tests/haskell-format--get-language-extensions-1 ()
  (haskell-tests--test-result
    :action
    (haskell-format--get-language-extensions (current-buffer) t)
    :expected-value
    '("Safe" "AlternativeLayoutRule" "AllowAmbiguousTypes" "FlexibleContexts")
    :contents
    (haskell-tests--multiline
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
    (haskell-tests--multiline
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
    (haskell-tests--multiline
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
    (haskell-tests--multiline
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
    (haskell-tests--multiline
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
    "x = 1 ++ 2"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 +           _|_ 2"
    "x = 1 ++ 2"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
    "x = 1 +           _|_"
    "x = 1 +* "))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1_|_"
    "x = 1 + "))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-5 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1 _|_"
    "x = 1 + "))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-6 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = 1  _|_"
    "x = 1  + "))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-7 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = f \(_|_"
    "x = f \(+ "))

(ert-deftest haskell-tests/haskell-smart-operators--sections-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = f \(_|_\)"
    "x = f \(+\)"))

(ert-deftest haskell-tests/haskell-smart-operators--sections-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "x = f \(     _|_\)"
    "x = f \(+\)"))

(ert-deftest haskell-tests/haskell-smart-operators--space-after--lambdas-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?\\)
    "x = \(_|_\)"
    "x = \(\\\)"))

(ert-deftest haskell-tests/haskell-smart-operators--space-after--lambdas-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?$)
    "x = f _|_\\ x -> x"
    "x = f $ \\ x -> x"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "x = \[_|_"
    "x = \[| "))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "x = \[| foobar _|_\]"
    "x = \[| foobar |\]"))

(ert-deftest haskell-tests/haskell-smart-operators--beginning-of-buffer ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
    "_|_ + bar"
    " * + bar"))

(ert-deftest haskell-tests/haskell-smart-operators--end-of-buffer ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
    "+ bar_|_"
    "+ bar * "))


(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "--_|_ foobar"
    "-- | foobar"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    "--_|_ foobar"
    "-- ^ foobar"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "--     _|_ foobar"
    "-- | foobar"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    "--      _|_ foobar"
    "-- ^ foobar"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-no-action-if-not-toplevel-comment-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  --      _|_ foobar"
     "  pure baz")
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  --      ^ foobar"
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-no-action-if-not-toplevel-comment-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  --      _|_ foobar"
     "  pure baz")
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  --      | foobar"
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-no-action-if-not-toplevel-comment-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  --      _|_ foobar"
     "  pure baz")
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  --      + foobar"
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "--    _|_"
    "-- | "))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    "--    _|_"
    "-- ^ "))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    "--    _|_"
    "--    +"))


(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-disabled-if-not-on-first-column-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    " --    _|_"
    " --    |"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-disabled-if-not-on-first-column-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    " --    _|_"
    " --    ^"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-insertion-disabled-if-not-on-first-column-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
    " --    _|_"
    " --    +"))


(ert-deftest haskell-tests/haskell-smart-operators--almost-haddock-comments-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
    "-       _|_"
    "-| "))

(ert-deftest haskell-tests/haskell-smart-operators--almost-haddock-comments-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
    "-      _|_"
    "-^ "))


(ert-deftest haskell-tests/haskell-smart-operators--operator-$-1 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f _|_(xs ++ ys)"
    "x = f $ xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-2 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f_|_ (xs ++ ys)"
    "x = f $ xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-3 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f _|_ (xs ++ ys)"
    "x = f $ xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-4 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f _|_     (xs ++ ys)"
    "x = f $ xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-5 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f _|_(xs ++ ys)"
    "x = f $ xs ++ ys"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-6 ()
  (haskell-tests--test-buffer-contents
      (haskell-smart-operators-$)
    "x = f_|_(xs ++ ys)"
    "x = f $ xs ++ ys"))

(ert-deftest haskell-tests/shm/!-1 ()
  (haskell-tests--test-buffer-contents
      (shm/!)
    (haskell-tests--multiline
     "data Foo = Foo"
     "  { foo ::_|_ Set Int"
     "  , bar :: Map Int Double"
     "  }")
    (haskell-tests--multiline
     "data Foo = Foo"
     "  { foo :: !(Set Int)"
     "  , bar :: Map Int Double"
     "  }")))

(ert-deftest haskell-tests/shm/!-2 ()
  (haskell-tests--test-buffer-contents
      (shm/!)
    (haskell-tests--multiline
     "data Foo = Foo"
     "  { foo :: _|_Set Int"
     "  , bar :: Map Int Double"
     "  }")
    (haskell-tests--multiline
     "data Foo = Foo"
     "  { foo :: !(Set Int)"
     "  , bar :: Map Int Double"
     "  }")))

(ert-deftest haskell-tests/shm/!-3 ()
  (haskell-tests--test-buffer-contents
      (shm/!)
    (haskell-tests--multiline
     "data Foo = Foo"
     "  { foo :: _|_ Set Int"
     "  , bar :: Map Int Double"
     "  }")
    (haskell-tests--multiline
     "data Foo = Foo"
     "  { foo :: !(Set Int)"
     "  , bar :: Map Int Double"
     "  }")))

(ert-deftest haskell-tests/shm/!-4 ()
  (haskell-tests--test-buffer-contents
      (shm/!)
    (haskell-tests--multiline
     "data Foo = Foo"
     "  { foo ::      _|_      Set Int"
     "  , bar :: Map Int Double"
     "  }")
    (haskell-tests--multiline
     "data Foo = Foo"
     "  { foo :: !(Set Int)"
     "  , bar :: Map Int Double"
     "  }")))

(ert-deftest haskell-tests/haskell-smart-operators--arrows-in-non-haddock-comment-1 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?-)
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
        (insert "value2"))
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  putDocLn $ ppDict \"foobar\""
     "    [ \"label1\" --> value1"
     "    , \"label2\" _|_"
     "    , \"label3\" --> value3"
     "    ]"
     "  pure baz")
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  putDocLn $ ppDict \"foobar\""
     "    [ \"label1\" --> value1"
     "    , \"label2\" --> value2"
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
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  putDocLn $ ppDict \"foobar\""
     "    [ \"label1\" --> value1"
     "    , \"label2\" _|_"
     "    , \"label3\" --> value3"
     "    ]"
     "  pure baz")
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  putDocLn $ ppDict \"foobar\""
     "    [ \"label1\" --> value1"
     "    , \"label2\" |-> value2"
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
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  putDocLn $ ppDict \"foobar\""
     "    [ \"label1\" --> value1"
     "    , \"label2\" _|_"
     "    , \"label3\" --> value3"
     "    ]"
     "  pure baz")
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  putDocLn $ ppDict \"foobar\""
     "    [ \"label1\" --> value1"
     "    , \"label2\" ---> value2"
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
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  putDocLn $ ppDict \"foobar\""
     "    [ \"label1\" --> value1"
     "    , \"label2\" _|_"
     "    , \"label3\" --> value3"
     "    ]"
     "  pure baz")
    (haskell-tests--multiline
     "foo = do"
     "  bar"
     "  putDocLn $ ppDict \"foobar\""
     "    [ \"label1\" --> value1"
     "    , \"label2\" --->>> value2"
     "    , \"label3\" --> value3"
     "    ]"
     "  pure baz")))

(ert-deftest haskell-tests/haskell-smart-operators--arrows-in-haddock-comment-1 ()
  (haskell-tests--test-buffer-contents
      (progn
        (haskell-smart-operators--insert-char-surrounding-with-spaces ?>)
        (insert "test"))
    "-- _|_"
    "-- >test"))

;; (ert "haskell-tests/.*")

;; (setf haskell-tests/tests
;;       '(haskell-tests/abbrev+-extract-module-name))
;;
;; (let ((ert-debug-on-error nil))
;;   (eproj-reset-projects)
;;   (ert (join-lines (-map (comp #'regexp-quote #'symbol->string)
;;                          haskell-tests/tests)
;;                    "\\|")
;;        ;; "haskell-tests/.*"
;;        )
;;   nil)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; haskell-tests.el ends here
