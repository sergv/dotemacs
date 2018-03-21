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

(defmacro haskell-tests--with-temp-buffer (action contents)
  (declare (indent 1))
  `(save-match-data
     (with-temp-buffer
       (insert ,contents)
       (goto-char (point-min))
       (when (re-search-forward "_|_" nil t)
         (replace-match ""))
       (haskell-mode)
       (font-lock-fontify-buffer)
       ,action)))

(defmacro haskell-tests--test-buffer-contents (action contents expected)
  (declare (indent 1))
  `(haskell-tests--with-temp-buffer
       (progn
         ,action
         (should (equal (buffer-substring-no-properties (point-min) (point-max))
                        ,expected)))
     ,contents))

(defmacro haskell-tests--test-result (action expected-value contents)
  (declare (indent 2))
  `(haskell-tests--with-temp-buffer
       (should (equal ,action ,expected-value))
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
      (haskell-format--get-language-extensions (current-buffer) t)
      '("Safe" "AlternativeLayoutRule" "AllowAmbiguousTypes" "FlexibleContexts")
    (haskell-tests--multiline
     ""
     "{-# LANGUAGE Safe #-}"
     "{-#LANGUAGE AlternativeLayoutRule #-}"
     "{-# LANGUAGE AllowAmbiguousTypes#-}"
     "{-#LANGUAGE FlexibleContexts#-}")))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-2 ()
  (haskell-tests--test-result
      (haskell-format--get-language-extensions (current-buffer) t)
      '("Safe" "AlternativeLayoutRule" "AllowAmbiguousTypes" "FlexibleContexts")
    (haskell-tests--multiline
     ""
     "{-# LANGUAGE Safe,AlternativeLayoutRule, AllowAmbiguousTypes,"
     "FlexibleContexts #-}")))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-3 ()
  (haskell-tests--test-result
      (haskell-format--get-language-extensions (current-buffer) t)
      '("Safe" "AlternativeLayoutRule" "AllowAmbiguousTypes" "FlexibleContexts")
    (haskell-tests--multiline
     ""
     "{-# language Safe #-}"
     "{-#language AlternativeLayoutRule #-}"
     "{-# language AllowAmbiguousTypes#-}"
     "{-#language FlexibleContexts#-}")))

(ert-deftest haskell-tests/haskell-format--get-language-extensions-4 ()
  (haskell-tests--test-result
      (haskell-format--get-language-extensions (current-buffer) t)
      '("Safe" "AlternativeLayoutRule" "AllowAmbiguousTypes" "FlexibleInstances" "FlexibleContexts")
    (haskell-tests--multiline
     ""
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
      (haskell-format--get-language-extensions (current-buffer) t)
      '("FlexibleContexts" "FlexibleInstances" "RecordWildCards" "AllowAmbiguousTypes")
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
     ""
     "foo Frob{..} ="
     " frob1 * 2"
     "")))

(ert-deftest haskell-tests/forward-haskell-symbol-1 ()
  (haskell-tests--test-result
      (list
       (bounds-of-thing-at-point 'haskell-symbol)
       (thing-at-point 'haskell-symbol))
      (list
       (cons 2 9)
       "fooobar")
    " fooo_|_bar "))

(ert-deftest haskell-tests/forward-haskell-symbol-2 ()
  (haskell-tests--test-result
      (list
       (bounds-of-thing-at-point 'haskell-symbol)
       (thing-at-point 'haskell-symbol))
      (list
       (cons 2 9)
       "Fooobar")
    " Fooo_|_bar "))

(ert-deftest haskell-tests/forward-haskell-symbol-3 ()
  (haskell-tests--test-result
      (list
       (bounds-of-thing-at-point 'haskell-symbol)
       (thing-at-point 'haskell-symbol))
      (list
       (cons 3 10)
       "Fooobar")
    " 'Fooo_|_bar "))

(ert-deftest haskell-tests/forward-haskell-symbol-4 ()
  (haskell-tests--test-result
      (list
       (bounds-of-thing-at-point 'haskell-symbol)
       (thing-at-point 'haskell-symbol))
      (list
       (cons 4 11)
       "Fooobar")
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
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
     (buffer-string))
   "x = 1 ++ 2"
   "x = 1 +_|_2"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-2 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
     (buffer-string))
   "x = 1 ++ 2"
   "x = 1 +           _|_ 2"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-3 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
     (buffer-string))
   "x = 1 +* "
   "x = 1 +           _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-4 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
     (buffer-string))
   "x = 1 + "
   "x = 1_|_"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-5 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
     (buffer-string))
   "x = 1 + "
   "x = 1 _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-6 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
     (buffer-string))
   "x = 1  + "
   "x = 1  _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--prepend-to-prev-operator-7 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
     (buffer-string))
   "x = f (+ "
   "x = f \(_|_"))

(ert-deftest haskell-tests/haskell-smart-operators--sections-1 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
     (buffer-string))
   "x = f (+)"
   "x = f (_|_)"))

(ert-deftest haskell-tests/haskell-smart-operators--sections-2 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
     (buffer-string))
   "x = f (+)"
   "x = f (     _|_)"))

(ert-deftest haskell-tests/haskell-smart-operators--space-after--lambdas-1 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?\\)
     (buffer-string))
   "x = (\\)"
   "x = (_|_)"))

(ert-deftest haskell-tests/haskell-smart-operators--space-after--lambdas-2 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?$)
     (buffer-string))
   "x = f $ \\ x -> x"
   "x = f _|_\\ x -> x"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-1 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
     (buffer-string))
   "x = [| "
   "x = [_|_"))

(ert-deftest haskell-tests/haskell-smart-operators--oxford-brackets-2 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
     (buffer-string))
   "x = [| foobar |]"
   "x = [| foobar _|_]"))

(ert-deftest haskell-tests/haskell-smart-operators--beginning-of-buffer ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
     (buffer-string))
   "* + bar"
   "_|_ + bar"))

(ert-deftest haskell-tests/haskell-smart-operators--end-of-buffer ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?*)
     (buffer-string))
   "+ bar * "
   "+ bar_|_"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-1 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
     (buffer-string))
   "--| foobar"
   "--_|_ foobar"))

(ert-deftest haskell-tests/haskell-smart-operators--haddock-comments-2 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
     (buffer-string))
   "--^ foobar"
   "--_|_ foobar"))

(ert-deftest haskell-tests/haskell-smart-operators--comments-literal-insertion-1 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
     (buffer-string))
   "--    |"
   "--    _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--comments-literal-insertion-2 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
     (buffer-string))
   "--    ^"
   "--    _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--comments-literal-insertion-3 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?+)
     (buffer-string))
   "--    +"
   "--    _|_"))


(ert-deftest haskell-tests/haskell-smart-operators--almost-haddock-comments-1 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?|)
     (buffer-string))
   "-| "
   "-       _|_"))

(ert-deftest haskell-tests/haskell-smart-operators--almost-haddock-comments-2 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators--insert-char-surrounding-with-spaces ?^)
     (buffer-string))
   "-^ "
   "-      _|_"))


(ert-deftest haskell-tests/haskell-smart-operators--operator-$-1 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators-$)
     (buffer-string))
   "x = f $ xs ++ ys"
   "x = f _|_(xs ++ ys)"))

(ert-deftest haskell-tests/haskell-smart-operators--operator-$-2 ()
  (haskell-tests--test-result
   (progn
     (haskell-smart-operators-$)
     (buffer-string))
   "x = f $ xs ++ ys"
   "x = f_|_ (xs ++ ys)"))

(ert-deftest haskell-tests/shm/!-1 ()
  (haskell-tests--test-result
   (progn
     (shm/!)
     (buffer-substring-no-properties (point-min) (point-max)))
   (haskell-tests--multiline
    "data Foo = Foo"
    "  { foo :: !(Set Int)"
    "  , bar :: Map Int Double"
    "  }")
   (haskell-tests--multiline
    "data Foo = Foo"
    "  { foo ::_|_ Set Int"
    "  , bar :: Map Int Double"
    "  }")))

(ert-deftest haskell-tests/shm/!-2 ()
  (haskell-tests--test-result
   (progn
     (shm/!)
     (buffer-substring-no-properties (point-min) (point-max)))
   (haskell-tests--multiline
    "data Foo = Foo"
    "  { foo :: !(Set Int)"
    "  , bar :: Map Int Double"
    "  }")
   (haskell-tests--multiline
    "data Foo = Foo"
    "  { foo :: _|_Set Int"
    "  , bar :: Map Int Double"
    "  }")))
;; (ert "haskell-tests/.*")

(ert-deftest haskell-tests/shm/!-3 ()
  (haskell-tests--test-result
   (progn
     (shm/!)
     (buffer-substring-no-properties (point-min) (point-max)))
   (haskell-tests--multiline
    "data Foo = Foo"
    "  { foo :: !(Set Int)"
    "  , bar :: Map Int Double"
    "  }")
   (haskell-tests--multiline
    "data Foo = Foo"
    "  { foo :: _|_ Set Int"
    "  , bar :: Map Int Double"
    "  }")))

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
