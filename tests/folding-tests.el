;; folding-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 24 September 2025
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'cl))

(require 'dante)
(require 'alex-mode)
(require 'happy-mode)
(require 'haskell-abbrev+)
(require 'haskell-block-indent)
(require 'haskell-format-setup)
(require 'haskell-misc)
(require 'haskell-regexen)
(require 'haskell-smart-operators-mode)
(require 'haskell-sort-imports)

(require 'common)
(require 'ert)
(require 'search)
(require 'tests-utils)

(cl-defmacro folding-tests--haskell--test-buffer-contents*
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
            :collect-actual-contents
            (progn
              ;; We already have cursor inserted
              (goto-char (point-min))
              (let ((invis-start nil)
                    (invis-end nil)
                    (invis-chunks nil))
                (while (not (eobp))
                  (if (invisible-p (point))
                      (if invis-start
                          (setf invis-end (point))
                        (setf invis-start (point)))
                    (when invis-start
                      (push (cons invis-start invis-end) invis-chunks)
                      (setf invis-start nil
                            invis-end nil)))
                  (forward-char))
                (when invis-start
                  (push (cons invis-start invis-end) invis-chunks))
                ;; Delete from end to start so that no adjustments will be needed.
                ;;
                ;; Increment range end by 1 to definitely get the last character.
                ;; Emacs more or less behavies like 1 was added so do that
                ;; in order to reduceit confusion.
                (dolist (chunk invis-chunks)
                  (delete-region (car chunk) (+ 1 (cdr chunk)))))
              (buffer-substring-no-properties (point-min) (point-max)))
            ;; Use fresh buffers.
            :buffer-id nil)))))

(folding-tests--haskell--test-buffer-contents*
 :name folding-tests/haskell-hideshow-1a
 :action
 (vim:folding-hide-indented-or-sexp-or-commented:wrapper)
 :contents
 (tests-utils--multiline
  ""
  "#ifdef_|_ FOO"
  "import Data.Text qualified as T"
  "import Data.Bimap (Bimap)"
  "import Data.Bifunctors"
  "import qualified Data.Bimap as BM"
  "#endif"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "#ifdef_|_ FOO#endif"
  ""))

(folding-tests--haskell--test-buffer-contents*
 :name folding-tests/haskell-hideshow-1b
 :action
 (vim:folding-hide-indented-or-sexp-or-commented:wrapper)
 :contents
 (tests-utils--multiline
  ""
  "#ifdef_|_ FOO"
  "import Data.Text qualified as T"
  "import Data.Bimap (Bimap)"
  "#elif"
  "import Data.Bifunctors"
  "import qualified Data.Bimap as BM"
  "#endif"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "#ifdef_|_ FOO#elif"
  "import Data.Bifunctors"
  "import qualified Data.Bimap as BM"
  "#endif"
  ""))

(folding-tests--haskell--test-buffer-contents*
 :name folding-tests/haskell-hideshow-2
 :action
 (vim:folding-hide-indented-or-sexp-or-commented:wrapper)
 :contents
 (tests-utils--multiline
  ""
  "-- _|_foo"
  "-- bar"
  "-- baz"
  "-- quux"
  "foo :: a -> a"
  "foo = id"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "-- _|_foo"
  "foo :: a -> a"
  "foo = id"
  ""))

(folding-tests--haskell--test-buffer-contents*
 :name folding-tests/haskell-hideshow-3
 :action
 (vim:folding-hide-indented-or-sexp-or-commented:wrapper)
 :contents
 (tests-utils--multiline
  ""
  "foo :: a -> a"
  "foo = id"
  "  wh_|_ere"
  "    x = 1"
  "    y = 2"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo :: a -> a"
  "foo = id"
  "  wh_|_ere"
  ""))

(folding-tests--haskell--test-buffer-contents*
 :name folding-tests/haskell-hideshow-4
 :action
 (vim:folding-hide-indented-or-sexp-or-commented:wrapper)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  [ _|_bar"
  "  , baz"
  "  , quux"
  "  ]"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo ="
  "  [_|_ bar]"
  ""))

(folding-tests--haskell--test-buffer-contents*
 :name folding-tests/haskell-hideshow-5
 :action
 (vim:folding-hide-indented-or-sexp-or-commented:wrapper)
 :contents
 (tests-utils--multiline
  ""
  "foo ="
  "  ( _|_bar"
  "  , baz"
  "  , quux"
  "  )"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo ="
  "  (_|_ bar)"
  ""))

(folding-tests--haskell--test-buffer-contents*
 :name folding-tests/haskell-hideshow-6
 :action
 (vim:folding-hide-indented-or-sexp-or-commented:wrapper)
 :contents
 (tests-utils--multiline
  ""
  "foo = Foo"
  "  { _|_bar = 1"
  "  , baz = 2"
  ""
  "  , quux = 3"
  "  }"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo = Foo"
  "  {_|_ bar = 1}"
  ""))

(folding-tests--haskell--test-buffer-contents*
 :name folding-tests/haskell-hideshow-7
 :action
 (vim:folding-hide-indented-or-sexp-or-commented:wrapper)
 :contents
 (tests-utils--multiline
  ""
  "foo _|_= Foo"
  ""
  "  { bar = 1"
  ""
  "  , baz = 2"
  "            "
  "  , quux = 3"
  ""
  "  }"
  "")
 :expected-value
 (tests-utils--multiline
  ""
  "foo _|_= Foo"
  ""))

(provide 'folding-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; folding-tests.el ends here
