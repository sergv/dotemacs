;; fontification-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: 25 May 2024
;; Description:

(eval-when-compile
  (require 'cl-lib)
  (require 'cl))

(require 'common)
(require 'ert)
(require 'tests-utils)

;; '(haskell-ts-mode)

(cl-defmacro fontification-tests--test-ts-fontification (name &key contents fontification fresh-buffer modes)
  (declare (indent 1))
  `(progn
     ,@(cl-loop
        for mode in modes
        collect
        `(ert-deftest ,(string->symbol (format "%s/%s" name mode)) ()
           (tests-utils--with-temp-buffer
            :action
            (fontification-tests--check-properties ',fontification)
            :suppress-cursor t
            :contents ,contents
            :initialisation (,mode)
            :buffer-id ,(if fresh-buffer nil (string->symbol (format "fontification-tests-%s" mode))))))))

(defun fontification-tests--check-properties (props)
  "Check if syntax properties and font-lock properties as set properly.

LINES is a list of strings that will be inserted to a new
buffer. Then PROPS is a list of tripples of (string syntax
face). String is searched for in the buffer and then is checked
if all of its characters have syntax and face. See
`check-syntax-and-face-match-range`."
  (goto-char (point-min))
  (dolist (prop props)
    (cl-destructuring-bind (string face) prop
      (let ((case-fold-search nil))
        (search-forward string))
      (fontification-tests--check-syntax-and-face-match-range (match-beginning 0) (match-end 0) nil face))))

(defun fontification-tests--check-syntax-and-face-match-range (beg end syntax face)
  "Check if all charaters between positions BEG and END have
syntax set to SYNTAX and face set to FACE.

If SYNTAX or FACE are set to t then any syntex respective face is
not checked."
  (let (actual-syntaxes
        actual-faces
        (syntax-classes "-.w_()'\"$\\/<>@!|")
        (text (buffer-substring-no-properties beg end)))
    (while (< beg end)
      (when syntax
        (cl-pushnew (char-to-string (aref syntax-classes (syntax-class (syntax-after beg)))) actual-syntaxes :test #'equal))
      (cl-pushnew (get-text-property beg 'face) actual-faces :test #'equal)
      (setq beg (1+ beg)))
    (when syntax
      (should (equal (list text (mapconcat #'identity (sort (mapcar (lambda (syn) (char-to-string syn)) syntax) #'string<) ""))
                     (list text (mapconcat #'identity (sort actual-syntaxes #'string<) "")))))
    (unless (eq face t)
      (should (equal (list text (list face))
                     (list text actual-faces))))))

(fontification-tests--test-ts-fontification
    json-ts-mode/fontification-1
  :modes (json-ts-mode)
  :contents
  (tests-utils--multiline
   "{"
   "  \"signatures\": ["
   "    {"
   "      \"keyid\": \"foo\","
   "      \"method\": \"ed25519\","
   "      \"sig\": \"bar\""
   "    }"
   "  ],"
   "  \"signed\": {"
   "    \"_type\": \"Timestamp\","
   "    \"expires\": \"2024-05-21T20:19:41Z\","
   "    \"meta\": {"
   "      \"<repo>/snapshot.json\": {"
   "        \"hashes\": {"
   "          \"md5\": true,"
   "          \"sha256\": null"
   "        },"
   "        \"length\": false"
   "      }"
   "    },"
   "    \"version\": 95848"
   "  }"
   "}")
  :fontification
  (("\"signatures\""           json-mode-object-name-face)
   ("\"keyid\""                json-mode-object-name-face)
   ("\"foo\""                  font-lock-string-face)
   ("\"method\""               json-mode-object-name-face)
   ("\"ed25519\""              font-lock-string-face)
   ("\"sig\""                  json-mode-object-name-face)
   ("\"bar\""                  font-lock-string-face)
   ("\"signed\""               json-mode-object-name-face)
   ("\"_type\""                json-mode-object-name-face)
   ("\"Timestamp\""            font-lock-string-face)
   ("\"<repo>/snapshot.json\"" json-mode-object-name-face)
   ("\"hashes\""               json-mode-object-name-face)
   ("\"md5\""                  json-mode-object-name-face)
   ("true"                     font-lock-keyword-face)
   ("\"sha256\""               json-mode-object-name-face)
   ("null"                     font-lock-keyword-face)
   ("\"length\""               json-mode-object-name-face)
   ("false"                    font-lock-keyword-face)
   ("\"version\""              json-mode-object-name-face)
   ("95848"                    font-lock-constant-face))
  :fresh-buffer t)

(fontification-tests--test-ts-fontification
    haskell-ts-mode/fontification-1
  :modes (haskell-ts-mode)
  :contents
  (tests-utils--multiline
   ""
   "isWord :: _ Int# -> Bool#"
   "isWord x = case chr# x of"
   "  '/'#  -> False#"
   "  '\\\\'# -> False#"
   "  _     -> _ True#"
   "")
  :fontification
  (("::"      haskell-operator-face)
   ("_"       haskell-keyword-face)
   ("Int#"    haskell-type-face)
   ("->"      haskell-operator-face)
   ("Bool#"   haskell-type-face)
   ("case"    haskell-keyword-face)
   ("of"      haskell-keyword-face)
   ("'/'#"    font-lock-string-face)
   ("->"      haskell-operator-face)
   ("False#"  haskell-constructor-face)
   ("'\\\\'#" font-lock-string-face)
   ("->"      haskell-operator-face)
   ("False#"  haskell-constructor-face)
   ("_"       haskell-keyword-face)
   ("->"      haskell-operator-face)
   ("_"       haskell-keyword-face)
   ("True#"   haskell-constructor-face))
  :fresh-buffer t)

(fontification-tests--test-ts-fontification
    haskell-ts-mode/fontification-2
  :modes (haskell-ts-mode)
  :contents
  (tests-utils--multiline
   ""
   "test :: forall a. [a] -> [(a, a)]"
   "test !x@(y : ys) = do"
   "  y <- frobnicator"
   "  pure $ (x :: b) + 1 + x `foo` bar `Mod.foo3` baz"
   "")
  :fontification
  (("test"        nil)
   ("::"          haskell-operator-face)
   ("forall"      haskell-keyword-face)
   ("."           haskell-keyword-face)
   ("\["          nil)
   ("a"           nil)
   ("\]"          nil)
   ("->"          haskell-operator-face)
   ("\["          nil)
   ("\("          nil)
   ("a"           nil)
   (","           nil)
   ("a"           nil)
   ("\)"          nil)
   ("\]"          nil)
   ("!"           font-lock-negation-char-face)
   ("x"           nil)
   ("@"           haskell-operator-face)
   ("\("          nil)
   ("y"           nil)
   (":"           haskell-constructor-face)
   ("ys"          nil)
   ("\)"          nil)
   ("="           haskell-operator-face)
   ("do"          haskell-keyword-face)
   ("<-"          haskell-operator-face)
   ("frobnicator" nil)
   ("$"           haskell-operator-face)
   ("\("          nil)
   ("x"           nil)
   ("::"          haskell-operator-face)
   ("b"           nil)
   ("\)"          nil)
   ("+"           haskell-operator-face)
   ("1"           font-lock-constant-face)
   ("+"           haskell-operator-face)
   ("`foo`"       haskell-operator-face)
   ("`Mod.foo3`"  haskell-operator-face))
  :fresh-buffer t)

(fontification-tests--test-ts-fontification
    haskell-ts-mode/fontification-3
  :modes (haskell-ts-mode)
  :contents
  (tests-utils--multiline
   ""
   "data Foo a = Foo"
   "  { foo :: (Int `Bar` Quux `Mod.Bar2` Quux3)"
   "  , bar :: (a, Double)"
   "  , xxx :: (# a, Double #)"
   "  , baz :: {-# UNPACK #-} !Double"
   "  }"
   "")
  :fontification
  (("data"           haskell-keyword-face)
   ("Foo"            haskell-type-face)
   ("a"              nil)
   ("="              haskell-operator-face)
   ("Foo"            haskell-constructor-face)
   ("foo"            nil)
   ("::"             haskell-operator-face)
   ("\("             nil)
   ("Int"            haskell-type-face)
   ("`Bar`"          haskell-operator-face)
   ("Quux"           haskell-type-face)
   ("`Mod.Bar2`"     haskell-operator-face)
   ("Quux3"          haskell-type-face)
   ("\)"             nil)
   (","              nil)
   ("bar"            nil)
   ("::"             haskell-operator-face)
   ("\("             nil)
   ("a"              nil)
   (","              nil)
   ("Double"         haskell-type-face)
   ("\)"             nil)
   ("xxx"            nil)
   ("\(#"            nil)
   ("a"              nil)
   (","              nil)
   ("Double"         haskell-type-face)
   ("#\)"            nil)
   ("{-# UNPACK #-}" haskell-pragma-face)
   ("!"              font-lock-negation-char-face)
   ("Double"         haskell-type-face))
  :fresh-buffer t)

(fontification-tests--test-ts-fontification
    haskell-ts-mode/fontification-4
  :modes (haskell-ts-mode)
  :contents
  (tests-utils--multiline
   ""
   "test :: [Foo.Test a] -> [(a, a)]"
   "test ~(y : ys)"
   "  | checkLength (fmap (\\_ -> ()) ys) = z"
   "  | otherwise                        = []"
   "  where"
   "    z = Foo.Wrapped $ Foo.decombobulate y"
   "")
  :fontification
  (("test"              nil)
   ("::"                haskell-operator-face)
   ("\["                nil)
   ("Foo.Test"          haskell-type-face)
   ("a"                 nil)
   ("\]"                nil)
   ("~"                 font-lock-negation-char-face)
   (":"                 haskell-constructor-face)
   ("|"                 haskell-operator-face)
   ("checkLength"       nil)
   ("\\"                haskell-operator-face)
   ("_"                 haskell-keyword-face)
   ("->"                haskell-operator-face)
   ("()"                haskell-constructor-face)
   ("="                 haskell-operator-face)
   ("|"                 haskell-operator-face)
   ("="                 haskell-operator-face)
   ("[]"                haskell-constructor-face)
   ("where"             haskell-keyword-face)
   ("z"                 nil)
   ("="                 haskell-operator-face)
   ("Foo"               haskell-type-face)
   ("Wrapped"           haskell-constructor-face)
   ("$"                 haskell-operator-face)
   ("Foo.decombobulate" nil)
   ("y"                 nil))
  :fresh-buffer t)

(fontification-tests--test-ts-fontification
    haskell-ts-mode/fontification-5
  :modes (haskell-ts-mode)
  :contents
  (tests-utils--multiline
   ""
   "pattern Foo x = Cons x Nil"
   ""
   "test :: a -> a"
   "test pattern = pattern"
   "")
  :fontification
  (("pattern" haskell-keyword-face)
   ("Foo"     haskell-constructor-face)
   ("x"       nil)
   ("="       haskell-operator-face)
   ("Cons"    haskell-constructor-face)
   ("x"       nil)
   ("Nil"     haskell-constructor-face)
   ("test"    nil)
   ("pattern" nil)
   ("="       haskell-operator-face)
   ("pattern" nil))
  :fresh-buffer t)

(fontification-tests--test-ts-fontification
    haskell-ts-mode/fontification-6
  :modes (haskell-ts-mode)
  :contents
  (tests-utils--multiline
   ""
   "test = [quux|abcd|] ++ 'b' : \"oo\""
   "")
  :fontification
  (("test"    nil)
   ("="       haskell-operator-face)
   ("\["      nil)
   ("quux"    nil)
   ("|"       default)
   ("abcd"    font-lock-string-face)
   ("|\]"     nil)
   ("++"      haskell-operator-face)
   ("'b'"     font-lock-string-face)
   (":"       haskell-constructor-face)
   ("\"oo\""  font-lock-string-face))
  :fresh-buffer t)

(fontification-tests--test-ts-fontification
    haskell-ts-mode/fontification-7
  :modes (haskell-ts-mode)
  :contents
  (tests-utils--multiline
   ""
   "class Foo a where"
   "  test :: a -> a"
   "  default test :: Representable a ~ Foo => a -> a"
   "  test default = represent default"
   "")
  :fontification
  (("class"         haskell-keyword-face)
   ("Foo"           haskell-type-face)
   ("a"             nil)
   ("where"         haskell-keyword-face)
   ("test"          nil)
   ("::"            haskell-operator-face)
   ("a"             nil)
   ("->"            haskell-operator-face)
   ("a"             nil)
   ("default"       haskell-keyword-face)
   ("test"          nil)
   ("::"            haskell-operator-face)
   ("Representable" haskell-type-face)
   ("a"             nil)
   ("~"             haskell-operator-face)
   ("Foo"           haskell-type-face)
   ("=>"            haskell-operator-face)
   ("a"             nil)
   ("->"            haskell-operator-face)
   ("a"             nil)
   ("test"          nil)
   ("default"       nil)
   ("="             haskell-operator-face)
   ("represent"     nil)
   ("default"       nil))
  :fresh-buffer t)

(fontification-tests--test-ts-fontification
    haskell-ts-mode/fontification-8a
  :modes (haskell-ts-mode)
  :contents
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   "foo = (`Foo` 100)"
   "")
  :fontification
  (("foo"           nil)
   ("::"            haskell-operator-face)
   ("Int"           haskell-type-face)
   ("->"            haskell-operator-face)
   ("Int"           haskell-type-face)
   ("foo"           nil)
   ("="             haskell-operator-face)
   ("`Foo`"         haskell-operator-face)
   ("100"           font-lock-constant-face))
  :fresh-buffer t)

(fontification-tests--test-ts-fontification
    haskell-ts-mode/fontification-8b
  :modes (haskell-ts-mode)
  :contents
  (tests-utils--multiline
   ""
   "foo :: Int -> Int"
   "foo = (100 `Bar.Baz.Foo`)"
   "")
  :fontification
  (("foo"           nil)
   ("::"            haskell-operator-face)
   ("Int"           haskell-type-face)
   ("->"            haskell-operator-face)
   ("Int"           haskell-type-face)
   ("foo"           nil)
   ("="             haskell-operator-face)
   ("100"           font-lock-constant-face)
   ("`Bar.Baz.Foo`" haskell-operator-face))
  :fresh-buffer t)

(provide 'fontification-tests)

;; Local Variables:
;; no-byte-compile: t
;; End:

;; fontification-tests.el ends here
