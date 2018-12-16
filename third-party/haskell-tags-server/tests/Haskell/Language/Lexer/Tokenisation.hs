----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Tokenisation
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer.Tokenisation (tests) where

import Test.Tasty

import Data.List (sort)
import qualified Data.Text as T
import Data.Void (Void)

import Haskell.Language.Lexer (LiterateLocation(..))
import Haskell.Language.Lexer.TokenisationUtils
import TestUtils (makeTest)

tests :: TestTree
tests = testGroup "Basic tokenisation"
  [ testTokenise
  , testTokeniseWithNewlines
  , testStripComments
  , testBreakBlocks
  , testWhereBlock
  , testProcess
  ]

testTokenise :: TestTree
testTokenise = testGroup "Tokenise"
  [ "xyz  -- abc"          ==> [T "xyz", Newline 0]
  , "xyz  --- abc"         ==> [T "xyz", Newline 0]
  , "  {-   foo -}"        ==> [Newline 0]
  , "  {-   foo \n\
    \\n\
    \-}"                   ==> [Newline 0]
  , "  {- foo {- bar-} -}" ==> [Newline 0]
  , "  {-# INLINE #-}"     ==> [Newline 0]
  , "a::b->c"              ==> [T "a", DoubleColon, T "b", Arrow, T "c", Newline 0]
  , "a∷b→c"                ==> [T "a", DoubleColon, T "b", Arrow, T "c", Newline 0]
  , "x{-\n  bc#-}\n"       ==> [T "x", Newline 0, Newline 0]
  , "X.Y"                  ==> [T "X.Y", Newline 0]
  , "a=>b"                 ==> [T "a", Implies, T "b", Newline 0]
  , "a ⇒ b"                ==> [T "a", Implies, T "b", Newline 0]
  , "x9"                   ==> [T "x9", Newline 0]
  -- , "9x"                ==> ["nl 0", "9", "x"]
  , "x :+: y"              ==> [T "x", T ":+:", T "y", Newline 0]
  , "(#$)"                 ==> [LParen, T "#$", RParen, Newline 0]
  , "Data.Map.map"         ==> [T "Data.Map.map", Newline 0]
  , "Map.map"              ==> [T "Map.map", Newline 0]
  , "forall a. f a"        ==> [T "forall", T "a", Dot, T "f", T "a", Newline 0]
  , "forall a . Foo"       ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "forall a. Foo"        ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "forall a .Foo"        ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "forall a.Foo"         ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "$#-- hi"              ==> [T "$#--", T "hi", Newline 0]
  , "(*), (-)"             ==> [LParen, T "*", RParen, Comma, LParen, T "-", RParen, Newline 0]
  , "(.::)"                ==> [LParen, T ".::", RParen, Newline 0]
  -- we rely on this behavior
  , "data (:+) a b"        ==> [KWData, LParen, T ":+", RParen, T "a", T "b", Newline 0]
  , "data (.::+::) a b"    ==> [KWData, LParen, T ".::+::", RParen, T "a", T "b", Newline 0]
  -- string tokenization
  , "foo \"bar\" baz"      ==> [T "foo", String, T "baz", Newline 0]
  -- multiline string
  , "foo \"bar\\\n\
    \  \\bar\" baz"        ==> [T "foo", String, T "baz", Newline 0]

  -- multiline string with \r
  , "foo \"bar\\\r\n\
    \  \\bar\" baz"        ==> [T "foo", String, T "baz", Newline 0]
  -- multiline string with zero indentation
  , "foo \"bar\\\r\n\
    \\\bar\" baz"          ==> [T "foo", String, T "baz", Newline 0]
  , "(\\err -> case err of Foo -> True; _ -> False)" ==>
      [ LParen, LambdaBackslash, T "err", Arrow, KWCase, T "err", KWOf, T "Foo"
      , Arrow, T "True", Semicolon, T "_", Arrow, T "False", RParen, Newline 0
      ]
  , "foo = \"foo\\n\\\r\n\
    \  \\\" bar" ==> [T "foo", Equals, String, T "bar", Newline 0]
  , "foo = \"foo\\n\\\n\
    \  \\x\" bar" ==>
      [ T "foo", Equals, String, T "bar", Newline 0]

  , "{-   ___   -}import Data.Char;main=putStr$do{c<-\"/1 AA A A;9+ )11929 )1191A 2C9A \";e\r\n\
    \ {- {- | -} -}  {- {- || -} -}{- {- || -} -}{- {- || -} -} {--}.(`divMod`8).(+(-32)).ord$c};f(0,0)=\"\n\";f(m,n)=m?\"  \"++n?\"_/\"\r\n\
    \{- {- | -} -}n?x=do{[1..n];x}                                    --- obfuscated\r\n\
    \{-\\_/ on Fairbairn, with apologies to Chris Brown. Above is / Haskell 98 -}"
    ==>
    [ KWImport, T "Data.Char", Semicolon
    , T "main", Equals , T "putStr", T "$", KWDo, LBrace, T "c", T "<-", String, Semicolon, T "e", Newline 4
    , Dot, LParen, Backtick, T "divMod", Backtick, Number, RParen, Dot
    , LParen, T "+", LParen, Number, RParen, RParen, Dot, T "ord", T "$", T "c", RBrace, Semicolon
    , T "f", LParen, Number, Comma, Number, RParen, Equals, String, Semicolon
    , T "f", LParen, T "m", Comma, T "n", RParen, Equals, T "m", T "?", String, T "++", T "n", T "?", String, Newline 0
    , T "n", T "?", T "x", Equals, KWDo, LBrace, LBracket, Number, T "..", T "n", RBracket, Semicolon, T "x", RBrace, Newline 0, Newline 0
    ]

  , "one_hash, two_hash :: text_type\n\
    \hash_prec :: Int -> Int\n\
    \one_hash  = from_char '#'\n\
    \two_hash  = from_string \"##\"\n\
    \hash_prec = const 0"
    ==>
    [ T "one_hash", Comma, T "two_hash", DoubleColon, T "text_type", Newline 0
    , T "hash_prec", DoubleColon, T "Int", Arrow, T "Int", Newline 0
    , T "one_hash", Equals, T "from_char", Character, Newline 0
    , T "two_hash",  Equals, T "from_string", String, Newline 0
    , T "hash_prec", Equals, T "const", Number, Newline 0
    ]
  , "showComplexFloat x 0.0 = showFFloat Nothing x \"\""
    ==>
    [T "showComplexFloat", T "x", Number, Equals, T "showFFloat", T "Nothing", T "x", String, Newline 0]
  , "deriveJSON defaultOptions ''CI.CI"
    ==>
    [T "deriveJSON", T "defaultOptions", T "''CI.CI", Newline 0]
  , "--:+: :+: :+:"
    ==>
    [T "--:+:", T ":+:", T ":+:", Newline 0]

  , "\\x -> y" ==> [LambdaBackslash, T "x", Arrow, T "y", Newline 0]
  , "precalcClosure0 :: Grammar -> Name -> RuleList\n\
    \precalcClosure0 g = \n\
    \\\n -> case lookup n info' of\n\
    \\tNothing -> []\n\
    \\tJust c  -> c\n\
    \  where"
    ==>
    [ T "precalcClosure0", DoubleColon, T "Grammar", Arrow, T "Name", Arrow, T "RuleList", Newline 0
    , T "precalcClosure0", T "g", Equals, Newline 0
    , LambdaBackslash, T "n", Arrow, KWCase, T "lookup", T "n", T "info'", KWOf, Newline 1
    , T "Nothing", Arrow, LBracket, RBracket, Newline 1
    , T "Just", T "c", Arrow, T "c", Newline 2
    , KWWhere, Newline 0
    ]

  , tokeniseSplices
  , "import Foo hiding (Bar)"                ==>
    [KWImport, T "Foo", T "hiding", LParen, T "Bar", RParen, Newline 0]
  , "import {-# SOURCE #-} Foo hiding (Bar)" ==>
    [KWImport, Pragma SourcePragma, T "Foo", T "hiding", LParen, T "Bar", RParen, Newline 0]
  , "import {-# source #-} Foo hiding (Bar)" ==>
    [KWImport, Pragma SourcePragma, T "Foo", T "hiding", LParen, T "Bar", RParen, Newline 0]
  , "import {-# SoUrCe #-} Foo hiding (Bar)" ==>
    [KWImport, Pragma SourcePragma, T "Foo", T "hiding", LParen, T "Bar", RParen, Newline 0]
  ]
  where
    (==>) = makeTest f
    f :: T.Text -> [ServerToken]
    f = tail -- strip uninteresting initial newline
      . map valOf
      . tokenize' filename Vanilla

    tokeniseSplices = testGroup "Splices"
      [ "$(foo)"                                  ==>
        [SpliceStart, T "foo", RParen, Newline 0]
      , "$(foo [| baz |])"                        ==>
        [SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen, Newline 0]
      , "$(foo ⟦ baz ⟧)"                          ==>
        [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
        , Newline 0
        ]
      , "$(foo [bar| baz |])"                     ==>
        [SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen, Newline 0]
      , "$(foo [Foo.bar| baz ⟧)"                  ==>
        [SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen, Newline 0]
      , "$(foo [$bar| baz |])"                    ==>
        [SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen, Newline 0]
      , "$(foo [$Foo.bar| baz ⟧)"                 ==>
        [SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen, Newline 0]
      , "$(foo [bar| bar!\nbaz!\n $(baz) |])"     ==>
        [SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz", RParen, QuasiquoterEnd, RParen, Newline 0]
      , "$(foo [Foo.bar| bar!\nbaz!\n $(baz) ⟧)"  ==>
        [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
        , RParen, QuasiquoterEnd, RParen, Newline 0
        ]
      , "$(foo [$bar| bar!\nbaz!\n $(baz) |])"    ==>
        [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
        , RParen, QuasiquoterEnd, RParen, Newline 0
        ]
      , "$(foo [$Foo.bar| bar!\nbaz!\n $(baz) ⟧)" ==>
        [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
        , RParen, QuasiquoterEnd, RParen, Newline 0
        ]
      ]


testTokeniseWithNewlines :: TestTree
testTokeniseWithNewlines = testGroup "Tokenise with newlines"
  [ testGroup "Vanilla"
    [ "x\ny\n"     ==> [Newline 0, T "x", Newline 0, T "y", Newline 0, Newline 0]
    , " xx\n yy\n" ==> [Newline 1, T "xx", Newline 1, T "yy", Newline 0, Newline 0]
    , "\n\
      \class (X x) => C a b where\n\
      \\tm :: a->b\n\
      \\tn :: c\n"
      ==>
      [ Newline 0
      , Newline 0
      , KWClass, LParen, T "X", T "x", RParen, Implies, T "C", T "a", T "b", KWWhere, Newline 1
      , T "m", DoubleColon, T "a", Arrow, T "b", Newline 1
      , T "n", DoubleColon, T "c"
      , Newline 0
      , Newline 0
      ]
    ]
  , testGroup "Literate"
    [ "foo bar baz"
      |=>
      []
    , "foo bar baz\nquux fizz buzz"
      |=>
      []
    , "> foo = 1"
      |=>
      [Newline 1, T "foo", Equals, Number, Newline 0]
    , "This is a factorial function\n\
      \\n\
      \> factorial :: Integer -> Integer\n\
      \> factorial 0 = 1\n\
      \> factorial n = \n\
      \>   n * (factorial $ n - 1)\n\
      \\n\
      \And that's it !"
      |=>
      -- TODO: maybe keep track of minimal newline indent across whole file
      -- and subtract it from all newlines at the end?
      [ Newline 1
      , T "factorial", DoubleColon, T "Integer", Arrow, T "Integer", Newline 1
      , T "factorial", Number, Equals, Number, Newline 1
      , T "factorial", T "n", Equals, Newline 3
      , T "n", T "*", LParen, T "factorial", T "$", T "n", T "-", Number, RParen, Newline 0
      ]
    , "This is a factorial function\n\
      \\n\
      \> factorial :: Integer -> Integer\n\
      \> factorial 0 = 1\n\
      \> factorial n = \n\
      \>   n * (factorial $ n - 1)\n\
      \\n\
      \And another function:\n\
      \\n\
      \> foo :: a -> a\n\
      \> foo x = x"
      |=>
      -- TODO: maybe keep track of minimal newline indent across whole file
      -- and subtract it from all newlines at the end?
      [ Newline 1
      , T "factorial", DoubleColon, T "Integer", Arrow, T "Integer", Newline 1
      , T "factorial", Number, Equals, Number, Newline 1
      , T "factorial", T "n", Equals, Newline 3
      , T "n", T "*", LParen, T "factorial", T "$", T "n", T "-", Number, RParen, Newline 0
      , Newline 1
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 1
      , T "foo", T "x", Equals, T "x", Newline 0
      ]
    , "This is a factorial function\n\
      \\\begin{code}\n\
      \factorial :: Integer -> Integer\n\
      \factorial 0 = 1\n\
      \factorial n = \n\
      \  n * (factorial $ n - 1)\n\
      \\\end{code}\n\
      \And that's it !"
      |=>
      [ Newline 0
      , T "factorial", DoubleColon, T "Integer", Arrow, T "Integer", Newline 0
      , T "factorial", Number, Equals, Number, Newline 0
      , T "factorial", T "n", Equals, Newline 2
      , T "n", T "*", LParen, T "factorial", T "$", T "n", T "-", Number, RParen
      ]
    , "This is a 'factorial' function\n\
      \\\begin{code}\n\
      \factorial :: Integer -> Integer\n\
      \factorial 0 = 1\n\
      \factorial n = \n\
      \  n * (factorial $ n - 1)\n\
      \\\end{code}\n\
      \But that's not it yet! Here's another function:\n\
      \\\begin{code}\n\
      \foo :: a -> a\n\
      \foo x = x\n\
      \\\end{code}\n\
      \And that's it !"
      |=>
      [ Newline 0
      , T "factorial", DoubleColon, T "Integer", Arrow, T "Integer", Newline 0
      , T "factorial", Number, Equals, Number, Newline 0
      , T "factorial", T "n", Equals, Newline 2
      , T "n", T "*", LParen, T "factorial", T "$", T "n", T "-", Number, RParen, Newline 0
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
      , T "foo", T "x", Equals, T "x"
      ]
    , "This is a 'factorial' function\n\
      \\\begin{code}\n\
      \  factorial :: Integer -> Integer\n\
      \  factorial 0 = 1\n\
      \  factorial n = \n\
      \    n * (factorial $ n - 1)\n\
      \\\end{code}\n\
      \But that's not it yet! Here's another function:\n\
      \\\begin{code}\n\
      \  foo :: a -> a\n\
      \  foo x = x\n\
      \\\end{code}\n\
      \And that's it !"
      |=>
      [ Newline 2
      , T "factorial", DoubleColon, T "Integer", Arrow, T "Integer", Newline 2
      , T "factorial", Number, Equals, Number, Newline 2
      , T "factorial", T "n", Equals, Newline 4
      , T "n", T "*", LParen, T "factorial", T "$", T "n", T "-", Number, RParen

      , Newline 2
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 2
      , T "foo", T "x", Equals, T "x"
      ]
    , "Test\n\
      \\\begin{code}\n\
      \class (X x) => C a b where\n\
      \  m :: a->b\n\
      \  n :: c\n\
      \\\end{code}"
      |=>
      [ Newline 0
      , KWClass, LParen, T "X", T "x", RParen, Implies, T "C", T "a", T "b", KWWhere, Newline 2
      , T "m", DoubleColon, T "a", Arrow, T "b", Newline 2
      , T "n", DoubleColon, T "c"
      ]
    , "Test\n\
      \\\begin{code}\n\
      \class (X x) => C a b where\n\
      \\tm :: a->b\n\
      \\tn :: c\n\
      \\\end{code}"
      |=>
      [ Newline 0
      , KWClass, LParen, T "X", T "x", RParen, Implies, T "C", T "a", T "b", KWWhere, Newline 1
      , T "m", DoubleColon, T "a", Arrow, T "b", Newline 1
      , T "n", DoubleColon, T "c"
      ]
    ]
  ]
  where
    (==>) = makeTest (f Vanilla)
    (|=>) = makeTest (f LiterateOutside)
    f mode =
        map valOf
      . tokenize' filename mode


testStripComments :: TestTree
testStripComments = testGroup "Strip comments"
  [ "hello -- there"
    ==>
    [Newline 0, T "hello", Newline 0]
  , "hello --there"
    ==>
    [Newline 0, T "hello", Newline 0]
  , "hello {- there -} fred"
    ==>
    [Newline 0, T "hello", T "fred", Newline 0]
  , "hello -- {- there -}\n\
    \fred"
    ==>
    [Newline 0, T "hello", Newline 0, T "fred", Newline 0]
  , "{-# LANG #-} hello {- there {- nested -} comment -} fred"
    ==>
    [Newline 1, T "hello", T "fred", Newline 0]
  , "hello {-\n\
    \there\n\
    \------}\n\
    \ fred"
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , "hello {-  \n\
    \there\n\
    \  ------}  \n\
    \ fred"
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , "hello {-\n\
    \there\n\
    \-----}\n\
    \ fred"
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , "hello {-  \n\
    \there\n\
    \  -----}  \n\
    \ fred"
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , "hello {-\n\
    \-- there -}"
    ==>
    [Newline 0, T "hello", Newline 0]
  , "foo --- my comment\n\
    \--- my other comment\n\
    \bar"
    ==>
    [Newline 0, T "foo", Newline 0, Newline 0, T "bar", Newline 0]
  ]
  where
    (==>) = makeTest f
    f = map valOf . tokenize' filename Vanilla


testBreakBlocks :: TestTree
testBreakBlocks = testGroup "Break blocks"
  [ testGroup "Vanilla"
    [ "a\n\
      \b\n"
      ==>
      [ [T "a"]
      , [T "b"]
      ]
    , "a\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , "a\n\
      \ a\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    -- intervening blank lines are ignored
    , "a\n\
      \ a\n\
      \\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , "a\n\
      \\n\
      \\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , "a\n\
      \ aa\n\
      \ aa\n"
      ==>
      [[T "a", Newline 1, T "aa", Newline 1, T "aa"]]
    , " aa\n\
      \ aa\n"
      ==>
      [ [T "aa"]
      , [T "aa"]
      ]

    , "one_hash, two_hash :: text_type\n\
      \hash_prec :: Int -> Int\n\
      \one_hash  = from_char '#'\n\
      \two_hash  = from_string \"##\"\n\
      \hash_prec = const 0"
      ==>
      [ [T "one_hash", Comma, T "two_hash", DoubleColon, T "text_type"]
      , [T "hash_prec", DoubleColon, T "Int", Arrow, T "Int"]
      , [T "one_hash", Equals, T "from_char", Character]
      , [T "two_hash",  Equals, T "from_string", String]
      , [T "hash_prec", Equals, T "const", Number]
      ]
    , "one_hash, two_hash :: text_type; \
      \hash_prec :: Int -> Int; \
      \one_hash  = from_char '#'; \
      \two_hash  = from_string \"##\"; \
      \hash_prec = const 0"
      ==>
      [ [T "one_hash", Comma, T "two_hash", DoubleColon, T "text_type"]
      , [T "hash_prec", DoubleColon, T "Int", Arrow, T "Int"]
      , [T "one_hash", Equals, T "from_char", Character]
      , [T "two_hash",  Equals, T "from_string", String]
      , [T "hash_prec", Equals, T "const", Number]
      ]
    , "{\n\
      \  data F f :: * ; -- foo\n\
      \                  -- bar\n\
      \                  -- baz\n\
      \  mkF  :: f -> F f ; getF :: F f -> f ;\n\
      \} ;"
      ==>
      [ [ LBrace, Newline 2, KWData, T "F", T "f", DoubleColon, T "*", Semicolon, Newline 2
        , T "mkF", DoubleColon, T "f", Arrow, T "F", T "f", Semicolon
        , T "getF", DoubleColon, T "F", T "f", Arrow, T "f", Semicolon, Newline 0
        , RBrace
        ]
      ]
    ]
  , testGroup "Literate"
    [ "> a\n\
      \>\n\
      \>\n\
      \>  a\n\
      \> b\n"
      |=>
      [ [T "a", Newline 2, T "a"]
      , [T "b"]
      ]
    , "> a\n\
      \> \n\
      \> \n\
      \>  a\n\
      \> b\n"
      |=>
      [ [T "a", Newline 2, T "a"]
      , [T "b"]
      ]
    , "> a\n\
      \>  aa\n\
      \>  aa\n"
      |=>
      [[T "a", Newline 2, T "aa", Newline 2, T "aa"]]
    , "> a\n\
      \>  aa\n\
      \>\n\
      \>  aa\n"
      |=>
      [[T "a", Newline 2, T "aa", Newline 2, T "aa"]]
   ]
  ]
  where
    (==>) = makeTest (f Vanilla)
    (|=>) = makeTest (f LiterateOutside)
    f :: LiterateLocation Void -> T.Text -> [[ServerToken]]
    f mode =
        map (map (embedServerToken . valOf) . unstrippedTokensOf)
      . breakBlocks
      . UnstrippedTokens
      . stripServerTokens'
      . tokenize' filename mode


testWhereBlock :: TestTree
testWhereBlock = testGroup "whereBlock"
  [ "class A f where\n\
    \  data F f :: * -- foo\n\
    \                -- bar\n\
    \                -- baz\n\
    \  mkF  :: f -> F f\n\
    \  getF :: F f -> f"
    ==>
    [ [KWData, T "F", T "f", DoubleColon, T "*"]
    , [T "mkF", DoubleColon, T "f", Arrow, T "F", T "f"]
    , [T "getF", DoubleColon, T "F", T "f", Arrow, T "f"]
    ]
  , "class A f where {\n\
    \  data F f :: * ; -- foo\n\
    \                  -- bar\n\
    \                  -- baz\n\
    \  mkF  :: f -> F f ;\n\
    \  getF :: F f -> f ;\n\
    \} ;"
    ==>
    [ [KWData, T "F", T "f", DoubleColon, T "*"]
    , [T "mkF", DoubleColon, T "f", Arrow, T "F", T "f"]
    , [T "getF", DoubleColon, T "F", T "f", Arrow, T "f"]
    ]
  ]
  where
    (==>) = makeTest f
    f = map (map (embedServerToken . valOf) . unstrippedTokensOf)
      . whereBlock
      . UnstrippedTokens
      . stripServerTokens'
      . tokenize' filename Vanilla


testProcess :: TestTree
testProcess = testGroup "Process"
  [ testPrefixes
  , testData
  , testGADT
  , testFamilies
  , testFunctions
  , testClass
  , testInstance
  , testLiterate
  , testPatterns
  , testFFI
  ]

testPrefixes :: TestTree
testPrefixes = testGroup "Prefix tracking"
  [ "module Bar.Foo where\n" ==>
      [Pos (SrcPos fn 1 0 "" "") (TagVal "Foo" Module Nothing)]
  , "newtype Foo a b =\n\
    \\tBar x y z\n" ==>
    [ Pos (SrcPos fn 1 0 "" "") (TagVal "Foo" Type Nothing)
    , Pos (SrcPos fn 2 0 "" "") (TagVal "Bar" Constructor (Just "Foo"))
    ]
  , "data Foo a b =\n\
    \\tBar x y z\n"
    ==>
    [ Pos (SrcPos fn 1 0 "" "") (TagVal "Foo" Type Nothing)
    , Pos (SrcPos fn 2 0 "" "") (TagVal "Bar" Constructor (Just "Foo"))
    ]
  , "f :: A -> B\n\
    \g :: C -> D\n\
    \data D = C {\n\
    \\tf :: A\n\
    \\t}\n"
    ==>
    [ Pos (SrcPos fn 1 0 "" "") (TagVal "f" Function Nothing)
    , Pos (SrcPos fn 2 0 "" "") (TagVal "g" Function Nothing)
    , Pos (SrcPos fn 3 0 "" "") (TagVal "C" Constructor (Just "D"))
    , Pos (SrcPos fn 3 0 "" "") (TagVal "D" Type Nothing)
    , Pos (SrcPos fn 4 0 "" "") (TagVal "f" Function (Just "D"))
    ]
  , "instance Foo Bar where\n\
    \  newtype FooFam Bar = BarList [Int]"
    ==>
    [ Pos (SrcPos fn 2 0 "" "") (TagVal "BarList" Constructor (Just "FooFam"))
    ]
  , "instance Foo Bar where\n\
    \  newtype FooFam Bar = BarList { getBarList :: [Int] }"
    ==>
    [ Pos (SrcPos fn 2 0 "" "") (TagVal "BarList" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 2 0 "" "") (TagVal "getBarList" Function (Just "FooFam"))
    ]
  , "instance Foo Bar where\n\
    \  data (Ord a) => FooFam Bar a = BarList { getBarList :: [a] }\n\
    \                               | BarMap { getBarMap :: Map a Int }"
    ==>
    [ Pos (SrcPos fn 2 0 "" "")  (TagVal "BarList" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 2 0 "" "")  (TagVal "getBarList" Function (Just "FooFam"))
    , Pos (SrcPos fn 3 0 "" "") (TagVal "BarMap" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 3 0 "" "") (TagVal "getBarMap" Function (Just "FooFam"))
    ]
  , "newtype instance FooFam Bar = BarList { getBarList :: [Int] }"
    ==>
    [ Pos (SrcPos fn 1 0 "" "") (TagVal "BarList" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 1 0 "" "") (TagVal "getBarList" Function (Just "FooFam"))
    ]
  , "data instance (Ord a) => FooFam Bar a = BarList { getBarList :: [a] }\n\
    \                                      | BarMap { getBarMap :: Map a Int }"
    ==>
    [ Pos (SrcPos fn 1 0 "" "")  (TagVal "BarList" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 1 0 "" "")  (TagVal "getBarList" Function (Just "FooFam"))
    , Pos (SrcPos fn 2 0 "" "") (TagVal "BarMap" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 2 0 "" "") (TagVal "getBarMap" Function (Just "FooFam"))
    ]
  ]

  where
    (==>) = testFullTagsWithoutPrefixes fn Vanilla
    fn = filename

testData :: TestTree
testData = testGroup "Data"
  [ "data X\n"                            ==> ["X"]
  , "data X = X Int\n"                    ==> ["X", "X"]
  , "data Foo = Bar | Baz"                ==> ["Bar", "Baz", "Foo"]
  , "data Foo =\n\tBar\n\t| Baz"          ==> ["Bar", "Baz", "Foo"]
  -- Records.
  , "data Foo a = Bar { field :: Field }" ==> ["Bar", "Foo", "field"]
  , "data R = R { a::X, b::Y }"           ==> ["R", "R", "a", "b"]
  , "data R = R { a, b::X }"              ==> ["R", "R", "a", "b"]
  , "data R = R { a∷X, b∷Y }"             ==> ["R", "R", "a", "b"]
  , "data R = R { a, b∷X }"               ==> ["R", "R", "a", "b"]
  , "data R = R {\n\ta::X\n\t, b::Y\n\t}" ==> ["R", "R", "a", "b"]
  , "data R = R {\n\ta,b::X\n\t}"         ==> ["R", "R", "a", "b"]
  -- Record operators
  , "data Foo a b = (:*:) { foo :: a, bar :: b }" ==>
      [":*:", "Foo", "bar", "foo"]

  , "data R = R {\n\
    \    a :: !RealTime\n\
    \  , b :: !RealTime\n\
    \}"
    ==>
    ["R", "R", "a", "b"]
  , "data Rec = Rec {\n\
    \  a :: Int\n\
    \, b :: !Double\n\
    \, c :: Maybe Rec\
    \\n\
    \}"
    ==>
    ["Rec", "Rec", "a", "b", "c"]

  , "data X = X !Int"                            ==> ["X", "X"]
  , "data X = Y !Int !X | Z"                     ==> ["X", "Y", "Z"]
  , "data X = Y :+: !Z | !X `Mult` X"            ==> [":+:", "Mult", "X"]
  , "data X = !Y `Add` !Z"                       ==> ["Add", "X"]

  , "data X = forall a. Y a"                     ==> ["X", "Y"]
  , "data X = forall a . Y a"                    ==> ["X", "Y"]
  , "data X = forall a .Y a"                     ==> ["X", "Y"]
  , "data X = forall a.Y a"                      ==> ["X", "Y"]
  , "data X = forall a. Eq a => Y a"             ==> ["X", "Y"]
  , "data X = forall a. (Eq a) => Y a"           ==> ["X", "Y"]
  , "data X = forall (a :: Nat). (Eq' a) => Y a" ==> ["X", "Y"]
  , "data X = forall a. (Eq a, Ord a) => Y a"    ==> ["X", "Y"]
  , "data X = forall a. Ref :<: a => Y a"        ==> ["X", "Y"]
  , "data X = forall a. (:<:) Ref a => Y a"      ==> ["X", "Y"]
  , "data X = forall a. ((:<:) Ref a) => Y a"    ==> ["X", "Y"]
  , "data X = forall a. Y !a"                    ==> ["X", "Y"]
  , "data X = forall a. (Eq a, Ord a) => Y !a"   ==> ["X", "Y"]

  , "data Foo a = \n\
    \    Plain Int\n\
    \  | forall a. Bar a Int\n\
    \  | forall a b. Baz b a\n\
    \  | forall a . Quux a \
    \  | forall a .Quuz a"
    ==>
    ["Bar", "Baz", "Foo", "Plain", "Quux", "Quuz"]

  , "data X a = Add a "                                               ==> ["Add", "X"]
  , "data Eq a => X a = Add a"                                        ==> ["Add", "X"]
  , "data (Eq a) => X a = Add a"                                      ==> ["Add", "X"]
  , "data (Eq a, Ord a) => X a = Add a"                               ==> ["Add", "X"]
  , "data (Eq (a), Ord (a)) => X a = Add a"                           ==> ["Add", "X"]

  , "data Ref :<: f => X f = RRef f"                                  ==> ["RRef", "X"]
  , "data a :<: b => X a b = Add a"                                   ==> ["Add", "X"]
  , "newtype Ref :<: f => X f = RRef f"                               ==> ["RRef", "X"]
  , "newtype a :<: b => X a b = Add a"                                ==> ["Add", "X"]
  , "data Ref :<: [f] => X f = RRef f"                                ==> ["RRef", "X"]
  , "data [a] :<: b => X a b = Add a"                                 ==> ["Add", "X"]
  , "newtype Ref :<: [f] => X f = RRef f"                             ==> ["RRef", "X"]
  , "newtype [a] :<: b => X a b = Add a"                              ==> ["Add", "X"]

  , "data (a :<: b) => X a b = Add a"                                 ==> ["Add", "X"]
  , "data a :><: b = a :>|<: b"                                       ==> [":><:", ":>|<:"]
  , "data (:><:) a b = (:>|<:) a b"                                   ==> [":><:", ":>|<:"]
  , "data (:><:) a b = Foo b | (:>|<:) a b"                           ==> [":><:", ":>|<:", "Foo"]
  , "data (:><:) a b = Foo b | forall c. (:>|<:) a c"                 ==> [":><:", ":>|<:", "Foo"]
  , "data (:><:) a b = Foo b | forall c. (Eq c) => (:>|<:) a c"       ==> [":><:", ":>|<:", "Foo"]
  , "data (:><:) a b = Foo b | forall c. Eq c => (:>|<:) a c"         ==> [":><:", ":>|<:", "Foo"]

  , "newtype Eq a => X a = Add a"                                     ==> ["Add", "X"]
  , "newtype (Eq a) => X a = Add a"                                   ==> ["Add", "X"]
  , "newtype (Eq a, Ord a) => X a = Add a"                            ==> ["Add", "X"]
  , "newtype () => X a = Add a"                                       ==> ["Add", "X"]

  , "newtype (u :*: v) z = X"                                         ==> [":*:", "X"]
  , "data (u :*: v) z = X"                                            ==> [":*:", "X"]
  , "type (u :*: v) z = (u, v, z)"                                    ==> [":*:"]

  , "newtype ((u :: (* -> *) -> *) :*: v) z = X"                      ==> [":*:", "X"]
  , "data ((u :: (* -> *) -> *) :*: v) z = X"                         ==> [":*:", "X"]
  , "type ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"                 ==> [":*:"]

  , "newtype () => ((u :: (* -> *) -> *) :*: v) z = X"                ==> [":*:", "X"]
  , "data () => ((u :: (* -> *) -> *) :*: v) z = X"                   ==> [":*:", "X"]
  , "type () => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"           ==> [":*:"]

  , "newtype (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = X"        ==> [":*:", "X"]
  , "data (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = X"           ==> [":*:", "X"]
  , "type (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"   ==> [":*:"]

  , "newtype Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = X"          ==> [":*:", "X"]
  , "data Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = X"             ==> [":*:", "X"]
  , "type Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"     ==> [":*:"]

  , "newtype (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = X"      ==> ["Foo", "X"]
  , "data (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = X"         ==> ["Foo", "X"]
  , "type (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = (u, v, z)" ==> ["Foo"]
  , "type (Eq (v z)) => ((u ∷ (* -> *) -> *) `Foo` v) z = (u, v, z)"  ==> ["Foo"]

  , "newtype Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = X"        ==> ["Foo", "X"]
  , "data Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = X"           ==> ["Foo", "X"]
  , "type Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = (u, v, z)"   ==> ["Foo"]
  , "type Eq (v z) ⇒ ((u ∷ (* → *) → *) `Foo` v) z = (u, v, z)"       ==> ["Foo"]

  , "data (:*:) u v z = X"                                            ==> [":*:", "X"]
  , "data (Eq (u v), Ord (z)) => (:*:) u v z = X"                     ==> [":*:", "X"]
  , "data (u `W` v) z = X"                                            ==> ["W", "X"]
  , "data (Eq (u v), Ord (z)) => (u `W` v) z = X"                     ==> ["W", "X"]

  , "newtype X a = Z {\n\
    \ -- TODO blah\n\
    \ foo :: [a] }"
    ==>
    ["X", "Z", "foo"]
  , "newtype (u :*: v) z = X {\n\
    \ -- my insightful comment\n\
    \ extract :: (u (v z)) }"
    ==>
    [":*:", "X", "extract"]
  , "newtype (u :*: v) z = X {\n\
    \ -- my insightful comment\n\
    \ pattern :: (u (v z)) }"
    ==>
    [":*:", "X", "pattern"]

  , "data Hadron a b = Science { f :: a, g :: a, h :: b }"
    ==>
    ["Hadron", "Science", "f", "g", "h"]
  , "data Hadron a b = Science { f :: a, g :: a, pattern :: b }"
    ==>
    ["Hadron", "Science", "f", "g", "pattern"]
  , "data Hadron a b = Science { f :: a, g :: (a, b), h :: b }"
    ==>
    ["Hadron", "Science", "f", "g", "h"]
  , "data Hadron a b = forall x. Science { f :: x, h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x. Science { f :: [x], h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x y. Science { f :: (x, y), h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x y. Science { f :: [(x, y)], h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = \
    \forall x y. Science { f :: [(Box x, Map x y)], h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x y. Science { f ∷ [(Box x, Map x y)], h ∷ b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x y z. Science\n\
    \  { f :: x\n\
    \  , g :: [(Box x, Map x y, z)] \
    \  }"
    ==>
    ["Hadron", "Science", "f", "g"]
  , "data Hadron a b = forall x y z. Science\n\
    \  { f :: x\n\
    \  , g :: [(Box x, Map x y, z)] \
    \  , h :: b\n\
    \  }"
    ==>
    ["Hadron", "Science", "f", "g", "h"]
  , "data Hadron a b = Science { h :: b }"
    ==>
    ["Hadron", "Science", "h"]
  , "data Test a b =\n\
    \    Foo a\n\
    \  | Bar [(Maybe a, Map a b, b)]"
    ==>
    ["Bar", "Foo", "Test"]
  , "data Test a b =\n\
    \    Foo a\n\
    \  | [(Maybe b, Map b a, a)] `Bar` [(Maybe a, Map a b, b)]"
    ==>
    ["Bar", "Foo", "Test"]
  , "data IO a = IO (World->(a,World))"
    ==>
    ["IO", "IO"]

  , "data SubTransformTriple a =\n\
    \        SubTransformTriple\n\
    \           (forall sh. (Shape sh, Slice sh) => Transform (sh:.Int) a)\n\
    \           (forall sh. (Shape sh, Slice sh) => Transform (sh:.Int) a)\n\
    \           (forall sh. (Shape sh, Slice sh) => Transform (sh:.Int) a)"
    ==>
    ["SubTransformTriple", "SubTransformTriple"]

  , "data TestCase \n\
    \    = forall a prop . (Testable prop, Data a) \n\
    \    => TestCase  (((String, a, a) -> Property) -> prop)\n\
    \        deriving (Typeable)"
    ==>
    ["TestCase", "TestCase"]

  , "-- | Binding List\n\
    \data BindingList v a = Variable v => BindingList {source :: Source v a -- ^ the list's binding source\n\
    \                                                , list   :: v [v a]    -- ^ the bound list\n\
    \                                                , pos    :: v Int}     -- ^ the current position"
    ==>
    ["BindingList", "BindingList", "list", "pos", "source"]

  , "data Tester a = Tester\n\
    \    {(===) :: [String] -> a -> IO ()\n\
    \    ,fails :: [String] -> IO ()\n\
    \    ,isHelp :: [String] -> [String] -> IO ()\n\
    \    ,isHelpNot :: [String] -> [String] -> IO ()\n\
    \    ,isVersion :: [String] -> String -> IO ()\n\
    \    ,isVerbosity :: [String] -> Verbosity -> IO ()\n\
    \    ,completion :: [String] -> (Int,Int) -> [Complete] -> IO ()\n\
    \    }"
    ==>
    ["===", "Tester", "Tester", "completion", "fails", "isHelp", "isHelpNot", "isVerbosity", "isVersion"]

  , "data Tester a = Tester\n\
    \    {(===) :: [String] -> a -> IO ()\n\
    \    ,fails :: [String] -> IO ()\n\
    \    ,isHelp, isHelpNot :: [String] -> [String] -> IO ()\n\
    \    ,isVersion :: [String] -> String -> IO ()\n\
    \    ,isVerbosity :: [String] -> Verbosity -> IO ()\n\
    \    ,completion :: [String] -> (Int,Int) -> [Complete] -> IO ()\n\
    \    }"
    ==>
    ["===", "Tester", "Tester", "completion", "fails", "isHelp", "isHelpNot", "isVerbosity", "isVersion"]

  , "-- | View of the right end of a sequence.\n\
    \data ViewR s a\n\
    \    = EmptyR\n\
    \    | s a :> a"
    ==>
    [":>", "EmptyR", "ViewR"]
  , "-- | View of the right end of a sequence.\n\
    \data ViewR s a\n\
    \    = s a :> a\n\
    \    | EmptyR"
    ==>
    [":>", "EmptyR", "ViewR"]

  , "data [] a = [] | a : [a]"
    ==> [":", "[]", "[]"]
  , "data () = ()"
    ==> ["()", "()"]
  , "data (,) a b = (,) a b"
    ==> ["(,)", "(,)"]
  , "data (,,) a b c = (,,) a b c"
    ==> ["(,,)", "(,,)"]
  , "data (a, b) = (a, b)"
    ==> ["(,)", "(,)"]
  , "data (a, b, c) = (a, b, c)"
    ==> ["(,,)", "(,,)"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testGADT :: TestTree
testGADT = testGroup "GADT"
  [ "data X where A :: X\n"              ==> ["A", "X"]
  , "data X where\n\tA :: X\n"           ==> ["A", "X"]
  , "data X where\n\tA :: X\n\tB :: X\n" ==> ["A", "B", "X"]
  , "data X where\n\tA, B :: X\n"        ==> ["A", "B", "X"]
  , "data X :: * -> * -> * where\n\
    \  A, B :: Int -> Int -> X\n"
    ==>
    ["A", "B", "X"]
  , "data X ∷ * → * → * where\n\
    \  A, B ∷ Int → Int → X\n"
    ==>
    ["A", "B", "X"]
  , "data Vec ix where\n\
    \  Nil   :: Int -> Foo Int\n\
    \  (:::) :: Int -> Vec Int -> Vec Int\n\
    \  (:+.) :: Int -> Int -> Vec Int -> Vec Int\n"
    ==>
    [":+.", ":::", "Nil", "Vec"]
  , "data Vec ix where\n\
    \  Nil   :: Int -> Foo Int\n\
    \  -- foo\n\
    \  (:::) :: Int -> Vec Int -> Vec Int\n\
    \-- bar\n\
    \  (:+.) :: Int     -> \n\
    \           -- ^ baz\n\
    \           Int     -> \n\
    \           Vec Int -> \n\
    \Vec Int\n"
    ==>
    [":+.", ":::", "Nil", "Vec"]
  , "data NatSing (n :: Nat) where\n\
    \  ZeroSing :: 'Zero\n\
    \  SuccSing :: NatSing n -> NatSing ('Succ n)\n"
    ==>
    ["NatSing", "SuccSing", "ZeroSing"]
  , "data Rec a where\n\
    \  C :: { foo :: Int } -> Rec a"
    ==>
    ["C", "Rec", "foo"]
  , "data Rec a where\n\
    \  C :: { foo :: Int, bar :: Int -> Int } -> Rec a"
    ==>
    ["C", "Rec", "bar", "foo"]
  , "data Rec a where\n\
    \  C :: { foo :: Int, bar :: Int -> Int } -> Rec a\n\
    \  D :: { baz :: (Int -> Int) -> Int, bar :: (((Int) -> (Int))) } -> Rec a"
    ==>
    ["C", "D", "Rec", "bar", "bar", "baz", "foo"]
  , "newtype TyConProxy a b where\n\
    \    TyConProxy :: () -> TyConProxy a b\n\
    \  deriving ( Arbitrary\n\
    \           , Show\n\
    \           , Generic\n\
    \#if defined(__LANGUAGE_DERIVE_GENERIC1__)\n\
    \           , Generic1\n\
    \#endif\n\
    \           )"
    ==>
    ["TyConProxy", "TyConProxy"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testFamilies :: TestTree
testFamilies = testGroup "Families"
  [ "type family X :: *\n"      ==> ["X"]
  , "data family X :: * -> *\n" ==> ["X"]
  , "data family a ** b"        ==> ["**"]

  , "type family a :<: b\n"                               ==> [":<:"]
  , "type family (a :: Nat) :<: (b :: Nat) :: Nat\n"      ==> [":<:"]
  , "type family (a :: Nat) `Family` (b :: Nat) :: Nat\n" ==> ["Family"]
  , "type family (m :: Nat) <=? (n :: Nat) :: Bool"       ==> ["<=?"]
  , "type family (m ∷ Nat) <=? (n ∷ Nat) ∷ Bool"         ==> ["<=?"]

  , "data instance X a b = Y a | Z { unZ :: b }"                     ==>
      ["Y", "Z", "unZ"]
  , "data instance (Eq a, Eq b) => X a b = Y a | Z { unZ :: b }"     ==>
      ["Y", "Z", "unZ"]
  , "data instance (Eq a, Eq b) => X a b = Y a | Z { pattern :: b }" ==>
      ["Y", "Z", "pattern"]
  , "data instance XList Char = XCons !Char !(XList Char) | XNil"    ==>
      ["XCons", "XNil"]
  , "newtype instance Cxt x => T [x] = A (B x) deriving (Z,W)"       ==> ["A"]
  , "type instance Cxt x => T [x] = A (B x)"                         ==> []
  , "data instance G [a] b where\n\
    \   G1 :: c -> G [Int] b\n\
    \   G2 :: G [a] Bool"
    ==>
    ["G1", "G2"]
  , "class C where\n\ttype X y :: *\n" ==> ["C", "X"]
  , "class C where\n\tdata X y :: *\n" ==> ["C", "X"]
  , "class C where\n\ttype X y ∷ *\n"  ==> ["C", "X"]
  , "class C where\n\tdata X y ∷ *\n"  ==> ["C", "X"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testFunctions :: TestTree
testFunctions = testGroup "Functions"
  [
  -- Multiple declarations.
    "a,b::X"          ==> ["a", "b"]
  -- With an operator.
  , "(+), a :: X"     ==> ["+", "a"]
  -- Unicode operator.
  , "(•) :: X"        ==> ["•"]
  -- Don't get fooled by literals.
  , "1 :: Int"        ==> []
  -- Don't confuse _ wildcard for an operator.
  , "f :: Int -> Int\n\
    \f _ = 1"         ==> ["f"]

  -- plain functions and operators
  , "(.::) :: X -> Y" ==> [".::"]
  , "(+::) :: X -> Y" ==> ["+::"]
  , "(->:) :: X -> Y" ==> ["->:"]
  , "(--+) :: X -> Y" ==> ["--+"]
  , "(=>>) :: X -> Y" ==> ["=>>"]

  -- Multi-line with semicolons at the end
  , "one_hash, two_hash :: text_type;\n\
    \hash_prec :: Int -> Int;\n\
    \one_hash  = from_char '#';\n\
    \two_hash  = from_string \"##\";\n\
    \hash_prec = const 0"
    ==>
    ["hash_prec", "one_hash", "two_hash"]
  -- Single-line separated by semicolons - e.g. result of a macro expansion
  , "one_hash, two_hash :: text_type; \
    \hash_prec :: Int -> Int; \
    \one_hash  = from_char '#'; \
    \two_hash  = from_string \"##\"; \
    \hash_prec = const 0"
    ==>
    ["hash_prec", "one_hash", "two_hash"]

  , "assertDataFormatError :: DecompressError -> IO String\n\
    \assertDataFormatError (DataFormatError detail) = return detail\n\
    \assertDataFormatError _                        = assertFailure \"expected DataError\"\n\
    \                                              >> return \"\"\n"
    ==>
    ["assertDataFormatError"]

  , "instance PartialComparison Double where\n\
    \    type PartialCompareEffortIndicator Double = ()\n\
    \    pCompareEff _ a b = Just $ toPartialOrdering $ Prelude.compare a b\n\
    \--        case (isNaN a, isNaN b) of\n\
    \--           (False, False) -> Just $ toPartialOrdering $ Prelude.compare a b  \n\
    \--           (True, True) -> Just EQ\n\
    \--           _ -> Just NC \n\
    \    pCompareDefaultEffort _ = ()\n\
    \\n\
    \pComparePreludeCompare _ a b =\n\
    \    Just $ toPartialOrdering $ Prelude.compare a b\n\
    \\n\
    \propPartialComparisonReflexiveEQ :: \n\
    \    (PartialComparison t) =>\n\
    \    t -> \n\
    \    (PartialCompareEffortIndicator t) -> \n\
    \    (UniformlyOrderedSingleton t) -> \n\
    \    Bool\n\
    \propPartialComparisonReflexiveEQ _ effort (UniformlyOrderedSingleton e) = \n\
    \    case pCompareEff effort e e of Just EQ -> True; Nothing -> True; _ -> False"
    ==>
    ["pComparePreludeCompare", "propPartialComparisonReflexiveEQ"]

  , "hexQuad :: Z.Parser Int\n\
    \hexQuad = do\n\
    \  s <- Z.take 4\n\
    \  let hex n | w >= C_0 && w <= C_9 = w - C_0\n\
    \            | w >= C_a && w <= C_f = w - 87\n\
    \            | w >= C_A && w <= C_F = w - 55\n\
    \            | otherwise          = 255\n\
    \        where w = fromIntegral $ B.unsafeIndex s n\n\
    \      a = hex 0; b = hex 1; c = hex 2; d = hex 3\n\
    \  if (a .|. b .|. c .|. d) /= 255\n\
    \    then return $! d .|. (c `shiftL` 4) .|. (b `shiftL` 8) .|. (a `shiftL` 12)\n\
    \    else fail \"invalid hex escape\"\n"
    ==>
    ["hexQuad"]

  -- Semicolon within instance does not produce toplevel tags
  , "instance Path  T.Text      where readPath = Just; pathRep _ = typeRep (Proxy :: Proxy Text)"
    ==>
    []

  , "prop_bounds2 o1 w1 o2 w2 = let n1 = fromW8 w1 ; n2 = fromW8 w2 ; bs = ((o1, o2), (o1 + n1, o2 + n2)) in bs == bounds (listArray bs (take ((n1 + 1) * (n2 + 1)) (cycle [False, True, True])))"
    ==>
    ["prop_bounds2"]

  , "string :: String -> ReadP r String\n\
    \-- ^ Parses and returns the specified string.\n\
    \string this = do s <- look; scan this s\n\
    \ where\n\
    \  scan []     _               = do return this\n\
    \  scan (x:xs) (y:ys) | x == y = do get >> scan xs ys\n\
    \  scan _      _               = do pfail"
    ==>
    ["string"]

  , "-- | Get every node of a tree, put it into a list.\n\
    \climb :: Tree a -> [a]\n\
    \climb x = case x of (Empty) -> [];(Branch a Empty b) -> a : climb b;\n\
    \                    (Branch a b Empty) -> a : climb b;\n\
    \                    (Branch a b d) -> a : climb b ++ climb d"
    ==>
    ["climb"]

  , "addend hid a (NotM (App uid okh elr as)) = NotM $ App uid okh elr (f as)\n\
    \ where f (NotM ALNil) = NotM $ ALCons hid a (NotM $ ALNil)\n\
    \       f (NotM (ALCons hid a as)) = NotM $ ALCons hid a (f as)\n\
    \       f _ = __IMPOSSIBLE__\n\
    \addend _ _ _ = __IMPOSSIBLE__\n\
    \copyarg _ = False"
    ==>
    ["addend", "copyarg"]

  , "realWorldTc :: TyCon; \\\n\
    \realWorldTc = mkTyCon3 \"ghc-prim\" \"GHC.Types\" \"RealWorld\"; \\\n\
    \instance Typeable RealWorld where { typeOf _ = mkTyConApp realWorldTc [] }"
    ==>
    ["realWorldTc"]

  , "(r `mkQ` br) a = _" ==> ["mkQ"]

  , "_?_ = return unsafePerformIO" ==> ["?"]

  , "(+) :: DebMap -> String -> DebianVersion\n\
    \m + k = maybe (error (\"No version number for \" ++ show k ++ \" in \" ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)"
    ==>
    ["+"]

  , "(!) :: DebMap -> String -> DebianVersion\n\
    \_ ! k = maybe (error (\"No version number for \" ++ show k ++ \" in \" ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)"
    ==>
    ["!"]

  , "(.) :: DebMap -> String -> DebianVersion\n\
    \m . k = maybe (error (\"No version number for \" ++ show k ++ \" in \" ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)"
    ==>
    ["."]

  , "(.) :: DebMap -> String -> DebianVersion\n\
    \m . k x = maybe (error (\"No version number for \" ++ show k ++ \" in \" ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)"
    ==>
    ["."]

  , "{- 123___ -}import Data.Char;main=putStr$do{c<-\"/1 AA A A;9+ )11929 )1191A 2C9A \";e\n\
    \{-  |  -}    .(`divMod`8).(+(-32)).ord$c};f(0,0)=\"\n\";f(m,n)=m?\"  \"++n?\"_/\"\n\
    \{-  |  -}n?x=do{[1..n];x}                                    --- obfuscated\n\
    \{-\\_/ on Fairbairn, with apologies to Chris Brown. Above is / Haskell 98 -}"
    ==>
    ["?", "f", "main"]
  , "{-   456___   -}import Data.Char;main=putStr$do{c<-\"/1 AA A A;9+ )11929 )1191A 2C9A \";e\n\
    \ {- {- | -} -}  {- {- || -} -}{- {- || -} -}{- {- || -} -} {--}.(`divMod`8).(+(-32)).ord$c};f(0,0)=\"\n\";f(m,n)=m?\"  \"++n?\"_/\"\n\
    \{- {- | -} -}n?x=do{[1..n];x}                                    --- obfuscated\n\
    \{-\\_/ on Fairbairn, with apologies to Chris Brown. Above is / Haskell 98 -}"
    ==>
    ["?", "f", "main"]

  , "showComplexFloat :: Double -> Double -> String\n\
    \showComplexFloat x 0.0 = showFFloat Nothing x \"\"\n\
    \showComplexFloat 0.0 y = showFFloat Nothing y \"i\"\n\
    \showComplexFloat x y = (showFFloat Nothing x \"\") ++ (if y > 0 then \"+\" else \"\") ++ (showFFloat Nothing y \"i\")"
    ==>
    ["showComplexFloat"]

  , "createAlert            :<|>\n\
    \ getAlert              :<|>\n\
    \ deleteAlert           :<|>\n\
    \ setAlertStatus        :<|>\n\
    \ tagAlert              :<|>\n\
    \ untagAlert            :<|>\n\
    \ updateAlertAttributes = client (Proxy :: Proxy AlertApi)"
    ==>
    ["createAlert"]

  , "_g :: X -> Y" ==> ["_g"]
  , "(f . g) x = f (g x)" ==> ["."]
  , toplevelFunctionsWithoutSignatures
  ]
  where
    (==>) = testTagNames filename Vanilla
    toplevelFunctionsWithoutSignatures =
      testGroup "Toplevel functions without signatures"
      [ "$(return . map sumDeclaration $ [0..15])" ==> []
      , "$( fmap (reverse . concat) . traverse prismsForSingleType $ [1..15] )" ==> []
      , "T.makeInstances [2..6]\nx" ==> []
      , "infix 5 |+|"  ==> []
      , "infixl 5 |+|" ==> []
      , "infixr 5 |+|" ==> []
      , "f = g"        ==> ["f"]
      -- Relies on RepeatableTag.
      , "f :: a -> b -> a\n\
        \f x y = x"
        ==>
        ["f"]
      , "f x y = x"               ==> ["f"]
      , "f (x :+: y) z = x"       ==> ["f"]
      , "(x :+: y) `f` z = x"     ==> ["f"]
      , "(x :+: y) *: z = x"     ==> ["*:"]
      , "((:+:) x y) *: z = x"   ==> ["*:"]
      , "(*:) (x :+: y) z = x"   ==> ["*:"]
      , "(*:) ((:+:) x y) z = x" ==> ["*:"]
      , strictMatchTests
      , lazyMatchTests
      , atPatternsTests
      , "f x Nothing = x\n\
        \f x (Just y) = y"
        ==>
        ["f"]
      , "x `f` Nothing = x\n\
        \x `f` (Just y) = y"
        ==>
        ["f"]
      , "f x y = g x\n\
        \  where\n\
        \    g _ = y"
        ==>
        ["f"]
      , "x `f` y = x"   ==> ["f"]
      , "(|+|) x y = x" ==> ["|+|"]
      , "x |+| y = x"   ==> ["|+|"]
      , "(!) x y = x" ==> ["!"]
      , "--- my comment" ==> []
      , "foo :: Rec -> Bar\n\
        \foo Rec{..} = Bar (recField + 1)"
        ==>
        ["foo"]
      , "foo :: Rec -> Bar\n\
        \foo Rec { bar = Baz {..}} = Bar (recField + 1)"
        ==>
        ["foo"]
      -- Functions named "pattern"
      , "pattern :: Int -> String -> [Int]"           ==> ["pattern"]
      , "pattern x () = x + x"                        ==> ["pattern"]
      , "pattern, foo :: Int -> String -> [Int]"      ==> ["foo", "pattern"]
      , "foo, pattern :: Int -> String -> [Int]"      ==> ["foo", "pattern"]
      , "foo, pattern, bar :: Int -> String -> [Int]" ==> ["bar", "foo", "pattern"]
      , "pattern x = x "                              ==> ["pattern"]
      , "pattern x y = x + y"                         ==> ["pattern"]
      , "x `pattern` y = x + y"                       ==> ["pattern"]
      , "pattern x y z = x + y + z"                   ==> ["pattern"]
      -- Arguments named "forall
      , "f forall = forall + 1"                       ==> ["f"]
      ]
    strictMatchTests = testGroup "Strict match (!)"
      [ "f !x y = x"                 ==> ["f"]
      , "f x !y = x"                 ==> ["f"]
      , "f !x !y = x"                ==> ["f"]
      , "f ! x y = x"                ==> ["f"]
      -- this one is a bit controversial but it seems to be the way ghc
      -- parses it
      , "f ! x = x"                  ==> ["f"]
      , "(*:) !(x :+: y) z = x"      ==> ["*:"]
      , "(*:) !(!x :+: !y) !z = x"   ==> ["*:"]
      , "(*:) !((:+:) x y) z = x"    ==> ["*:"]
      , "(*:) !((:+:) !x !y) !z = x" ==> ["*:"]
      , "(!) :: a -> b -> a\n\
        \(!) x y = x"
        ==>
        ["!"]
      -- this is a degenerate case since even ghc treats ! here as
      -- a BangPatterns instead of operator
      , "x ! y = x" ==> ["x"]
      ]
    lazyMatchTests = testGroup "Lazy match (~)"
      [ "f ~x y = x"                 ==> ["f"]
      , "f x ~y = x"                 ==> ["f"]
      , "f ~x ~y = x"                ==> ["f"]
      , "f ~ x y = x"                ==> ["f"]
      -- this one is a bit controversial but it seems to be the way ghc
      -- parses it
      , "f ~ x = x"                  ==> ["f"]
      , "(*:) ~(x :+: y) z = x"      ==> ["*:"]
      , "(*:) ~(~x :+: ~y) ~z = x"   ==> ["*:"]
      , "(*:) ~((:+:) x y) z = x"    ==> ["*:"]
      , "(*:) ~((:+:) ~x ~y) ~z = x" ==> ["*:"]
      , "(~) :: a -> b -> a\n\
        \(~) x y = x"
        ==>
        ["~"]
      -- this is a degenerate case since even ghc treats ~ here as
      -- a BangPatterns instead of operator
      , "x ~ y = x" ==> ["x"]
      ]
    atPatternsTests = testGroup "At patterns (@)"
      [ "f z@x y    = z"                        ==> ["f"]
      , "f x   z'@y = z'"                       ==> ["f"]
      , "f z@x z'@y = z"                        ==> ["f"]
      , "f z@(Foo _) z'@y = z"                  ==> ["f"]
      , "f z@(Foo _) z'@(Bar _) = z"            ==> ["f"]
      , "f z @ x y  = z"                        ==> ["f"]
      , "f z @ (x : xs) = z: [x: xs]"           ==> ["f"]
      , "f z @ (x : zs @ xs) = z: [x: zs]"      ==> ["f"]
      , "f z @ (zz @x : zs @ xs) = z: [zz: zs]" ==> ["f"]

      , "(*:) zzz@(x :+: y) z = x"             ==> ["*:"]
      , "(*:) zzz@(zx@x :+: zy@y) zz@z = x"    ==> ["*:"]
      , "(*:) zzz@((:+:) x y) z = x"           ==> ["*:"]
      , "(*:) zzz@((:+:) zs@x zs@y) zz@z = x"  ==> ["*:"]

      , "f z@(!x) ~y = x"                       ==> ["f"]
      ]

testClass :: TestTree
testClass = testGroup "Class"
  [ "class (X x) => C a b where\n\tm :: a->b\n\tn :: c\n"          ==> ["C", "m", "n"]
  , "class (X x) ⇒ C a b where\n\tm ∷ a→b\n\tn ∷ c\n"              ==> ["C", "m", "n"]
  , "class (X x) => C a b | a -> b where\n\tm :: a->b\n\tn :: c\n" ==> ["C", "m", "n"]
  , "class (X x) ⇒ C a b | a → b where\n\tm ∷ a→b\n\tn ∷ c\n"      ==> ["C", "m", "n"]
  , "class A a where f :: X\n"                                     ==> ["A", "f"]
    -- indented inside where
  , "class X where\n\ta, (+) :: X\n"                               ==> ["+", "X", "a"]
  , "class X where\n\ta :: X\n\tb, c :: Y"                         ==> ["X", "a", "b", "c"]
  , "class X\n\twhere\n\ta :: X\n\tb, c :: Y"                      ==> ["X", "a", "b", "c"]
  , "class X\n\twhere\n\ta ::\n\t\tX\n\tb :: Y"                    ==> ["X", "a", "b"]

  , "class a :<: b where\n    f :: a -> b"                         ==> [":<:", "f"]
  , "class (:<:) a b where\n    f :: a -> b"                       ==> [":<:", "f"]
  , "class Eq a => a :<: b where\n    f :: a -> b"                 ==> [":<:", "f"]
  , "class a ~ 'Foo => a :<: b where\n    f :: a -> b"             ==> [":<:", "f"]
  , "class 'Foo ~ a => a :<: b where\n    f :: a -> b"             ==> [":<:", "f"]
  , "class (Eq a) => a :<: b where\n    f :: a -> b"               ==> [":<:", "f"]
  , "class (a ~ 'Foo) => a :<: b where\n    f :: a -> b"           ==> [":<:", "f"]
  , "class ('Foo ~ a) => a :<: b where\n    f :: a -> b"           ==> [":<:", "f"]
  , "class a :<<<: b => a :<: b where\n    f :: a -> b"            ==> [":<:", "f"]
  , "class (a :<<<: b) => a :<: b where\n    f :: a -> b"   ==> [":<:", "f"]
  , "class (a :<<<: b) ⇒ a :<: b where\n    f ∷ a → b"      ==> [":<:", "f"]
  , "class (Eq a, Ord b) => a :<: b where\n    f :: a -> b" ==> [":<:", "f"]
  , "class (Eq a, Ord b) => (a :: (* -> *) -> *) :<: b where\n    f :: a -> b"
    ==>
    [":<:", "f"]
    -- this is bizzarre
  , "class (Eq (a), Ord (f a [a])) => f `Z` a" ==> ["Z"]

  , "class A f where\n  data F f :: *\n  g :: a -> f a\n  h :: f a -> a"
    ==>
    ["A", "F", "g", "h"]
  , "class A f where\n  data F f :: *\n  mkF :: f -> F f\n  getF :: F f -> f"
    ==>
    ["A", "F", "getF", "mkF"]
  , "class A f where\n\
    \  data F f :: * -- foo\n\
    \                -- bar\n\
    \                -- baz\n\
    \  mkF  :: f -> F f\n\
    \  getF :: F f -> f"
    ==>
    ["A", "F", "getF", "mkF"]
  -- Not confused by a class context on a method.
  , "class X a where\n\tfoo :: Eq a => a -> a\n" ==> ["X", "foo"]
  , "class Category cat where\n\
    \    -- | the identity morphism\n\
    \    id :: cat a a\n\
    \ \n\
    \    -- | morphism composition\n\
    \    (.) :: cat b c -> cat a b -> cat a c" ==> [".", "Category", "id"]
  , "class Match a b where\n\
    \    pattern :: Pattern a b\n" ==> ["Match", "pattern"]
  , "class a ~~ b => (a :: k) ~ (b :: k) | a -> b, b -> a"
    ==>
    ["~"]
  , "class a ~~ b => (a :: k) ! (b :: k) | a -> b, b -> a"
    ==>
    ["!"]
  , "class A f where {\n\
    \  data F f :: * ; -- foo\n\
    \                  -- bar\n\
    \                  -- baz\n\
    \  mkF  :: f -> F f ; getF :: F f -> f ;\n\
    \} ;"
    ==>
    ["A", "F", "getF", "mkF"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testInstance :: TestTree
testInstance = testGroup "Instance"
  [ "instance Foo Quux where\n\
    \  data Bar Quux a = QBar { frob :: a }\n\
    \                  | QBaz { fizz :: String }\n\
    \                  deriving (Show)"
    ==>
    ["QBar", "QBaz", "fizz", "frob"]
  , "instance Foo Quux where\n\
    \  data Bar Quux a = QBar a | QBaz String deriving (Show)"
    ==>
    ["QBar", "QBaz"]
  , "instance Foo Quux where\n\
    \  data Bar Quux a = QBar { frob :: a }\n\
    \                  | QBaz { fizz :: String }\n\
    \                  deriving (Show)\n\
    \  data IMRuunningOutOfNamesHere Quux = Whatever"
    ==>
    ["QBar", "QBaz", "Whatever", "fizz", "frob"]
    -- in this test foo function should not affect tags found
  , "instance Foo Quux where\n\
    \  data Bar Quux a = QBar { frob :: a }\n\
    \                  | QBaz { fizz :: String }\n\
    \                  deriving (Show)\n\
    \\n\
    \  foo _ = QBaz \"hey there\""
    ==>
    ["QBar", "QBaz", "fizz", "frob"]
  , "instance Foo Int where foo _ = 1"
    ==>
    []
  , "instance Foo Quux where\n\
    \  newtype Bar Quux a = QBar a \n\
    \                     deriving (Show)\n\
    \\n\
    \  foo _ = QBaz \"hey there\""
    ==>
    ["QBar"]
  , "instance Foo Quux where\n\
    \  newtype Bar Quux a = QBar { frob :: a }"
    ==>
    ["QBar", "frob"]
  , "instance (Monoid w,MonadBaseControl b m) => MonadBaseControl b (JournalT w m) where\n\
    \   newtype StM (JournalT w m) a =\n\
    \       StMJournal { unStMJournal :: ComposeSt (JournalT w) m a }\n\
    \   liftBaseWith = defaultLiftBaseWith StMJournal\n\
    \   restoreM     = defaultRestoreM   unStMJournal\n\
    \   {-# INLINE liftBaseWith #-}\n\
    \   {-# INLINE restoreM #-}\n\
    \"
    ==>
    ["StMJournal", "unStMJournal"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testLiterate :: TestTree
testLiterate = testGroup "Literate"
  [ "> class (X x) => C a b where\n\
    \>\tm :: a->b\n\
    \>\tn :: c\n"
    ==>
    ["C", "m", "n"]
  , "Test\n\
    \\\begin{code}\n\
    \class (X x) => C a b where\n\
    \\tm :: a->b\n\
    \\tn :: c\n\
    \\\end{code}"
    ==>
    ["C", "m", "n"]
  , "> precalcClosure0 :: Grammar -> Name -> RuleList\n\
    \> precalcClosure0 g = \n\
    \>\t\\n -> case lookup n info' of\n\
    \>\t\tNothing -> []\n\
    \>\t\tJust c  -> c\n\
    \>  where"
    ==>
    ["precalcClosure0"]
  ]
  where
    (==>) = makeTest f
    f = sort . map untag . fst . processTokens . tokenize' "fn.lhs" LiterateOutside

testPatterns :: TestTree
testPatterns = testGroup "Patterns"
  [ "pattern Arrow a b = ConsT \"->\" [a, b]"
    ==>
    ["Arrow"]
  , "pattern Arrow a b = ConsT \"->\" [a, b]\n\
    \pattern Pair a b = [a, b]"
    ==>
    ["Arrow", "Pair"]
  , "pattern Sub a b = Op '-' [a, b]\n\
    \pattern Pair a b = [a, b]"
    ==>
    ["Pair", "Sub"]
  , "pattern (:++) x y = [x, y]"
    ==>
    [":++"]
  , "pattern x :** y = [x, y]"
    ==>
    [":**"]
  , "pattern Nil :: Vec2 a\n\
    \pattern Nil = Vec2 []\n"
    ==>
    ["Nil", "Nil"]
  , "pattern (:>) x xs <- ((\\ys -> (head $ unvec2 ys,Vec2 . tail $ unvec2 ys)) -> (x,xs))\n\
    \where\n\
    \   (:>) x xs = Vec2 (x:unvec2 xs)"
    ==>
    [":>"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testFFI :: TestTree
testFFI = testGroup "FFI"
  [ "foreign import ccall foo :: Double -> IO Double"            ==> ["foo"]
  , "foreign import unsafe java foo :: Double -> IO Double"      ==> ["foo"]
  , "foreign import safe stdcall foo :: Double -> IO Double"     ==> ["foo"]
  , "foreign import safe stdcall pattern :: Double -> IO Double" ==> ["pattern"]
  ]
  where
    (==>) = testTagNames filename Vanilla
