{
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2    #-}
{-# OPTIONS_GHC -Wwarn #-}

module Haskell.Language.LexerSimple.Lexer (tokenize) where

import Control.Monad
import Control.Monad.Except.Ext
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

import qualified Data.ByteString as BS
import Data.Char (chr)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext (Pretty(..), Doc, (<+>), (##))
import qualified Data.Text.Prettyprint.Doc.Ext as PP
import Data.Void (Void, absurd)
import Data.Word

import Data.IgnoreEqOrdHashNFData
import Haskell.Language.Lexer.FastTags
import Haskell.Language.Lexer.Types (LiterateStyle(..), Context(..), mkSrcPos, AlexCode(..))
import Haskell.Language.LexerSimple.LensBlaze
import Haskell.Language.LexerSimple.Types

}

$ascspace  = [\ \t\r]
$unispace  = \x01
$space     = [$ascspace $unispace]
$nl        = [\n]
$ws        = [$space\f\v] # $nl

$dot       = [\.]

$asclarge  = [A-Z]
$unilarge  = \x02
$large     = [$asclarge $unilarge]

$ascsmall  = [a-z]
$unismall  = \x03
$small     = [$ascsmall $unismall]

-- These symbols can be part of operators but are reserved when occur by
-- themselves.
$symbols_reserved_as_standalone = [ \→ \∷ \⇒ \∀ ]

$special_sym  = [\(\)\,\;\[\]\`\{\}]
$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol    = \x04
$symbol       = [$ascsymbol $unisymbol $symbols_reserved_as_standalone] # [$special_sym \_\'\"]

$ascident  = [$ascsmall $asclarge]
$uniident  = [$unismall $unilarge]
$ascdigit  = [0-9]
$unidigit  = \x05
$digit     = [$ascdigit $unidigit]
$unisuffix = \x06
$ident_nonsym = [$ascident $uniident $unisuffix $digit] # [$symbol]
$ident_syms   = [ \' \_ \# ]
$ident     = [$ident_nonsym $ident_syms]

-- Stands for "→", "∷", "⇒", "⦇", "⦈", "∀", "⟦", "⟧"
$reserved_symbol = \x07

-- $reserved_op = [→ ∷ ⇒ ∀]

@nl = ( [\r]? $nl )

@qualificationPrefix = ( $large $ident* $dot )*

@arrow       =  "->"
@doublecolon =  "::"
@implies     =  "=>"

@lbanana     =  "(|"
@rbanana     =  "|)"

$charesc    = [a b f n r t v \\ \" \' \&]
$octdigit   = [0-7]
$hexdigit   = [0-9a-fA-F]
@charescape = [\\] ( $charesc | $asclarge+ | "o" $octdigit+ | "x" $hexdigit+ )

@float_number =  ( [\+\-]? ( $digit+ ( "." $digit+ )? | $digit* "." $digit+ ) ( [eE] [\+\-]? $digit* )? )

@number = ( [\+\-]? $digit+ | 0 ( [oO] $octdigit+ | [xX] $hexdigit ) | @float_number )

@source_pragma = [Ss][Oo][Uu][Rr][Cc][Ee]

@cpp_ws          = ( $ascspace | [\\] @nl )
@cpp_opt_ws      = @cpp_ws*
@cpp_nonempty_ws = ( $ascspace @cpp_ws* | @cpp_ws* $ascspace )
@define_body     = ( [^ \\ $nl]+ | [\\] ( @nl | . ) )+ @nl

:-

-- Can skip whitespace everywhere since it does not affect meaning in any
-- state.
<0, comment, qq, literate> $ws+ ;

-- Literate Haskell support. 'literate' code handles all text except actual
-- Haskell program text. It aims to strip all non-Haskell text.
<literate> {
$nl ">" $ws*
  -- / { isLiterateEnabled' }
  { \_ len -> (Newline $! len - 2)  <$ startLiterateBird }
$nl "\begin{code}" @nl $space*
  -- / { isLiterateEnabled' }
  { \input len -> (Newline $! countInputSpace input len) <$ startLiterateLatex }
$nl ;
.
  { \_ _ -> dropUntilNL' }
}

-- Analyse "#if 0" constructs used in e.g. GHC.Base
<0> {
"#" @cpp_opt_ws "if" @cpp_opt_ws "0" ( @cpp_nonempty_ws .* )? { \_ _ -> startPreprocessorStripping }

-- Strip preprocessor
"#" @cpp_opt_ws "define" @define_body
  { kw (Newline 0) }

"#" @cpp_opt_ws ( "if" | "ifdef" | "endif" | "elif" | "else" | "undef" | "line" | "error" | "include" ) .* ( [\\] @nl .* )* ;
}

<stripCpp> {
"#" @cpp_opt_ws ( "ifdef" | "if" ) .*  { \_ _ -> startPreprocessorStripping }
"#" @cpp_opt_ws "endif" .*             { \_ _ -> endPreprocessorStripping   }
"#" @cpp_opt_ws ( "elif" | "else" | "define" | "undef" | "line" | "error" | "include" )
  { \_ _ -> dropUntilNL' }
$nl ;
.
  { \_ _ -> dropUntil' 35 -- '#'
  }
}

<0> {

$nl ">" $space*
  / { isLiterateEnabled' }
  { \_ len -> pure $! Newline $! len - 2 }
$nl [^>]
  / { isLiterateBirdOrOutside' }
  { \_ _   -> endLiterate }
$nl "\end{code}"
  / { isLiterateLatexOrOutside' }
  { \_ _   -> endLiterate }


[\\]? @nl $space* "{-"  { \input len -> startIndentationCounting (countInputSpace input len) }
[\\]? [\r] $nl $space*  { \input len -> pure $! Newline $! (len - 2) - (case chr (fromIntegral (unsafeTextHeadAscii (aiInput input))) of { '\\' -> 1; _ -> 0 }) }
[\\]? $nl $space*       { \input len -> pure $! Newline $! (len - 1) - (case chr (fromIntegral (unsafeTextHeadAscii (aiInput input))) of { '\\' -> 1; _ -> 0 }) }
[\-][\-]+ ~[$symbol $nl]
  { \_ _ -> dropUntilNL' }
[\-][\-]+ / @nl         ;

}

-- Pragmas
<0> "{-#" $ws* @source_pragma $ws* "#-}" { \_ _ -> pure $ Pragma SourcePragma }


-- Nested comments
<0, comment>
  "{-"                  { \_ _ -> startComment }
<comment> "-}"          { \_ _ -> endComment startCode }
-- 45  - '-'
-- 123 - '{'
<comment> $nl ;
<comment> .             { \_ _ -> dropUntil2' 45 123 }
<0> "-}"                { \_ _ -> errorAtLine "Unmatched -}" }

<indentComment>
  "{-"                  { \_ _ -> startIndentComment }
<indentComment> "-}"    { \_ _ -> endComment indentCountCode }
<indentComment> (. | $nl) ;

<indentCount> {
$space* "{-"            { \input len -> addIndentationSize (fromIntegral (countInputSpace input len)) *> startIndentComment }
$space*                 { \_ len -> endIndentationCounting len }
}

-- Strings
<0> [\"]                { \_ _ -> startString }
<string> ( [\\] @nl $ws* [\\] )? [\"]
                        { \_ _ -> endString startCode }
<string> [\\] @nl ( $ws* [\\] )? ;
<string> ( $ws | [^ \" \\ $nl] )+ ;
<string> ( . | $nl | [\\] . )     ;

-- Characters
<0> [\'] ( [^\'\\] | @charescape ) [\'] { kw Character }

-- Template Haskell quasiquoters

<0> "[" [\$\(]* @qualificationPrefix $ident* [\)]*  "|"
                        { \input len -> startQuasiquoter input len }
<qq> "$("               { \_ _ -> startSplice CtxQuasiquoter }
<qq> ("|]" | "⟧")       { \_ _ -> endQuasiquoter }
<qq> $reserved_symbol   { \input _len -> reservedSymbolQQ (unsafeTextHead (aiInput input)) }
<qq> (. | $nl)          ;

<0> "$("                { \_ _ -> startSplice CtxHaskell }

-- Vanilla tokens
<0> {

"#{" [^\}]+ "}"         { \_ _ -> pure HSC2HS }

"case"                  { kw KWCase }
"class"                 { kw KWClass }
"data"                  { kw KWData }
"default"               { kw KWDefault }
"deriving"              { kw KWDeriving }
"do"                    { kw KWDo }
"else"                  { kw KWElse }
"family"                { kw KWFamily }
"forall"                { \_ _ -> pure $! T "forall" }
"foreign"               { kw KWForeign }
"if"                    { kw KWIf }
"import"                { kw KWImport }
"in"                    { kw KWIn }
"infix"                 { kw KWInfix }
"infixl"                { kw KWInfixl }
"infixr"                { kw KWInfixr }
"instance"              { kw KWInstance }
"let"                   { kw KWLet }
"module"                { kw KWModule }
"newtype"               { kw KWNewtype }
"of"                    { kw KWOf }
"pattern"               { \_ _ -> pure $! T "pattern" }
"then"                  { kw KWThen }
"type"                  { kw KWType }
"where"                 { kw KWWhere }
@arrow                  { kw Arrow }
"@"                     { kw At }
"`"                     { kw Backtick }
","                     { kw Comma }
"."                     { kw Dot }
@doublecolon            { kw DoubleColon }
"="                     { kw Equals }
"!"                     { kw ExclamationMark }
@implies                { kw Implies }
"{"                     { kw LBrace }
"["                     { kw LBracket }
"("                     { pushLParen }
"|"                     { kw Pipe }
"}"                     { kw RBrace }
"]"                     { kw RBracket }
")"                     { popRParen }
"~"                     { kw Tilde }
";"                     { kw Semicolon }

[\\]                    { kw LambdaBackslash }

-- Not interested in numbers, but it takes time to extract their text so
-- it's quicker to just ignore them.
@number                 { kw Number }

[\']* @qualificationPrefix ($ident | $large)+
                        { \input len -> pure $! T $! retrieveToken input len }
@qualificationPrefix $symbol+
                        { \input len -> pure $! T $! retrieveToken input len }

$reserved_symbol        { \input _len -> reservedSymbol (unsafeTextHead (aiInput input)) }

@lbanana / ~[$symbol]   { \_ _ -> pure LBanana }
@rbanana                { \_ _ -> pure RBanana }

}

{

type AlexAction m = AlexInput -> Int -> m ServerToken
type AlexPred a = a -> AlexInput -> Int -> AlexInput -> Bool

{-# INLINE kw #-}
kw :: Applicative m => ServerToken -> AlexAction m
kw tok = \_ _ -> pure tok


isLiterateEnabled'
  :: AlexPred (LiterateLocation a)
isLiterateEnabled' litLoc _inputBefore _len _inputAfter =
  isLiterateEnabled litLoc

isLiterateBirdOrOutside'
  :: AlexPred (LiterateLocation LiterateStyle)
isLiterateBirdOrOutside' litLoc _inputBefore _len _inputAfter =
  isLiterateBirdOrOutside litLoc

isLiterateLatexOrOutside'
  :: AlexPred (LiterateLocation LiterateStyle)
isLiterateLatexOrOutside' litLoc _inputBefore _len _inputAfter =
  isLiterateLatexOrOutside litLoc

tokenize
  :: WithCallStack
  => FilePath -> LiterateLocation Void -> BS.ByteString -> [Pos ServerToken]
tokenize filename litLoc input =
  runAlexM litLoc startCode' input $ scanTokens filename
  where
    startCode' = case litLoc of
      Vanilla          -> startCode
      LiterateOutside  -> literateCode
      LiterateInside x -> absurd x

scanTokens :: WithCallStack => FilePath -> AlexM ()
scanTokens filename = go
  where
    go = do
      nextTok <- continueScanning
      case nextTok of
        EOF -> pure ()
        _   -> do
          -- Use input after reading token to get proper prefix that includes
          -- token we currently read.
          AlexState{asInput} <- get
          let !tok = Pos (mkSrcPos filename $! view aiLineL asInput) nextTok
          tell [tok]
          go

continueScanning :: WithCallStack => AlexM ServerToken
continueScanning = do
  s@AlexState{asInput} <- get
  go (view asCodeL s) (view asLiterateLocL s) asInput
  where
    go :: AlexCode -> LiterateLocation LiterateStyle -> AlexInput -> AlexM ServerToken
    go code !litLoc = go'
      where
        go' input =
          case alexScanUser litLoc input (unAlexCode code) :: AlexReturn (AlexAction AlexM) of
            AlexEOF                              ->
              pure EOF
            AlexError input@AlexInput{aiInput} -> do
              code <- gets (view asCodeL)
              pure $ Error $ IgnoreEqOrdHashNFData $ "Lexical error while in state" <+> pretty code
                <+> "at line" <+>
                pretty (unLine (view aiLineL input)) <> ":" ## PP.squotes (PP.ppByteString (utf8BS 40 aiInput))
            AlexSkip input' _                    ->
              go' input'
            AlexToken input' tokLen action       ->
              alexSetInput input' *> action input tokLen

dropUntilNL' :: AlexM ServerToken
dropUntilNL' = do
  modify $ \s -> s { asInput = dropUntilNL $ asInput s }
  continueScanning

dropUntil' :: Word8 -> AlexM ServerToken
dropUntil' w = do
  modify $ \s -> s { asInput = dropUntil w $ asInput s }
  continueScanning

dropUntil2' :: Word8 -> Word8 -> AlexM ServerToken
dropUntil2' w1 w2 = do
  modify $ \s -> s { asInput = dropUntil2 w1 w2 $ asInput s }
  continueScanning

startIndentationCounting :: WithCallStack => Int -> AlexM ServerToken
startIndentationCounting !n = do
  modify (\s -> set asCommentDepthL 1 $ set asIndentationSizeL (fromIntegral n) s)
  alexSetNextCode indentCommentCode
  continueScanning

endIndentationCounting :: Int -> AlexM ServerToken
endIndentationCounting !n = do
  alexSetNextCode startCode
  Newline . (+ n) . fromIntegral <$> gets (view asIndentationSizeL)

startIndentComment :: WithCallStack => AlexM ServerToken
startIndentComment = do
  void $ modifyCommentDepth (+1)
  alexSetNextCode indentCommentCode
  continueScanning

startPreprocessorStripping :: WithCallStack => AlexM ServerToken
startPreprocessorStripping = do
  void $ modifyPreprocessorDepth (+1)
  alexSetNextCode stripCppCode
  continueScanning

endPreprocessorStripping :: WithCallStack => AlexM ServerToken
endPreprocessorStripping = do
  newDepth <- modifyPreprocessorDepth (\x -> x - 1)
  when (newDepth == 0) $
    alexSetNextCode startCode
  continueScanning

startComment :: WithCallStack => AlexM ServerToken
startComment = do
  void $ modifyCommentDepth (+1)
  alexSetNextCode commentCode
  continueScanning

endComment :: WithCallStack => AlexCode -> AlexM ServerToken
endComment nextCode = do
  newDepth <- modifyCommentDepth (\x -> x - 1)
  when (newDepth == 0) $
    alexSetNextCode nextCode
  continueScanning

startString :: WithCallStack => AlexM ServerToken
startString =
  alexSetNextCode stringCode *> continueScanning

endString :: AlexCode -> AlexM ServerToken
endString nextCode =
  String <$ alexSetNextCode nextCode

startQuasiquoter :: AlexInput -> Int -> AlexM ServerToken
startQuasiquoter _ n
  | n == 2 = startUnconditionalQuasiQuoter
startQuasiquoter AlexInput{aiInput} _ = do
  !haveEnd     <- gets (view asHaveQQEndL)
  isEndPresent <- case haveEnd of
    Nothing    -> do
      let haveEnd' = calculateQuasiQuoteEnds aiInput
      modify $ set asHaveQQEndL (Just haveEnd')
      pure haveEnd'
    Just ends' -> pure ends'
  case isEndPresent of
    -- No chance of quasi-quote closing till the end of current file.
    -- Assume that file ought to be well-formed and treat currently
    -- matched input
    False -> pure LBracket
    True  -> startUnconditionalQuasiQuoter

startUnconditionalQuasiQuoter :: AlexM ServerToken
startUnconditionalQuasiQuoter =
  QuasiquoterStart <$ alexSetNextCode qqCode

startSplice :: Context -> AlexM ServerToken
startSplice ctx = do
  alexSetNextCode startCode
  pushContext ctx
  pure SpliceStart

endQuasiquoter :: AlexM ServerToken
endQuasiquoter =
  QuasiquoterEnd <$ alexSetNextCode startCode

pushLParen :: AlexAction AlexM
pushLParen _ _ =
  LParen <$ pushContext CtxHaskell

popRParen :: AlexAction AlexM
popRParen _ _ = do
  cs <- gets asContextStack
  case cs of
    [] -> pure ()
    c : cs' -> do
      modify $ \s -> s { asContextStack = cs' }
      alexSetNextCode $ case c of
        CtxHaskell     -> startCode
        CtxQuasiquoter -> qqCode
  pure RParen

{-# INLINE errorAtLine #-}
errorAtLine
  :: MonadState AlexState m
  => Doc Void -> m ServerToken
errorAtLine msg = do
  line <- gets (unLine . view aiLineL . asInput)
  pure $ Error $ IgnoreEqOrdHashNFData $ "Error at line" <+> pretty line <> ":" <+> msg

startLiterateBird :: AlexM ()
startLiterateBird = do
  alexSetNextCode startCode
  alexEnterBirdLiterateEnv

startLiterateLatex :: AlexM ()
startLiterateLatex = do
  alexSetNextCode startCode
  alexEnterLiterateLatexEnv

endLiterate :: AlexM ServerToken
endLiterate = do
  alexSetNextCode literateCode
  alexExitLiterateEnv
  continueScanning

reservedSymbol :: WithCallStack => Char -> AlexM ServerToken
reservedSymbol = \case
  '→' -> pure Arrow
  '∷' -> pure DoubleColon
  '⇒' -> pure Implies
  '∀' -> pure $! T $! "forall"
  '⦇' -> pure LBanana
  '⦈' -> pure RBanana
  '⟦' -> startUnconditionalQuasiQuoter
  '⟧' -> endQuasiquoter
  c   -> error $ PP.displayDocString $ "Unexpected reserved symbol:" <+> pretty c

reservedSymbolQQ :: WithCallStack => Char -> AlexM ServerToken
reservedSymbolQQ = \case
  '⟧' -> endQuasiquoter
  _   -> continueScanning

-- Known codes

{-# INLINE startCode         #-}
{-# INLINE qqCode            #-}
{-# INLINE stringCode        #-}
{-# INLINE commentCode       #-}
{-# INLINE indentCommentCode #-}
{-# INLINE indentCountCode   #-}
{-# INLINE literateCode      #-}
{-# INLINE stripCppCode      #-}
startCode, qqCode, stringCode, commentCode, indentCommentCode, indentCountCode, literateCode, stripCppCode :: AlexCode
startCode         = AlexCode 0
qqCode            = AlexCode qq
stringCode        = AlexCode string
commentCode       = AlexCode comment
indentCommentCode = AlexCode indentComment
indentCountCode   = AlexCode indentCount
literateCode      = AlexCode literate
stripCppCode      = AlexCode stripCpp

}
