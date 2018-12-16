{
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Very important to have this one as it enables GHC to infer proper type of
-- Alex 3.2.1 actions.
--
-- The basic type is (Monad m => AlexInput -> Int -> AlexT m TokenVal), but
-- monomorphism restriction breaks its inference.
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wwarn #-}

module Haskell.Language.Lexer.Lexer (tokenizeM) where

import Control.Monad
import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Profunctor (lmap)
import Data.Semigroup as Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc (Doc, Pretty(..), (<+>))
import Data.Text.Prettyprint.Doc.Ext
import Data.Void (Void)

import Data.ErrorMessage
import qualified Data.KeyMap as KM
import Data.Symbols.MacroName (mkMacroName)
import Haskell.Language.Lexer.Env
import Haskell.Language.Lexer.FastTags (PragmaType(..), ServerToken(..), Pos(..), unLine, valOf)
import Haskell.Language.Lexer.Input (AlexInput, aiInput, aiLine, alexInputPrevChar, alexGetByte, retrieveToken)
import qualified Haskell.Language.Lexer.InputStack as InputStack
import Haskell.Language.Lexer.Monad
import Haskell.Language.Lexer.Preprocessor
import Haskell.Language.Lexer.RulePredicate
import Haskell.Language.Lexer.State
import Haskell.Language.Lexer.Types

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
$ident_syms   = [\'\_\#]
$ident     = [$ident_nonsym $ident_syms]

@nl = ( [\r]? $nl )

$cpp_ident       = [ $ascsmall $asclarge $ascdigit \_ \' \` ]
@cpp_ident       = [ $ascsmall $asclarge \_ \' \` ] $cpp_ident*
@cpp_ident_split = [ $ascsmall $asclarge \_ \' \` ] ( $cpp_ident | [\\] @nl )*

@cpp_ws          = ( $ascspace | [\\] @nl )
@cpp_opt_ws      = @cpp_ws*
@cpp_nonempty_ws = ( $ascspace @cpp_ws* | @cpp_ws* $ascspace )
-- NB must include newline so that full line will be replaced by a Newline token.
-- We'll strip trailing newline during parsing so it won't cause any troubles.
@define_body     = ( [^\\$nl] | [\\] @nl )+ @nl

-- $reserved_op = [→ ∷ ⇒ ∀]

@qualificationPrefix = ( $large $ident* $dot )*

@arrow       = ( "->" | "→" )
@doublecolon = ( "::" | "∷" )
@implies     = ( "=>" | "⇒" )

$charesc    = [a b f n r t v \\ \" \' \&]
$octdigit   = [0-7]
$hexdigit   = [0-9a-fA-F]
@charescape = [\\] ( $charesc | $asclarge+ | "o" $octdigit+ | "x" $hexdigit+ )

@float_number =  ( [\+\-]? ( $digit+ ( "." $digit+ )? | $digit* "." $digit+ ) ( [eE] [\+\-]? $digit* )? )

@number = ( [\+\-]? $digit+ | 0 ([oO] $octdigit+ | [xX] $hexdigit ) | @float_number )

@source_pragma = [Ss][Oo][Uu][Rr][Cc][Ee]

$filename = [a-z A-Z 0-9 \- \_ \. \\]
@filename = $filename+

@character = [\'] ( [^\'\\] | @charescape ) [\']
@string    = [\"] ( [^\"\\] | @charescape | [\\] @nl ( $ws* [\\] )? )* [\"]

:-

-- Can skip whitespace everywhere since it does not affect meaning in any
-- state.
<0, macroDefined, comment, qq, literate> $ws+ ;

-- Literate Haskell support
<literate> {
^ > $ws*
  / { runRulePredM' (lmap predAlexEnv isLiterate) }
  { \_ len -> startLiterateBird  *> pure (one $! Newline $! len - 1) }
^ "\begin{code}" @nl
  / { runRulePredM' (lmap predAlexEnv isLiterate) }
  { \_ len -> startLiterateLatex *> pure (one $! Newline $! len - 12) }
(. | @nl)
  ;
}

<0, macroDefined> {

@nl > $space*
  / { runRulePredM' (lmap predAlexEnv isLiterate) }
  { \_ len -> pure $! one $! Newline $! len - 2 }
@nl [^>]
  / { runRulePredM' (isLiterate .&&&. isInBirdEnv) }
  { \_ _   -> endLiterate }
^ "\end{code}"
  / { runRulePredM' (isLiterate .&&&. isInLatexCodeEnv) }
  { \_ _   -> endLiterate }

}

-- Line comments and pragmas
<0, macroDefined> {

$nl $space*             { \_ len -> pure $! one $! Newline $! len - 1 }

[\-][\-]+ ~[$symbol $nl] .* ;
[\-][\-]+ / $nl         ;

-- Pragmas
"{-#" $ws* @source_pragma $ws* "#-}" { \_ _ -> pure $! one $! Pragma SourcePragma }

}

-- Recursive comments, {- ... -}
<0, macroDefined, comment> "{-"
  { \_ _ -> startRecursiveComment }
<comment> "-}"
  { \_ _ -> endRecursiveComment }
<comment> (. | @nl)
  ;
<0, macroDefined> "-}"
  { \_ _ -> errorAtLine "Unmatched -}" }

-- Characters and strings
<0, macroDefined> @character { kw Character }
<0, macroDefined> @string    { kw String }

-- Template Haskell quasiquoters

<0, macroDefined> "[" $ident* "|"
  { \_ _ -> startQuasiquoter }
<qq> "$("               { \_ _ -> startSplice CtxQuasiquoter }
<qq> "|]"               { \_ _ -> endQuasiquoter }
<qq> (. | @nl)          ;

<0, macroDefined> "$("  { \_ _ -> startSplice CtxHaskell }

-- Preprocessor

-- Pragmas
<0, macroDefined> {

-- "#" @cpp_opt_ws "include" @cpp_nonempty_ws "<" @filename ">" @nl
--   { \input len -> do
--       throwErrorWithCallStack "TODO"
--       pure $ Newline 0
--   }
--
-- "#" @cpp_opt_ws "include" @cpp_nonempty_ws [\"] @filename [\"] @nl
--   { \input len -> do
--       throwErrorWithCallStack "TODO - #include <foo>"
--       pure $ Newline 0
--   }
--
-- So-called computed includes, when filename to be included comes from a
-- macro. Not sure we should implement them yet...
-- "#" @cpp_opt_ws "include" @cpp_nonempty_ws @cpp_ident_split
--   { \input len -> do
--       throwErrorWithCallStack "TODO - #include \"foo\""
--       pure $ Newline 0
--   }
--
-- "#" @cpp_opt_ws "include_next" @cpp_nonempty_ws "<" @filename ">" @nl
--   { \input len -> do
--       throwErrorWithCallStack "TODO - #include_next <foo>"
--       pure $ Newline 0
--   }
--
-- "#" @cpp_opt_ws "include_next" @cpp_nonempty_ws [\"] @filename [\"] @nl
--   { \input len -> do
--       throwErrorWithCallStack "TODO - #include_next \"foo\""
--       pure $ Newline 0
--   }
--
-- "#" @cpp_opt_ws ( "warning" | "error" ) @cpp_nonempty_ws [\"] ( . | [\\] [\"] ) * [\"] @nl
--   { \input len -> do
--       throwErrorWithCallStack "TODO - warnings / errors"
--       pure $ Newline 0
--   }
--
-- "#" @cpp_opt_ws  "pragma" @cpp_nonempty_ws .* @nl
--   { \input len -> do
--       throwErrorWithCallStack "TODO - #pragma"
--       pure $ Newline 0
--   }
--
-- "#" @cpp_opt_ws  "line" @cpp_nonempty_ws .* @nl
--   { \input len -> do
--       throwErrorWithCallStack "TODO - #line"
--       pure $ Newline 0
--   }

-- #define FOO(x, y, z)
"#" @cpp_opt_ws
  "define" @cpp_nonempty_ws
  @cpp_ident_split
  ( "(" @cpp_opt_ws ( @cpp_ident_split ( @cpp_opt_ws "," @cpp_opt_ws @cpp_ident_split )* @cpp_opt_ws )? ")" )?
  @cpp_nonempty_ws @define_body
  { \input len -> do
      macro <- parsePreprocessorDefine $! retrieveToken input len
      addMacroDef macro
      alexChangeToplevelCode macroDefinedCode
      alexSetCode macroDefinedCode
      pure $ one $ Newline 0
  }

-- #undef FOO
"#" @cpp_opt_ws
  "undef" @cpp_nonempty_ws
  @cpp_ident_split
  { \input len -> do
      name <- parsePreprocessorUndef $! retrieveToken input len
      removeMacroDef name
      pure $ one $ Newline 0
  }

}

-- Expansion of macro definitions. Macro definitions *must* be
-- expanded *after* strings, comments and quasiquotes are
-- lexed. Except quasiquotes, C preprocessor does not expand within
-- strings or comments. It's not absolutely clear what should happen
-- with quasiquotes, but it's definitely possible (and easier) to
-- pretend they're strings, really. However, 'cpp' may disagree...
<macroDefined, comment, qq> {

-- Expand macro function application
@cpp_ident "(" @cpp_opt_ws
  -- / { runRulePredM (lmap predAlexState (matchedNameInPredicate >>= isNameDefinedAsFunction . mkMacroName . T.takeWhile (/= '('))) }
  { \input len -> resolvePossibleFunctionMacroApplication input len }

}

<readMacroCallArguments> {

")"
  { \_input _len -> do
    s <- get
    if asMacroArgsParenDepth s <= 1
    then do
      alexSetCode $ asCodeBeforeParsingMacroArgs s
      let realArgs = reverse $ asMacroArgs s
      enterFunctionMacroDef (asFunctionMacroDef s) realArgs
      modify $ \s' -> s'
        { asFunctionMacroDef    = defaultFunctionMacroDef
        , asMacroArgsParenDepth = 0
        , asMacroArgs           = []
        }
    else do
      addToCurrentMacroArg ")"
      modify $ \s' -> s'
        { asMacroArgsParenDepth = max 0 (asMacroArgsParenDepth s' - 1) }
    continueScanning
  }

"(" [^ \( \) \' \" ]*
  { \input len -> do
    addToCurrentMacroArg $ retrieveToken input len
    modify $ \s -> s { asMacroArgsParenDepth = asMacroArgsParenDepth s + 1 }
    continueScanning
  }

( [^ \( \) \, \' \" ] )+ -- | [ \n ]
  { \input len -> do
    addToCurrentMacroArg $ retrieveToken input len
    continueScanning
  }

( @character | @string )
  { \input len -> do
    addToCurrentMacroArg $ retrieveToken input len
    continueScanning
  }

","
  { \_input _len -> do
    addNewMacroArg
    continueScanning
  }

}

-- <macroDefined, comment, qq> {
<macroDefined> {

-- Expand macro constant
@cpp_ident
  { \input len ->
      resolvePossibleMacroArgumentOrConstantMacro input $ retrieveToken input len
  }

  -- / { runRulePredM (lmap predAlexState (matchedNameInPredicate >>= isNameDefinedAsConstant . mkMacroName)) }
  -- / { runRulePredM (do
  --       name <- matchedNameInPredicate
  --       isConstant <- lmap predAlexState $ isNameDefinedAsConstant $ mkMacroName name) }

}

-- Vanilla tokens
<0, macroDefined> {

"case"                  { kw KWCase }
"class"                 { kw KWClass }
"data"                  { kw KWData }
"default"               { kw KWDefault }
"deriving"              { kw KWDeriving }
"do"                    { kw KWDo }
"else"                  { kw KWElse }
"family"                { kw KWFamily }
"forall"                { \_ _ -> pure $ one $ T "forall" }
"∀"                     { \_ _ -> pure $ one $ T "forall" }
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
"pattern"               { \_ _ -> return $ one $ T "pattern" }
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

@qualificationPrefix ( $ident+ | $symbol+ )
                        { \input len -> pure $ one $ T $ retrieveToken input len }

}

{

foo :: a -> a
foo x = x

-- -- Like (NonEmpty TokenVal)
-- data SomeTokens = SomeTokens !TokenVal ![TokenVal]

type AlexAction m = AlexInput -> Int -> m (NonEmpty ServerToken)

one :: a -> NonEmpty a
one x = x :| []

kw :: Applicative m => ServerToken -> AlexAction m
kw tok = \_ _ -> pure $ one tok

tokenizeM
  :: (WithCallStack, Monad m)
  => FilePath -> LiterateMode -> Text -> m (Either ErrorMessage [Pos ServerToken])
tokenizeM filename mode input =
  runAlexT filename mode code toplevelCode input scanTokens
  where
    code :: AlexCode
    code = case mode of
      Vanilla  -> startCode
      Literate -> literateCode
    -- Use 'startCode' as a toplevel until a macro will be defined.
    toplevelCode :: AlexCode
    toplevelCode = startCode

-- TODO: add unsafe interleave here for producing tokens
scanTokens :: (WithCallStack, Monad m) => AlexT m [Pos ServerToken]
scanTokens = do
  toks <- alexMonadScan
  case valOf $ NE.last toks of
    EOF -> pure []
    _   -> (toList toks <>) <$> scanTokens

alexMonadScan :: (WithCallStack, Monad m) => AlexT m (NonEmpty (Pos ServerToken))
alexMonadScan = do
  filename <- asks aeFilename
  line     <- gets (aiLine . asInput)
  fmap (Pos (mkSrcPos filename line)) <$> continueScanning

continueScanning :: forall m. (WithCallStack, Monad m) => AlexT m (NonEmpty ServerToken)
continueScanning = do
  env   <- ask
  state <- get
  let predEnv = PredEnv env state
  let scanResult :: AlexReturn (AlexAction (AlexT m))
      scanResult = alexScanUser predEnv (asInput state) (unAlexCode (asCode state))
  toks <- case scanResult of
    AlexEOF                       ->
      pure $ one EOF
    AlexError input               -> do
      state' <- get
      throwErrorWithCallStack $
        "lexical error while in state" <+> pretty (asCode state') <+>
        "at line" <+> pretty (unLine (aiLine input)) <> ":" <+>
        PP.squotes (pretty (TL.fromStrict (InputStack.take 40 (aiInput input))))
    AlexSkip input _              -> do
      alexSetInput input
      continueScanning
    AlexToken input tokLen action -> do
      alexSetInput input
      action (asInput state) tokLen
  pure toks

startRecursiveComment :: Monad m => AlexT m (NonEmpty ServerToken)
startRecursiveComment = do
  void $ modifyCommentDepth (+1)
  alexSetCode commentCode
  continueScanning

endRecursiveComment :: Monad m => AlexT m (NonEmpty ServerToken)
endRecursiveComment = do
  newDepth <- modifyCommentDepth (\x -> max 0 (x - 1))
  when (newDepth == 0) $
    alexSetToplevelCode
  continueScanning

startQuasiquoter :: Monad m => AlexT m (NonEmpty ServerToken)
startQuasiquoter = do
  alexSetCode qqCode
  pure $ one QuasiquoterStart

startSplice :: Monad m => Context -> AlexT m (NonEmpty ServerToken)
startSplice ctx = do
  alexSetToplevelCode
  pushContext ctx
  pure $ one SpliceStart

endQuasiquoter :: Monad m => AlexT m (NonEmpty ServerToken)
endQuasiquoter = do
  alexSetToplevelCode
  pure $ one QuasiquoterEnd

pushLParen :: Monad m => AlexAction (AlexT m)
pushLParen _ _ = do
  pushContext CtxHaskell
  pure $ one LParen

popRParen :: Monad m => AlexAction (AlexT m)
popRParen _ _ = do
  cs <- gets asContextStack
  case cs of
    []      -> pure ()
    c : cs' -> do
      modify $ \s -> s { asContextStack = cs' }
      case c of
        CtxHaskell     -> alexSetToplevelCode
        CtxQuasiquoter -> alexSetCode qqCode
  pure $ one RParen

popContext
  :: (WithCallStack, MonadState AlexState m, MonadError ErrorMessage m)
  => m (Maybe Context)
popContext = do
  cs <- gets asContextStack
  case cs of
    []      -> pure Nothing
    c : cs' -> do
      modify $ \s -> s { asContextStack = cs' }
      pure $ Just c


errorAtLine
  :: (WithCallStack, MonadError ErrorMessage m, MonadState AlexState m)
  => Doc Void -> m a
errorAtLine msg = do
  line <- gets (unLine . aiLine . asInput)
  throwErrorWithCallStack $ pretty line Semigroup.<> ":" <+> msg

startLiterateBird :: Monad m => AlexT m ()
startLiterateBird = do
  alexSetToplevelCode
  alexEnterBirdLiterateEnv

startLiterateLatex :: Monad m => AlexT m ()
startLiterateLatex = do
  alexSetToplevelCode
  alexEnterLatexCodeEnv

endLiterate :: Monad m => AlexT m (NonEmpty ServerToken)
endLiterate = do
  alexSetCode literateCode
  alexExitLiterateEnv
  continueScanning

-- Alex codes

startCode :: AlexCode
startCode = AlexCode 0

literateCode :: AlexCode
literateCode = AlexCode literate

commentCode :: AlexCode
commentCode = AlexCode comment

qqCode :: AlexCode
qqCode = AlexCode qq

macroDefinedCode :: AlexCode
macroDefinedCode = AlexCode macroDefined

readMacroCallArgumentsCode :: AlexCode
readMacroCallArgumentsCode = AlexCode readMacroCallArguments

data PredEnv = PredEnv
  { predAlexEnv   :: AlexEnv
  , predAlexState :: AlexState
  } deriving (Show)

(.&&&.)
  :: RulePredM AlexEnv Bool
  -> RulePredM AlexState Bool
  -> RulePredM PredEnv Bool
(.&&&.) x y = (&&) <$> lmap predAlexEnv x <*> lmap predAlexState y

-- TODO: alex does not like two adjacent single quotes, even in comments!

resolvePossibleMacroArgumentOrConstantMacro
  :: (WithCallStack, Monad m)
  => AlexInput -> Text -> AlexT m (NonEmpty ServerToken)
resolvePossibleMacroArgumentOrConstantMacro input matchedText = do
  let macroName   = mkMacroName matchedText
  case InputStack.lookupMacroArg macroName $ aiInput input of
    Just body -> do
      enterConstantMacroDef $ ConstantMacroDef
        { cmdName = macroName
        , cmdBody = body
        }
      continueScanning
    Nothing   -> do
      defs               <- gets $ KM.lookup macroName . asDefines
      isConstantMacroDef <- case defs of
        Nothing    -> pure Nothing
        Just defs1 ->
          case mapMaybe extractConstant $ toList defs1 of
            []      -> pure Nothing
            [def]   -> pure $ Just def
            defs2   -> throwErrorWithCallStack $ PP.vsep
              [ PP.hsep
                  [ "The macro name"
                  , PP.squotes $ pretty macroName
                  , "defines multiple constant macros"
                  ]
              , PP.indent 2 $ ppList defs2
              ]
      case isConstantMacroDef of
        Nothing  ->
          -- Neither macro nor argument, return token for matched text.
          pure $ one $ T matchedText
        Just def -> do
          enterConstantMacroDef def
          continueScanning

resolvePossibleFunctionMacroApplication
  :: (WithCallStack, Monad m)
  => AlexAction (AlexT m)
resolvePossibleFunctionMacroApplication input len = do
  let functionName :: Text
      functionName = T.init
                   $ T.dropWhileEnd (\c -> c /= '(')
                   $ retrieveToken input len
      macroName    = mkMacroName functionName
  defs               <- gets $ KM.lookup macroName . asDefines
  isFunctionMacroDef <- case defs of
    Nothing    -> pure Nothing
    Just defs1 ->
      case mapMaybe extractFunction $ toList defs1 of
        []     -> pure Nothing
        [def]  -> pure $ Just def
        defs2  -> throwErrorWithCallStack $ PP.vsep
          [ PP.hsep
              [ "The macro name"
              , PP.squotes $ pretty macroName
              , "defines multiple function macros:"
              ]
          , PP.indent 2 $ ppList defs2
          ]
  case isFunctionMacroDef of
    Nothing  ->
      -- Not a function, but possibly a constant.
      (<> one LParen) <$> resolvePossibleMacroArgumentOrConstantMacro input functionName
    Just def -> do
      modify $ \s -> s
        { asCodeBeforeParsingMacroArgs = asCode s
        , asFunctionMacroDef           = def
        , asMacroArgsParenDepth        = 1 -- Encountered first paren in this rule.
        , asMacroArgs                  = []
        }
      alexSetCode readMacroCallArgumentsCode
      continueScanning

}

-- Types for Alex actions

-- alex_actions :: Monad m => Array Int (AlexAction (AlexT m))
-- alex_action_1 :: Monad m => AlexAction (AlexT m)

