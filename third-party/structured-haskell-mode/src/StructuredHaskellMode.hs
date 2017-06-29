----------------------------------------------------------------------------
-- |
-- Module      :  StructuredHaskellMode
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  24 June 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module StructuredHaskellMode
  ( ParseType(..)
  , Extension
  , SourceSpan(..)
  , CompositeType
  , ctConstructor
  , ctParams
  , ConstructorName
  , unConstructorName
  , ParseError
  , SourceCode(..)
  , parseErrorMessage
  , parseErrorLine
  , parseErrorColumn
  , parseSpans
  , check
  , getExtensions
  ) where

import Control.Monad (foldM)
import Control.Monad.Except.Ext (MonadError, throwError, throwErrorWithBacktrace)
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Data
import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Stack
import Language.Haskell.Exts hiding (Pretty)

import Data.ErrorMessage
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Ext

-- | A generic Dynamic-like constructor -- but more convenient to
-- write and pattern match on.
data D = forall a. Data a => D a

-- | A parser. Presently there is only 'parseTopLevel', but in the
-- past, and in the future, there will be the facility to parse
-- specific parse of declarations, rather than re-parsing the whole
-- declaration which can be both slow and brittle.
type Parser a = ParseMode -> SourceCode -> ParseResult a

(<||>) :: ParseResult a -> ParseResult a -> ParseResult a
(<||>) x y = case x of
  ParseFailed{} -> y
  _             -> x

-- | Thing to parse.
data ParseType = Decl | Stmt

data ParseError = ParseError
  { parseErrorMessage :: String
  , parseErrorLine    :: Int
  , parseErrorColumn  :: Int
  }

newtype SourceCode = SourceCode { unSourceCode :: String }

newtype SourceCodeLines = SourceCodeLines { _unSourceCodeLines :: [Text] }

toSourceCodeLines :: SourceCode -> SourceCodeLines
toSourceCodeLines (SourceCode xs) = SourceCodeLines $ T.lines $ T.pack xs

parseSpans
  :: (HasCallStack, MonadError ParseError m)
  => ParseType -> Set Extension -> SourceCode -> m [SourceSpan]
parseSpans typ exts code =
  (\(mode, D x) -> genHSE codeLines mode x) <$> runParser (chooseParser typ) exts code
  where
    codeLines = toSourceCodeLines code

check
  :: (HasCallStack, MonadError ParseError m)
  => ParseType -> Set Extension -> SourceCode -> m ()
check typ exts code = () <$ runParser (chooseParser typ) exts code

chooseParser :: ParseType -> Parser D
chooseParser = \case
  Decl -> parseTopLevel
  Stmt -> parseSomeStmt

-- | Output AST info for the given Haskell code.
runParser
  :: (HasCallStack, MonadError ParseError m)
  => Parser a
  -> Set Extension
  -> SourceCode
  -> m (ParseMode, a)
runParser parser exts code =
  case parser mode code of
    ParseFailed loc e ->
      throwError ParseError
        { parseErrorMessage = e
        , parseErrorLine    = srcLine loc
        , parseErrorColumn  = srcColumn loc
        }
    ParseOk x         ->
      pure (mode, x)
  where
    mode :: ParseMode
    mode = parseMode { extensions = toList exts }

-- | An umbrella parser to parse:
--
-- * A declaration.
--
-- * An import line (not normally counted as a declaration).
--
-- * A module header (not normally counted either).
--
-- * A module pragma (normally part of the module header).
--
parseTopLevel :: Parser D
parseTopLevel mode (SourceCode code) =
  ((D . fix) <$> parseDeclWithMode mode code)   <||>
  (D <$> parseImport mode code)                 <||>
  ((D . fix) <$> parseModuleWithMode mode code) <||>
  (D <$> parseModulePragma mode code)

-- | Parse a do-notation statement.
parseSomeStmt :: Parser D
parseSomeStmt mode (SourceCode code) =
  ((D . fix) <$> parseStmtWithMode mode code) <||>
  ((D . fix) <$> parseExpWithMode mode code)  <||>
  (D <$> parseImport mode code)               <||>
  (D <$> parseExportSpecs mode code)

-- | Apply fixities after parsing.
fix :: AppFixity a => a SrcSpanInfo -> a SrcSpanInfo
fix ast = fromMaybe ast (applyFixities baseFixities ast)

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode = defaultParseMode
  { extensions = toList defaultExtensions
  -- disable fixities so that unfamiliar operators do not confuse the parser
  , fixities   = Nothing
  }

-- | Generate a list of spans from the HSE AST.
genHSE :: Data a => SourceCodeLines -> ParseMode -> a -> [SourceSpan]
genHSE sourceCode mode = go
  where
    go :: Data b => b -> [SourceSpan]
    go x =
      case gmapQ D x of
        zs@(D y:ys) ->
          case cast y of
            Just s -> concat
              [ [ spanHSE
                    sourceCode
                    (RealType (typeOf x))
                    (ConstructorName $ T.pack $ showConstr $ toConstr x)
                    (srcInfoSpan s)
                ]
              , concatMap (\(i, D d) -> pre sourceCode x i ++ go d) (zip [0..] ys)
              , post sourceCode mode x
              ]
            _      -> concatMap (\(D d) -> go d) zs
        _ -> []

-- | Pre-children tweaks for a given parent at index i.
--
pre :: Typeable a => SourceCodeLines -> a -> Integer -> [SourceSpan]
pre sourceCode x i = case cast x :: Maybe (Exp SrcSpanInfo) of
  -- <foo { <foo = 1> }> becomes <foo <{ <foo = 1> }>>
  Just (RecUpdate SrcSpanInfo{ srcInfoPoints = start:_, srcInfoSpan = end } _ _)
    | i == 1 ->
      [ spanHSE
          sourceCode
          (FakeType RecUpdates)
          (ConstructorName "RecUpdates")
          (SrcSpan
             (srcSpanFilename start)
             (srcSpanStartLine start)
             (srcSpanStartColumn start)
             (srcSpanEndLine end)
             (srcSpanEndColumn end))
      ]
  _ ->
    case cast x :: Maybe (Deriving SrcSpanInfo) of
      -- <deriving (X,Y,Z)> becomes <deriving (<X,Y,Z>)
      Just (Deriving _ ds@(_:_)) ->
        [ spanHSE
            sourceCode
            (FakeType InstHeads)
            (ConstructorName "InstHeads")
            (SrcSpan
               (srcSpanFilename start)
               (srcSpanStartLine start)
               (srcSpanStartColumn start)
               (srcSpanEndLine end)
               (srcSpanEndColumn end))
        | Just (IRule _ _ _ (IHCon (SrcSpanInfo start _) _)) <- [listToMaybe ds]
        , Just (IRule _ _ _ (IHCon (SrcSpanInfo end _) _))   <- [listToMaybe (reverse ds)]
        ]
      _ -> []

-- | Post-node tweaks for a parent, e.g. adding more children.
post :: Typeable a => SourceCodeLines -> ParseMode -> a ->  [SourceSpan]
post sourceCode mode x =
  case cast x of
    Just (QuasiQuote (base :: SrcSpanInfo) qname content) ->
      case parseExpWithMode mode content of
        ParseOk ex    -> genHSE sourceCode mode $ redelta qname base <$> ex
        ParseFailed{} -> []
    _ -> []

-- | Apply a delta to the positions in the given span from the base.
redelta :: String -> SrcSpanInfo -> SrcSpanInfo -> SrcSpanInfo
redelta qname base (SrcSpanInfo (SrcSpan fp sl sc el ec) pts) =
  SrcSpanInfo
    (if sl == 1
        then SrcSpan fp
                     (sl + lineOffset)
                     (sc + columnOffset)
                     (el + lineOffset)
                     (if el == sl
                         then ec + columnOffset
                         else ec)
        else SrcSpan fp
                     (sl + lineOffset)
                     sc
                     (el + lineOffset)
                     ec)
    pts
  where lineOffset = sl' - 1
        columnOffset =
          sc' - 1 +
          length ("[" :: String) +
          length qname +
          length ("|" :: String)
        (SrcSpanInfo (SrcSpan _ sl' sc' _ _) _) = base

data CompositeType = CompositeType
  { ctConstructor :: !Text -- unqualified
  , ctParams      :: [Text]
  }

instance Pretty CompositeType where
  pretty CompositeType{ctConstructor, ctParams} =
    pretty ctConstructor <+> hsep (map pretty ctParams)

data SourceSpan = SourceSpan
  { ssType          :: !CompositeType
  , ssConstructor   :: !ConstructorName
  , ssMatchedSource :: !Text
  , ssStartLine     :: !Int
  , ssStartColumn   :: !Int
  , ssEndLine       :: !Int
  , ssEndColumn     :: !Int
  }

instance Pretty SourceSpan where
  pretty SourceSpan{ssType, ssConstructor, ssMatchedSource, ssStartLine, ssStartColumn, ssEndLine, ssEndColumn} =
    ppDict "SourceSpan"
      [ "ssType"          :-> pretty ssType
      , "ssConstructor"   :-> pretty ssConstructor
      , "ssMatchedSource" :-> pretty ssMatchedSource
      , "ssStartLine"     :-> pretty ssStartLine
      , "ssStartColumn"   :-> pretty ssStartColumn
      , "ssEndLine"       :-> pretty ssEndLine
      , "ssEndColumn"     :-> pretty ssEndColumn
      ]

data Lines a =
    SingleLine a
  | MultipleLines a [a] a

splitIntoLines :: forall a. [a] -> Maybe (Lines a)
splitIntoLines = \case
  []          -> Nothing
  [x]         -> Just $ SingleLine x
  x : x' : xs -> Just $ go x mempty x' xs
  where
    go :: a -> DList a -> a -> [a] -> Lines a
    go h acc t []       = MultipleLines h (toList acc) t
    go h acc t (x : xs) = go h (DL.snoc acc t) x xs

getSourceCoveredBySpan :: HasCallStack => SrcSpan -> SourceCodeLines -> Text
getSourceCoveredBySpan span@SrcSpan{srcSpanStartLine, srcSpanStartColumn, srcSpanEndLine, srcSpanEndColumn} (SourceCodeLines codeLines) =
  case splitIntoLines relevantLines of
    Nothing -> error $ "Span '" ++ show span ++ "' covers 0 lines."
    Just x  -> case x of
      SingleLine line      -> T.take columnsDelta $ T.drop (srcSpanStartColumn - 1) line
      MultipleLines h xs t -> T.concat $
        [T.drop (srcSpanStartColumn - 1) h] ++ xs ++ [T.take srcSpanEndColumn t]
  where
    relevantLines
      = take linesDelta
      $ drop (srcSpanStartLine - 1)
      $ codeLines
    linesDelta   = srcSpanEndLine - srcSpanStartLine + 1
    columnsDelta = srcSpanEndColumn - srcSpanStartColumn + 1

data FakeType = RecUpdates | InstHeads
  deriving (Eq, Ord, Show)

data TypeName =
    RealType TypeRep
  | FakeType FakeType
  deriving (Eq, Ord, Show)

typeNameToTypeApp :: TypeName -> CompositeType
typeNameToTypeApp = \case
  RealType rep -> CompositeType
    { ctConstructor = unqual $ T.pack $ show typeCons
    , ctParams      = map (unqual . T.pack . show) typeParams
    }
    where
      (typeCons, typeParams) = splitTyConApp rep
      unqual = T.takeWhileEnd (/= '.')
  FakeType typ -> CompositeType
    { ctConstructor =
        case typ of
          RecUpdates -> "RecUpdates"
          InstHeads  -> "InstHeads"
    , ctParams      = []
    }

newtype ConstructorName = ConstructorName { unConstructorName :: Text }
  deriving (Eq, Ord, Pretty, Show)

-- | Generate a span from a HSE SrcSpan.
spanHSE :: SourceCodeLines -> TypeName -> ConstructorName -> SrcSpan -> SourceSpan
spanHSE sourceCode typ cons span@SrcSpan{..} = SourceSpan
  { ssType          = typeNameToTypeApp typ
  , ssConstructor   = cons
  , ssMatchedSource = getSourceCoveredBySpan span sourceCode
  , ssStartLine     = srcSpanStartLine
  , ssStartColumn   = srcSpanStartColumn
  , ssEndLine       = srcSpanEndLine
  , ssEndColumn     = srcSpanEndColumn
  }

--------------------------------------------------------------------------------
-- Parsers that HSE hackage doesn't have

parseExportSpecs :: ParseMode -> String -> ParseResult [ExportSpec SrcSpanInfo]
parseExportSpecs mode code =
  case parseModuleWithMode mode code of
    ParseOk (Module _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ xs)))) _ _ _) -> return xs
    ParseOk _ -> ParseFailed noLoc "parseExportSpecs"
    ParseFailed x y -> ParseFailed x y

parseImport :: ParseMode -> String -> ParseResult (ImportDecl SrcSpanInfo)
parseImport mode code =
  case parseModuleWithMode mode code of
    ParseOk (Module _ _ _ [i] _) -> return i
    ParseOk _ -> ParseFailed noLoc "parseImport"
    ParseFailed x y -> ParseFailed x y

parseModulePragma :: ParseMode -> String -> ParseResult (ModulePragma SrcSpanInfo)
parseModulePragma mode code =
  case parseModuleWithMode mode (code ++ "\nmodule X where") of
    ParseOk (Module _ _ [p] _ _) -> return p
    ParseOk _ -> ParseFailed noLoc "parseModulePragma"
    ParseFailed x y -> ParseFailed x y

--------------------------------------------------------------------------------
-- Extensions stuff stolen from hlint

-- | Consume an extensions list from arguments.
getExtensions
  :: forall m. (HasCallStack, MonadError ErrorMessage m)
  => [Text] -> m (Set Extension)
getExtensions = foldM f defaultExtensions . map T.unpack
  where
    f :: Set Extension -> String -> m (Set Extension)
    f _ "Haskell98" = pure mempty
    f a ('N':'o':x) | Just x' <- readExtension x
                    = pure $ S.delete x' a
    f a x           | Just x' <- readExtension x
                    = pure $ S.insert x' a
    f _ x           = throwErrorWithBacktrace $ TL.pack $ "Unknown extension: " ++ x

-- | Parse an extension.
readExtension :: String -> Maybe Extension
readExtension x =
  case classifyExtension x of
    UnknownExtension _ -> Nothing
    x' -> Just x'

-- | Default extensions.
defaultExtensions :: Set Extension
defaultExtensions =
  S.fromList [e | e@EnableExtension{} <- knownExtensions] S.\\
  S.fromList (map EnableExtension badExtensions)

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ]
