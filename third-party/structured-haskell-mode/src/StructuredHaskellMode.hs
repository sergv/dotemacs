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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module StructuredHaskellMode
  ( ParseType(..)
  , Extension
  , SourceSpan(..)
  , parseSpans
  , check
  , getExtensions
  ) where

import Data.Data
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.Exts

-- | A generic Dynamic-like constructor -- but more convenient to
-- write and pattern match on.
data D = forall a. Data a => D a

-- | A parser. Presently there is only 'parseTopLevel', but in the
-- past, and in the future, there will be the facility to parse
-- specific parse of declarations, rather than re-parsing the whole
-- declaration which can be both slow and brittle.
type Parser a = ParseMode -> String -> ParseResult a

(<||>) :: ParseResult a -> ParseResult a -> ParseResult a
(<||>) x y = case x of
  ParseFailed{} -> y
  _             -> x

-- | Thing to parse.
data ParseType = Decl | Stmt

parseSpans :: ParseType -> [Extension] -> String -> Either String [SourceSpan]
parseSpans typ exts code =
  (\(mode, D x) -> genHSE mode x) <$> runParser (chooseParser typ) exts code

check :: ParseType -> [Extension] -> String -> Either String ()
check typ exts code = () <$ runParser (chooseParser typ) exts code

chooseParser :: ParseType -> Parser D
chooseParser = \case
  Decl -> parseTopLevel
  Stmt -> parseSomeStmt

-- | Output AST info for the given Haskell code.
runParser
  :: Parser a
  -> [Extension]
  -> String
  -> Either String (ParseMode, a)
runParser parser exts code =
  case parser mode code of
    ParseFailed loc e -> Left $ show loc ++ ":" ++ e
    ParseOk x         -> Right (mode, x)
  where
    mode :: ParseMode
    mode = parseMode {extensions = exts}

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
parseTopLevel :: ParseMode -> String -> ParseResult D
parseTopLevel mode code =
  ((D . fix) <$> parseDeclWithMode mode code)   <||>
  (D <$> parseImport mode code)                 <||>
  ((D . fix) <$> parseModuleWithMode mode code) <||>
  (D <$> parseModulePragma mode code)

-- | Parse a do-notation statement.
parseSomeStmt :: ParseMode -> String -> ParseResult D
parseSomeStmt mode code =
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
  { extensions = defaultExtensions
  -- disable fixities so that unfamiliar operators do not confuse the parser
  , fixities   = Nothing
  }

-- | Generate a list of spans from the HSE AST.
genHSE :: Data a => ParseMode -> a -> [SourceSpan]
genHSE mode = go
  where
    go :: Data b => b -> [SourceSpan]
    go x =
      case gmapQ D x of
        zs@(D y:ys) ->
          case cast y of
            Just s -> concat
              [ [ spanHSE
                    (show (show (typeOf x)))
                    (showConstr (toConstr x))
                    (srcInfoSpan s)
                ]
              , concatMap (\(i, D d) -> pre x i ++ go d) (zip [0..] ys)
              , post mode x
              ]
            _      -> concatMap (\(D d) -> go d) zs
        _ -> []

-- | Pre-children tweaks for a given parent at index i.
--
pre :: (Typeable a) => a -> Integer -> [SourceSpan]
pre x i = case cast x of
  -- <foo { <foo = 1> }> becomes <foo <{ <foo = 1> }>>
  Just (RecUpdate SrcSpanInfo{ srcInfoPoints = start:_, srcInfoSpan = end } _ _)
    | i == 1 ->
      [ spanHSE
          (show "RecUpdates")
          "RecUpdates"
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
            (show "InstHeads")
            "InstHeads"
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
post :: (Typeable a) => ParseMode -> a ->  [SourceSpan]
post mode x =
  case cast x of
    Just (QuasiQuote (base :: SrcSpanInfo) qname content) ->
      case parseExpWithMode mode content of
        ParseOk ex    -> genHSE mode (fmap (redelta qname base) ex)
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

data SourceSpan = SourceSpan
  { ssType        :: !Text -- unqualified
  , ssConstructor :: !Text
  , ssStartLine   :: !Int
  , ssStartColumn :: !Int
  , ssEndLine     :: !Int
  , ssEndColumn   :: !Int
  }

-- | Generate a span from a HSE SrcSpan.
spanHSE :: String -> String -> SrcSpan -> SourceSpan
spanHSE typ cons SrcSpan{..} = SourceSpan
  { ssType        = T.takeWhileEnd (/= '.') $ T.pack typ
  , ssConstructor = T.pack cons
  , ssStartLine   = srcSpanStartLine
  , ssStartColumn = srcSpanStartColumn
  , ssEndLine     = srcSpanEndLine
  , ssEndColumn   = srcSpanEndColumn
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
getExtensions :: [Text] -> [Extension]
getExtensions = foldl f defaultExtensions . map T.unpack
  where f _ "Haskell98" = []
        f a ('N':'o':x)
          | Just x' <- readExtension x =
            delete x' a
        f a x
          | Just x' <- readExtension x =
            x' :
            delete x' a
        f _ x = error $ "Unknown extension: " ++ x

-- | Parse an extension.
readExtension :: String -> Maybe Extension
readExtension x =
  case classifyExtension x of
    UnknownExtension _ -> Nothing
    x' -> Just x'

-- | Default extensions.
defaultExtensions :: [Extension]
defaultExtensions =
  [e | e@EnableExtension{} <- knownExtensions] \\
  map EnableExtension badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ]
