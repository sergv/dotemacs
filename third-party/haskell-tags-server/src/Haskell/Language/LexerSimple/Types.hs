{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-# OPTIONS_GHC -O2 #-}

module Haskell.Language.LexerSimple.Types
  ( countInputSpace
  , AlexInput(..)
  , aiLineL
  , byteStringPos
  , Context(..)
  , LiterateLocation(..)
  , isLiterateEnabled
  , isLiterateBirdOrOutside
  , isLiterateLatexOrOutside
  , AlexState(..)
  , mkAlexState
  , alexEnterBirdLiterateEnv
  , alexEnterLiterateLatexEnv
  , alexExitLiterateEnv
  , pushContext
  , modifyCommentDepth
  , modifyQuasiquoterDepth
  , modifyPreprocessorDepth
  , retrieveToken
  , addIndentationSize
  , calculateQuasiQuoteEnds
  , AlexM
  , runAlexM
  , alexSetInput
  , alexSetNextCode
  , alexInputPrevChar
  , dropUntilNL
  , dropUntil
  , dropUntil2
  , alexGetByte
  , unsafeTextHeadAscii
  , unsafeTextHead
  , utf8BS

  , asCodeL
  , asCommentDepthL
  , asQuasiquoterDepthL
  , asIndentationSizeL
  , asPreprocessorDepthL
  , asLiterateLocL
  , asHaveQQEndL
  ) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Data.Char
import Data.Int
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void (Void, vacuous)
import Data.Word (Word8)

import Haskell.Language.Lexer.FastTags
import Haskell.Language.Lexer.Types (LiterateStyle(..), Context(..), AlexCode(..))
import Haskell.Language.LexerSimple.LensBlaze

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as BSI

import Foreign.ForeignPtr
import GHC.Base
import GHC.IO (IO(..))
import GHC.Ptr
import GHC.Word

{-# INLINE advanceLine #-}
advanceLine :: Char# -> Line -> Line
advanceLine '\n'# = increaseLine
advanceLine _     = id

countInputSpace :: AlexInput -> Int -> Int
countInputSpace AlexInput{aiInput} len =
  utf8FoldlBounded len inc 0 aiInput
  where
    inc acc ' '#  = acc + 1
    inc acc '\t'# = acc + 8
    inc acc c#    = case fixChar c# of 1## -> acc + 1; _ -> acc

data AlexInput = AlexInput
  { aiInput  :: {-# UNPACK #-} !(Ptr Word8)
  , aiLine   :: {-# UNPACK #-} !Line
  } deriving (Show, Eq, Ord)

{-# INLINE aiLineL #-}
aiLineL :: Lens' AlexInput Line
aiLineL = lens aiLine (\b s -> s { aiLine = b })

{-# INLINE byteStringPos #-}
byteStringPos :: C8.ByteString -> Int
byteStringPos (BSI.PS _payload offset _len) = offset

{-# INLINE withAlexInput #-}
withAlexInput :: C8.ByteString -> (AlexInput -> a) -> a
withAlexInput s f =
  case s' of
    BSI.PS ptr offset _len ->
      inlinePerformIO $ withForeignPtr ptr $ \ptr' -> do
        let !input = set aiLineL initLine AlexInput
              { aiInput  = ptr' `plusPtr` offset
              , aiLine   = Line 0
              }
            !res = f input
        touchForeignPtr ptr
        pure res
  where
    -- Line numbering starts from 0 because we're adding additional newline
    -- at the beginning to simplify processing. Thus, line numbers in the
    -- result are 1-based.
    initLine = Line 0

    -- Add '\0' at the end so that we'll find the end of stream (just
    -- as in the old C days...)
    s' = C8.cons '\n' $ C8.snoc (C8.snoc (stripBOM s) '\n') '\0'
    stripBOM :: C8.ByteString -> C8.ByteString
    stripBOM xs = fromMaybe xs $ C8.stripPrefix "\xEF\xBB\xBF" xs

data LiterateLocation a = LiterateInside a | LiterateOutside | Vanilla
  deriving (Eq, Ord, Show, Functor)

{-# INLINE litLocToInt #-}
litLocToInt :: LiterateLocation LiterateStyle -> Int
litLocToInt = \case
  Vanilla              -> 0
  LiterateOutside      -> 1
  LiterateInside Bird  -> 2
  LiterateInside Latex -> 3

{-# INLINE intToLitLoc #-}
intToLitLoc :: Int -> LiterateLocation LiterateStyle
intToLitLoc = \case
  0 -> Vanilla
  1 -> LiterateOutside
  2 -> LiterateInside Bird
  3 -> LiterateInside Latex
  x -> error $ "Invalid literate location representation: " ++ show x

{-# INLINE isLiterateEnabled #-}
isLiterateEnabled :: LiterateLocation a -> Bool
isLiterateEnabled = \case
  LiterateInside _ -> True
  LiterateOutside  -> True
  Vanilla          -> False

{-# INLINE isLiterateBirdOrOutside #-}
isLiterateBirdOrOutside :: LiterateLocation LiterateStyle -> Bool
isLiterateBirdOrOutside = \case
  LiterateInside Bird  -> True
  LiterateInside Latex -> False
  LiterateOutside      -> True
  Vanilla              -> False

{-# INLINE isLiterateLatexOrOutside #-}
isLiterateLatexOrOutside :: LiterateLocation LiterateStyle -> Bool
isLiterateLatexOrOutside = \case
  LiterateInside Bird  -> False
  LiterateInside Latex -> True
  LiterateOutside      -> True
  Vanilla              -> False

data AlexState = AlexState
  { asInput        :: {-# UNPACK #-} !AlexInput
  , asIntStore     :: {-# UNPACK #-} !Word64
  , asContextStack :: [Context]
  } deriving (Show, Eq, Ord)

{-# INLINE asIntStoreL #-}
asIntStoreL :: Lens' AlexState Word64
asIntStoreL = lens asIntStore (\b s -> s { asIntStore = b })

{-# INLINE maybeBoolToInt #-}
maybeBoolToInt :: Maybe Bool -> Int
maybeBoolToInt = \case
  Nothing    -> 0
  Just False -> 1
  Just True  -> 2

{-# INLINE intToMaybeBool #-}
intToMaybeBool :: Int -> Maybe Bool
intToMaybeBool = \case
  0 -> Nothing
  1 -> Just False
  2 -> Just True
  x -> error $ "Invalid integer representation of 'Maybe Bool': " ++ show x

{-# INLINE asCodeL              #-}
{-# INLINE asCommentDepthL      #-}
{-# INLINE asQuasiquoterDepthL  #-}
{-# INLINE asIndentationSizeL   #-}
{-# INLINE asPreprocessorDepthL #-}
{-# INLINE asLiterateLocL       #-}
{-# INLINE asHaveQQEndL         #-}
-- | Current Alex state the lexer is in. E.g. comments, string, TH quasiquoter
-- or vanilla toplevel mode.
asCodeL        :: Lens' AlexState AlexCode
asCommentDepthL, asQuasiquoterDepthL, asIndentationSizeL :: Lens' AlexState Int16
-- | How many directives deep are we.
asPreprocessorDepthL :: Lens' AlexState Int16
-- | Whether we're in bird-style or latex-style literate environment
asLiterateLocL :: Lens' AlexState (LiterateLocation LiterateStyle)
asHaveQQEndL   :: Lens' AlexState (Maybe Bool)
asCodeL              = asIntStoreL . int16L' 0  0x000f
asCommentDepthL      = asIntStoreL . int16L' 4  0x03ff
asQuasiquoterDepthL  = asIntStoreL . int16L' 14 0x03ff
asIndentationSizeL   = asIntStoreL . int16L  24
asPreprocessorDepthL = asIntStoreL . int16L  40
asLiterateLocL       = \f -> asIntStoreL (int16L' 56 0x0003 (fmap litLocToInt    . f . intToLitLoc))
asHaveQQEndL         = \f -> asIntStoreL (int16L' 58 0x0003 (fmap maybeBoolToInt . f . intToMaybeBool))

mkAlexState :: LiterateLocation Void -> AlexCode -> AlexInput -> AlexState
mkAlexState litLoc startCode input =
  set asCodeL startCode $
  set asLiterateLocL (vacuous litLoc) AlexState
    { asInput        = input
    , asIntStore     = 0
    , asContextStack = []
    }

{-# INLINE alexEnterBirdLiterateEnv #-}
alexEnterBirdLiterateEnv :: MonadState AlexState m => m ()
alexEnterBirdLiterateEnv =
  modify $ set asLiterateLocL (LiterateInside Bird)

{-# INLINE alexEnterLiterateLatexEnv #-}
alexEnterLiterateLatexEnv :: MonadState AlexState m => m ()
alexEnterLiterateLatexEnv =
  modify $ set asLiterateLocL (LiterateInside Latex)

{-# INLINE alexExitLiterateEnv #-}
alexExitLiterateEnv :: MonadState AlexState m => m ()
alexExitLiterateEnv =
  modify $ set asLiterateLocL LiterateOutside

{-# INLINE pushContext #-}
pushContext :: MonadState AlexState m => Context -> m ()
pushContext ctx = modify (\s -> s { asContextStack = ctx : asContextStack s })

{-# INLINE modifyCommentDepth #-}
modifyCommentDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyCommentDepth f = do
  depth <- gets (view asCommentDepthL)
  let depth' = f depth
  modify $ \s -> set asCommentDepthL depth' s
  pure depth'

{-# INLINE modifyQuasiquoterDepth #-}
modifyQuasiquoterDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyQuasiquoterDepth f = do
  depth <- gets (view asQuasiquoterDepthL)
  let depth' = f depth
  modify $ \s -> set asQuasiquoterDepthL depth' s
  pure depth'

{-# INLINE modifyPreprocessorDepth #-}
modifyPreprocessorDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyPreprocessorDepth f = do
  depth <- gets (view asPreprocessorDepthL)
  let depth' = f depth
  modify $ \s -> set asPreprocessorDepthL depth' s
  pure depth'

{-# INLINE retrieveToken #-}
retrieveToken :: AlexInput -> Int -> T.Text
retrieveToken AlexInput{aiInput} len =
  TE.decodeUtf8 $ utf8BS len aiInput

{-# INLINE addIndentationSize #-}
addIndentationSize :: MonadState AlexState m => Int16 -> m ()
addIndentationSize x =
  modify (over asIndentationSizeL (+ x))

data QQEndsState = QQEndsState
  { qqessPresent  :: !Bool
  , qqessPrevChar :: !Char#
  }

calculateQuasiQuoteEnds :: Ptr Word8 -> Bool
calculateQuasiQuoteEnds =
  qqessPresent . utf8Foldl' combine (QQEndsState False '\n'#)
  where
    combine :: QQEndsState -> Char# -> QQEndsState
    combine QQEndsState{qqessPresent, qqessPrevChar} c# = QQEndsState
      { qqessPresent      =
        qqessPresent ||
        case (# qqessPrevChar, c# #) of
          (# '|'#, ']'# #) -> True
          (# _,    '⟧'# #) -> True
          _                -> False
      , qqessPrevChar = c#
      }

type AlexM = WriterT [Pos ServerToken] (State AlexState)

{-# INLINE runAlexM #-}
runAlexM
  :: LiterateLocation Void
  -> AlexCode
  -> C8.ByteString
  -> AlexM ()
  -> [Pos ServerToken]
runAlexM litLoc startCode input action =
  withAlexInput input $ \input' ->
    evalState (execWriterT action) $ mkAlexState litLoc startCode input'

{-# INLINE alexSetInput #-}
alexSetInput :: MonadState AlexState m => AlexInput -> m ()
alexSetInput input = modify $ \s -> s { asInput = input }

{-# INLINE alexSetNextCode #-}
alexSetNextCode :: MonadState AlexState m => AlexCode -> m ()
alexSetNextCode code = modify $ set asCodeL code

-- Alex interface
{-# INLINE alexInputPrevChar #-}
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = const '\0'

{-# INLINE dropUntilNL #-}
dropUntilNL :: AlexInput -> AlexInput
dropUntilNL input@AlexInput{aiInput} =
  input { aiInput = dropUntilNL# aiInput }

{-# INLINE dropUntil #-}
dropUntil :: Word8 -> AlexInput -> AlexInput
dropUntil w input@AlexInput{aiInput} =
  input { aiInput = dropUntil# w aiInput }

{-# INLINE dropUntil2 #-}
dropUntil2 :: Word8 -> Word8 -> AlexInput -> AlexInput
dropUntil2 w1 w2 input@AlexInput{aiInput} =
  input { aiInput = dropUntil2# w1 w2 aiInput }

{-# INLINE alexGetByte #-}
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{aiInput} =
  case nextChar aiInput of
    (C# '\0'#, _)  -> Nothing
    (C# c#,    cs) -> Just (b, input')
      where
        !b     = W8# (fixChar c#)
        input' = over aiLineL (advanceLine c#) $ input { aiInput = cs }

-- Translate unicode character into special symbol we teached Alex to recognize.
{-# INLINE fixChar #-}
fixChar :: Char# -> Word#
fixChar = \case
  -- These should not be translated since Alex knows about them
  '→'#    -> reservedSym
  '∷'#    -> reservedSym
  '⇒'#    -> reservedSym
  '∀'#    -> reservedSym
  '⦇'#    -> reservedSym
  '⦈'#    -> reservedSym
  '⟦'#    -> reservedSym
  '⟧'#    -> reservedSym
  '\x01'# -> other
  '\x02'# -> other
  '\x03'# -> other
  '\x04'# -> other
  '\x05'# -> other
  '\x06'# -> other
  '\x07'# -> other
  c# -> case ord# c# of
    c2# | isTrue# (c2# <=# 0x7f#) ->
          int2Word# c2# -- Plain ascii needs no fixing.
        | otherwise   ->
          case generalCategory (C# c#) of
            UppercaseLetter      -> upper
            LowercaseLetter      -> lower
            TitlecaseLetter      -> upper
            ModifierLetter       -> suffix
            NonSpacingMark       -> suffix
            OtherLetter          -> lower
            DecimalNumber        -> digit
            OtherNumber          -> digit
            Space                -> space
            ConnectorPunctuation -> symbol
            DashPunctuation      -> symbol
            OtherPunctuation     -> symbol
            MathSymbol           -> symbol
            CurrencySymbol       -> symbol
            ModifierSymbol       -> symbol
            OtherSymbol          -> symbol
            _                    -> other
  where
    other, space, upper, lower, symbol, digit, suffix, reservedSym :: Word#
    other       = 0x00## -- Don't care about these
    space       = 0x01##
    upper       = 0x02##
    lower       = 0x03##
    symbol      = 0x04##
    digit       = 0x05##
    suffix      = 0x06##
    reservedSym = 0x07##

{-# INLINE unsafeTextHeadAscii #-}
unsafeTextHeadAscii :: Ptr Word8 -> Word8
unsafeTextHeadAscii (Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 0#)

{-# INLINE unsafeTextHead #-}
unsafeTextHead :: Ptr Word8 -> Char
unsafeTextHead = fst . nextChar

{-# INLINE nextChar #-}
nextChar :: Ptr Word8 -> (Char, Ptr Word8)
nextChar (Ptr ptr#) =
  case utf8DecodeChar# ptr# of
    (# c#, nBytes# #) -> (C# c#, Ptr (ptr# `plusAddr#` nBytes#))

{-# INLINE dropUntilNL# #-}
dropUntilNL# :: Ptr Word8 -> Ptr Word8
dropUntilNL# (Ptr start#) = Ptr (go start#)
  where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
      0##  -> ptr#
      10## -> ptr# -- '\n'
      _    -> go (ptr# `plusAddr#` 1#)

{-# INLINE dropUntil# #-}
dropUntil# :: Word8 -> Ptr Word8 -> Ptr Word8
dropUntil# (W8# w#) (Ptr start#) = Ptr (go start#)
  where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
      0##  -> ptr#
      10## -> ptr# -- '\n'
      c# | isTrue# (c# `eqWord#` w#) -> ptr#
         | otherwise                 -> go (ptr# `plusAddr#` 1#)

{-# INLINE dropUntil2# #-}
dropUntil2# :: Word8 -> Word8 -> Ptr Word8 -> Ptr Word8
dropUntil2# (W8# w1#) (W8# w2#) (Ptr start#) = Ptr (go start#)
  where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
      0##  -> ptr#
      10## -> ptr# -- '\n'
      c# | isTrue# ((c# `eqWord#` w1#) `orI#` (c# `eqWord#` w2#)) -> ptr#
         | otherwise                                              -> go (ptr# `plusAddr#` 1#)

{-# INLINE utf8Foldl' #-}
utf8Foldl' :: forall a. (a -> Char# -> a) -> a -> Ptr Word8 -> a
utf8Foldl' f x0 (Ptr ptr#) =
  go x0 ptr#
  where
    go :: a -> Addr# -> a
    go !acc addr# =
      case utf8DecodeChar# addr# of
        (# _,  0#      #) -> acc
        (# c#, nBytes# #) -> go (acc `f` c#) (addr# `plusAddr#` nBytes#)

{-# INLINE utf8FoldlBounded #-}
utf8FoldlBounded :: forall a. Int -> (a -> Char# -> a) -> a -> Ptr Word8 -> a
utf8FoldlBounded (I# len#) f x0 (Ptr ptr#) =
  go len# x0 ptr#
  where
    go :: Int#-> a -> Addr# -> a
    go 0# !acc _ = acc
    go n# !acc addr# =
      case utf8DecodeChar# addr# of
        (# _,  0#      #) -> acc
        (# c#, nBytes# #) -> go (n# -# 1#) (acc `f` c#) (addr# `plusAddr#` nBytes#)

{-# INLINE utf8BS #-}
utf8BS :: Int -> Ptr Word8 -> BS.ByteString
utf8BS (I# n#) (Ptr start#) =
  BSI.PS (inlinePerformIO (newForeignPtr_ (Ptr start#))) 0 (I# (go n# start# 0#))
  where
    go :: Int# -> Addr# -> Int# -> Int#
    go 0# _    m# = m#
    go k# ptr# m# =
      case utf8SizeChar# ptr# of
        0#      -> m#
        nBytes# -> go (k# -# 1#) (ptr# `plusAddr#` nBytes#) (m# +# nBytes#)

{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

{-# INLINE utf8DecodeChar# #-}
utf8DecodeChar# :: Addr# -> (# Char#, Int# #)
utf8DecodeChar# a# =
  case indexWord8OffAddr# a# 0# of
    0## -> (# '\0'#, 0# #)
    !x# ->
      let !ch0 = word2Int# x# in
      case () of
        () | isTrue# (ch0 <=# 0x7F#) -> (# chr# ch0, 1# #)

          | isTrue# ((ch0 >=# 0xC0#) `andI#` (ch0 <=# 0xDF#)) ->
            let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then err 1# else
            (# chr# (((ch0 -# 0xC0#) `uncheckedIShiftL#` 6#) +#
                      (ch1 -# 0x80#)),
               2# #)

          | isTrue# ((ch0 >=# 0xE0#) `andI#` (ch0 <=# 0xEF#)) ->
            let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then err 1# else
            let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
            if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then err 2# else
            (# chr# (((ch0 -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                     ((ch1 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                      (ch2 -# 0x80#)),
               3# #)

         | isTrue# ((ch0 >=# 0xF0#) `andI#` (ch0 <=# 0xF8#)) ->
            let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then err 1# else
            let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
            if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then err 2# else
            let !ch3 = word2Int# (indexWord8OffAddr# a# 3#) in
            if isTrue# ((ch3 <# 0x80#) `orI#` (ch3 >=# 0xC0#)) then err 3# else
            (# chr# (((ch0 -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                     ((ch1 -# 0x80#) `uncheckedIShiftL#` 12#) +#
                     ((ch2 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                      (ch3 -# 0x80#)),
               4# #)

          | otherwise -> err 1#
      where
        -- all invalid sequences end up here:
        err :: Int# -> (# Char#, Int# #)
        err nBytes# = (# '\8'#, nBytes# #)
        -- TODO: check whether following note from ghc applies to server's lexer:
        -- '\xFFFD' would be the usual replacement character, but
        -- that's a valid symbol in Haskell, so will result in a
        -- confusing parse error later on.  Instead we use '\0' which
        -- will signal a lexer error immediately.

{-# INLINE utf8SizeChar# #-}
utf8SizeChar# :: Addr# -> Int#
utf8SizeChar# a# =
  case indexWord8OffAddr# a# 0# of
    0## -> 0#
    !x# ->
      let !ch0 = word2Int# x# in
      case () of
        _ | isTrue# (ch0 <=# 0x7F#) -> 1#

          | isTrue# ((ch0 >=# 0xC0#) `andI#` (ch0 <=# 0xDF#)) ->
            let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then 1# else
            2#

          | isTrue# ((ch0 >=# 0xE0#) `andI#` (ch0 <=# 0xEF#)) ->
            let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then 1# else
            let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
            if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then 2# else
            3#

         | isTrue# ((ch0 >=# 0xF0#) `andI#` (ch0 <=# 0xF8#)) ->
            let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then 1# else
            let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
            if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then 2# else
            let !ch3 = word2Int# (indexWord8OffAddr# a# 3#) in
            if isTrue# ((ch3 <# 0x80#) `orI#` (ch3 >=# 0xC0#)) then 3# else
            4#

          | otherwise -> 1#
