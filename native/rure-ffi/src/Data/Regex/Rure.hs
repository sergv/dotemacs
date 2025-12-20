-- |
-- Module:     Data.Regex.Rure
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Regex.Rure
  ( Match(..)
  , ReversedList(..)

  , Regex
  , compileRegex
  , finaliseRegex
  , bytestringHasMatch
  , bytestringAllMatches
  , utf8PtrAllMatchesIO

  , RegexSet
  , compileRegexSet
  , finaliseRegexSet
  , bytestringHasSetMatch
  , utf8PtrHasSetMatchIO

  -- * Flags
  , FFI.Flags
  , FFI.flagCaseInsensitive
  , FFI.flagMultiline
  , FFI.flagDotNewline
  , FFI.flagSwapGreed
  , FFI.flagIgnoreWhitespace
  , FFI.flagUnicode
  , FFI.flagDefault
  ) where

import Control.Exception
import Data.ByteString qualified as BS
import Data.Coerce
import Data.Foldable
import Data.Regex.Rure.FFI (CUInt8)
import Data.Regex.Rure.FFI qualified as FFI
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen, withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr
import Foreign.Storable (peek)
import GHC.Generics (Generic)
import System.IO.Unsafe

data Match = Match
  { matchStart :: !Int
  , matchEnd   :: !Int
  } deriving (Eq, Ord, Show, Generic)

data Options = Options
  {
    -- | Compiled regular expression size limit.
    optSizeLimit    :: !(Maybe Word)

  , -- | Approximate size of the cache used by the DFA during search.
    optDfaSizeLimit :: !(Maybe Word)
  }

newtype Regex = Regex (ForeignPtr FFI.Regex)
  deriving (Show)

newtype Iter = Iter (ForeignPtr FFI.Iter)
  deriving (Show)
newtype RegexSet = RegexSet (ForeignPtr FFI.RegexSet)
  deriving (Show)

finaliseRegex :: Regex -> IO ()
finaliseRegex (Regex r) = finalizeForeignPtr r

finaliseRegexSet :: RegexSet -> IO ()
finaliseRegexSet (RegexSet r) = finalizeForeignPtr r

withOptions :: Maybe Options -> (Ptr FFI.Options -> IO a) -> IO a
withOptions opts k =
  case opts of
    Nothing -> k nullPtr
    Just Options{optSizeLimit, optDfaSizeLimit} ->
      bracket
        FFI.rureOptionsNew
        FFI.rureOptionsFree
        $ \optsPtr -> do
          traverse_ (FFI.rureOptionsSetSizeLimit optsPtr . fromIntegral) optSizeLimit
          traverse_ (FFI.rureOptionsSetDfaSizeLimit optsPtr . fromIntegral) optDfaSizeLimit
          k optsPtr

compileRegex
  :: BS.ByteString
  -> FFI.Flags
  -> Maybe Options
  -> Either BS.ByteString Regex
compileRegex !pattern !flags opts = unsafePerformIO $
  BS.useAsCStringLen pattern $ \(patternPtr, patternLen) ->
    bracket
      FFI.rureErrorNew
      FFI.rureErrorFree
      $ \errPtr -> mask_ $ do
        rePtr <- withOptions opts $ \optsPtr ->
          FFI.rureCompile (coerce patternPtr) (fromIntegral patternLen) flags optsPtr errPtr
        msgPtr <- FFI.rureErrorMessage errPtr
        if rePtr == nullPtr
        then Left <$> BS.packCString msgPtr
        else Right . Regex <$> newForeignPtr FFI.ptrRureFree rePtr

compileRegexSet
  :: [BS.ByteString]
  -> FFI.Flags
  -> Maybe Options
  -> Either BS.ByteString RegexSet
compileRegexSet patterns !flags opts = unsafePerformIO $
  withMany BS.useAsCStringLen patterns $ \cstrLens -> do
    let patternPtrs = fmap fst cstrLens
        lens        = fmap snd cstrLens
    bracket
      FFI.rureErrorNew
      FFI.rureErrorFree
      $ \errPtr -> do
        withArrayLen patternPtrs $ \n patternPtrsPtr -> do
          withArray (map fromIntegral lens) $ \lensPtr -> mask_ $ do
            rePtr <- withOptions opts $ \optsPtr ->
              FFI.rureCompileSet
                (coerce patternPtrsPtr)
                lensPtr
                (fromIntegral n)
                flags
                optsPtr
                errPtr
            msgPtr <- FFI.rureErrorMessage errPtr
            if rePtr == nullPtr
            then Left <$> BS.packCString msgPtr
            else Right . RegexSet <$> newForeignPtr FFI.ptrRureSetFree rePtr

{-# INLINE isTruthy #-}
-- | Check whether a 'CBool' denotes true.
isTruthy :: CBool -> Bool
isTruthy = (/= CBool 0)

bytestringHasMatch :: Regex -> BS.ByteString -> Bool
bytestringHasMatch !(Regex re) !haystack = unsafePerformIO $
  BS.useAsCStringLen haystack $ \(haystackPtr, haystackLen) ->
    withForeignPtr re $ \rePtr ->
      isTruthy <$> FFI.rureIsMatch rePtr (coerce haystackPtr) (fromIntegral haystackLen) 0

newtype ReversedList a = ReversedList { unReversedList :: [a] }

bytestringAllMatches :: Regex -> BS.ByteString -> ReversedList Match
bytestringAllMatches re haystack = unsafePerformIO $
  BS.useAsCStringLen haystack $ \(haystackPtr, haystackLen) ->
    utf8PtrAllMatchesIO re (coerce haystackPtr) (fromIntegral haystackLen)

utf8PtrAllMatchesIO :: Regex -> Ptr CUInt8 -> CSize -> IO (ReversedList Match)
utf8PtrAllMatchesIO !(Regex re) !haystackPtr !haystackLen =
  withForeignPtr re $ \rePtr ->
    bracket
      (FFI.rureIterNew rePtr)
      FFI.rureIterFree
      $ \iterPtr ->
        alloca $ \matchPtr -> do
          let go :: [Match] -> IO (ReversedList Match)
              go acc = do
                res <- FFI.rureIterNext iterPtr haystackPtr haystackLen matchPtr
                if isTruthy res
                then do
                  !FFI.Match{FFI.matchStart, FFI.matchEnd} <- peek matchPtr
                  let !m = Match { matchStart = fromIntegral matchStart, matchEnd = fromIntegral matchEnd }
                  go (m : acc)
                else
                  pure (ReversedList acc)
          go []

bytestringHasSetMatch :: RegexSet -> BS.ByteString -> Bool
bytestringHasSetMatch !reSet !haystack = unsafePerformIO $
  BS.useAsCStringLen haystack $ \(haystackPtr, haystackLen) ->
    utf8PtrHasSetMatchIO reSet (coerce haystackPtr) (fromIntegral haystackLen)

utf8PtrHasSetMatchIO :: RegexSet -> Ptr CUInt8 -> CSize -> IO Bool
utf8PtrHasSetMatchIO !(RegexSet reSet) !haystackPtr !haystackLen =
  withForeignPtr reSet $ \reSetPtr ->
    isTruthy <$> FFI.rureSetIsMatch reSetPtr haystackPtr haystackLen 0

