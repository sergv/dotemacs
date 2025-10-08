-- |
-- Module:     Data.Regex.FFI.Rure
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Regex.Rure.FFI
  ( Match(..)

  , Flags(..)
  , flagCaseInsensitive
  , flagMultiline
  , flagDotNewline
  , flagSwapGreed
  , flagIgnoreWhitespace
  , flagUnicode
  , flagDefault

  , rureCompile
  , ptrRureFree

  , rureErrorNew
  , rureErrorFree
  , rureErrorMessage

  , rureIsMatch
  , rureFind

  , rureIterNew
  , rureIterFree
  , rureIterNext

  , rureOptionsNew
  , rureOptionsFree
  , rureOptionsSetSizeLimit
  , rureOptionsSetDfaSizeLimit

  , rureCompileSet
  , ptrRureSetFree
  , rureSetIsMatch

  , Error
  , Iter
  , Options
  , Regex
  , RegexSet

  , CUInt8
  , CUInt32
  ) where

import Data.Bits
import Data.Word
import Foreign
import Foreign.C.Types

#include <rure.h>

data Error
data Iter
data Options
data Regex
data RegexSet


data Match = Match
  { matchStart :: !CSize
  , matchEnd   :: !CSize
  }


newtype Flags = Flags CUInt32

instance Semigroup Flags where
  Flags x <> Flags y = Flags (x .|. y)

instance Monoid Flags where
  -- | NB this is not the same as 'flagDefault'.
  mempty = Flags 0


foreign import ccall unsafe "rure.h rure_compile" rureCompile
  :: Ptr CUInt8 -- pattern
  -> CSize      -- length
  -> Flags      -- flags
  -> Ptr Options
  -> Ptr Error
  -> IO (Ptr Regex)

foreign import ccall unsafe "rure.h &rure_free" ptrRureFree
  :: FunPtr (Ptr Regex -> IO ())


foreign import ccall unsafe "rure.h rure_error_new" rureErrorNew
  :: IO (Ptr Error)

foreign import ccall unsafe "rure.h rure_error_free" rureErrorFree
  :: Ptr Error
  -> IO ()

foreign import ccall unsafe "rure.h rure_error_message" rureErrorMessage
  :: Ptr Error
  -> IO (Ptr CChar)


foreign import ccall unsafe "rure.h rure_is_match" rureIsMatch
  :: Ptr Regex
  -> Ptr CUInt8 -- haystack
  -> CSize      -- length of haystack in bytes
  -> CSize      -- start offset
  -> IO CBool

foreign import ccall unsafe "rure.h rure_find" rureFind
  :: Ptr Regex
  -> Ptr CUInt8 -- haystack
  -> CSize      -- length of haystack in bytes
  -> CSize      -- start offset
  -> Ptr Match
  -> IO ()


foreign import ccall unsafe "rure.h rure_iter_new" rureIterNew
  :: Ptr Regex
  -> IO (Ptr Iter)

foreign import ccall unsafe "rure.h rure_iter_free" rureIterFree
  :: Ptr Iter
  -> IO ()

foreign import ccall unsafe "rure.h rure_iter_next" rureIterNext
  :: Ptr Iter
  -> Ptr CUInt8 -- haystack
  -> CSize      -- length of haystack in bytes
  -> Ptr Match
  -> IO CBool


foreign import ccall unsafe "rure.h rure_options_new" rureOptionsNew
  :: IO (Ptr Options)

foreign import ccall unsafe "rure.h rure_options_free" rureOptionsFree
  :: Ptr Options
  -> IO ()

foreign import ccall unsafe "rure.h rure_options_size_limit" rureOptionsSetSizeLimit
  :: Ptr Options
  -> CSize -- Compiled regular expression size limit.
  -> IO ()

foreign import ccall unsafe "rure.h rure_options_dfa_size_limit" rureOptionsSetDfaSizeLimit
  :: Ptr Options
  -> CSize -- Approximate size of the cache used by the DFA during search.
  -> IO ()


foreign import ccall unsafe "rure.h rure_compile_set" rureCompileSet
  :: Ptr (Ptr CUInt8) -- patterns
  -> Ptr CSize        -- pattern lengths
  -> CSize            -- number of patterns
  -> Flags
  -> Ptr Options
  -> Ptr Error
  -> IO (Ptr RegexSet)

foreign import ccall unsafe "rure.h &rure_set_free" ptrRureSetFree
  :: FunPtr (Ptr RegexSet -> IO ())

foreign import ccall unsafe "rure.h rure_set_is_match" rureSetIsMatch
  :: Ptr RegexSet
  -> Ptr CUInt8 -- haystack
  -> CSize      -- length of haystack in bytes
  -> CSize      -- start offset
  -> IO CBool

type CUInt8  = #{type uint8_t}
type CUInt32 = #{type uint32_t}


instance Storable Match where
  sizeOf    _ = (#size rure_match)
  alignment _ = (#alignment rure_match)
  peek ptr =
    Match <$> (#peek rure_match, start) ptr <*> (#peek rure_match, end) ptr
  poke ptr Match{matchStart, matchEnd} =
    (#poke rure_match, start) ptr matchStart *> (#poke rure_match, end) ptr matchEnd

-- | The case insensitive (i) flag.
flagCaseInsensitive :: Flags
flagCaseInsensitive = Flags (#const RURE_FLAG_CASEI)

-- | The multi-line matching (m) flag. (^ and $ match new line boundaries.)
flagMultiline :: Flags
flagMultiline = Flags (#const RURE_FLAG_MULTI)

-- The any character (s) flag. (. matches new line.)
flagDotNewline :: Flags
flagDotNewline = Flags (#const RURE_FLAG_DOTNL)

-- | The greedy swap (U) flag. (e.g., + is ungreedy and +? is greedy.)
flagSwapGreed :: Flags
flagSwapGreed = Flags (#const RURE_FLAG_SWAP_GREED)

-- | The ignore whitespace (x) flag.
flagIgnoreWhitespace :: Flags
flagIgnoreWhitespace = Flags (#const RURE_FLAG_SPACE)

-- | The Unicode (u) flag.
flagUnicode :: Flags
flagUnicode = Flags (#const RURE_FLAG_UNICODE)

-- | The default set of flags enabled when no flags are set.
flagDefault :: Flags
flagDefault = Flags (#const RURE_DEFAULT_FLAGS)
