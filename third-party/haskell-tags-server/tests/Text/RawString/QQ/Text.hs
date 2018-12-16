----------------------------------------------------------------------------
-- |
-- Module      :  Text.RawString.QQ.Text
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Friday, 23 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.RawString.QQ.Text (text, textLazy) where

import qualified Data.List as L
import Data.Text as T
import Data.Text.Lazy as TL
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

text :: QuasiQuoter
text = mkQQ "text" $ \str -> [e| T.pack $(liftString $ stripPrefixes str) |]

textLazy :: QuasiQuoter
textLazy = mkQQ "textLazy" $ \str -> [e| TL.pack $(liftString $ stripPrefixes str) |]

mkQQ :: String -> (String -> Q Exp) -> QuasiQuoter
mkQQ qqName mkExp =  QuasiQuoter
  { quoteExp  = mkExp
  , quotePat  = \_ -> fail $ "Cannot use " ++ qqName ++ " in patterns"
  , quoteType = \_ -> fail $ "Cannot use " ++ qqName ++ " in type"
  , quoteDec  = \_ -> fail $ "Cannot use " ++ qqName ++ " in declarations"
  }

stripPrefixes :: String -> String
stripPrefixes = L.unlines . L.map (L.drop 1 . L.dropWhile (/= '|')) . L.lines
