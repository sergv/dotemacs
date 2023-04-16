-- |
-- Module:     ${haskell path to module name}
-- Copyright:  (c) ${author} ${date year}
-- License:    ${license spdx} (see LICENSE)
-- Maintainer: ${email}

module ${haskell path to module name} (main) where

import Test.Tasty.Bench

fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

main :: IO ()
main = defaultMain
  [ bgroup "fibonacci numbers"
    [ bcompare "tenth"  $ bench "fifth"     $ nf fibo  5
    ,                     bench "tenth"     $ nf fibo 10
    , bcompare "tenth"  $ bench "twentieth" $ nf fibo 20
    ]
  ]
