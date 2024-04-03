-- |
-- Module:     ${haskell path to module name}
-- Copyright:  (c) ${author} ${date year}
-- License:    ${license spdx} (see LICENSE)
-- Maintainer: ${email}

module ${haskell path to module name} (main) where

import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer (printAwkExpr)

fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

main :: IO ()
main = do
  let baselineName = "fibo 7"
  defaultMain
    [ bgroup "fibonacci numbers"
      [ bcompare "tenth"  $ bench "fifth"     $ nf fibo  5
      ,                     bench "tenth"     $ nf fibo 10
      , bcompare "tenth"  $ bench "twentieth" $ nf fibo 20
      ]
    , mapLeafBenchmarks (addCompare baselineName) $ bgroup "fibonacci #2"
      [ bench baselineName $ nf fibo 7
      , bench "fibo 8" $ nf fibo 8
      , bench "fibo 9" $ nf fibo 9
      ]
    ]

addCompare :: String -> [String] -> Benchmark -> Benchmark
addCompare targetBenchName (name : path)
  | name /= targetBenchName
  = bcompare (printAwkExpr (locateBenchmark (targetBenchName : path)))
addCompare _ _ = id
