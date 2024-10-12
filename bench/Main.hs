module Main where

import Ch1MinFree (minFree', naiveMinFree)
import Control.Monad (replicateM)
import Criterion.Main
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    env,
    whnf,
  )
import System.Random (randomRIO)

randomIntList :: Int -> Int -> IO [Int]
randomIntList rangeMax n = replicateM n (randomRIO (0, rangeMax))

getSparseEnv :: IO ([Int], [Int], [Int], [Int], [Int])
getSparseEnv =
  (,,,,)
    <$> randomIntList 1000 100
    <*> randomIntList 10000 1000
    <*> randomIntList 100000 10000
    <*> randomIntList 1000000 100000
    <*> randomIntList 10000000 1000000

getDenseEnv :: IO ([Int], [Int], [Int], [Int], [Int])
getDenseEnv =
  (,,,,)
    <$> randomIntList 100 100
    <*> randomIntList 1000 1000
    <*> randomIntList 10000 10000
    <*> randomIntList 100000 100000
    <*> randomIntList 1000000 1000000

main :: IO ()
main =
  defaultMain
    [ bgroup
        "minFree"
        [ minFreeBench "sparse" getSparseEnv,
          minFreeBench "dense" getDenseEnv
        ]
    ]

minFreeBench :: String -> IO ([Int], [Int], [Int], [Int], [Int]) -> Benchmark
minFreeBench label envIO =
  env envIO $ \ ~(smallList, bigList, biggerList, hugeList, hugerList) ->
    bgroup
      label
      [ bgroup
          "checklist"
          [ bench "small" $ whnf minFree' smallList,
            bench "big" $ whnf minFree' bigList,
            bench "bigger" $ whnf minFree' biggerList,
            bench "huge" $ whnf minFree' hugeList,
            bench "huger" $ whnf minFree' hugerList
          ],
        bgroup
          "naive"
          [ bench "small" $ whnf naiveMinFree smallList,
            bench "big" $ whnf naiveMinFree bigList,
            bench "bigger" $ whnf naiveMinFree biggerList,
            bench "huge" $ whnf naiveMinFree hugeList,
            bench "huger" $ whnf naiveMinFree hugerList
          ]
      ]
