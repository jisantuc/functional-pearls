module Main where

import Ch1MinFree (minFree)
import Control.Monad (replicateM)
import Criterion.Main (bench, bgroup, defaultMain, env, whnf)
import System.Random (randomRIO)

randomIntList :: Int -> Int -> IO [Int]
randomIntList rangeMax n = replicateM n (randomRIO (0, rangeMax))

getEnv :: IO ([Int], [Int], [Int], [Int])
getEnv =
  (,,,)
    <$> randomIntList 1000 100
    <*> randomIntList 10000 1000
    <*> randomIntList 100000 10000
    <*> randomIntList 1000000 100000

main :: IO ()
main =
  defaultMain
    [ bgroup
        "minFree"
        [ env
            getEnv
            $ \ ~(smallList, bigList, biggerList, hugeList) ->
              bgroup
                "naive"
                [ bench "small" $ whnf minFree smallList,
                  bench "big" $ whnf minFree bigList,
                  bench "bigger" $ whnf minFree biggerList,
                  bench "huge" $ whnf minFree hugeList
                ]
        ]
    ]
