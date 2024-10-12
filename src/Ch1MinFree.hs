{-# LANGUAGE TupleSections #-}

module Ch1MinFree where

import Data.Array (Array, accumArray, elems)

-- Round 1: Naive
-- diff the list of actual numbers with the list of
-- numbers from 0 to infinity (thanks laziness!)

-- | Find elements in one list not present in another list.
(\\) :: (Eq a) => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

-- | Find the first natural number not present in xs
naiveMinFree :: [Int] -> Int
naiveMinFree xs = head ([0 ..] \\ xs)

-- Round 2: checklist
-- for a given list xs, the minimum free number must be less than
-- length xs, so we can check each number 0 .. length xs then stop
-- when we find one that's missing

-- | Find the index of the first non-True entry
search :: Array Int Bool -> Int
search = length . takeWhile id . elems
-- ^ Array i a is an array of values with index type i and values a, I think

checklist :: [Int] -> Array Int Bool
checklist xs =
  accumArray
    (||) -- rule for combining values
    False -- initial value at each index
    (0, listLength) -- range of indices
    ((,True) <$> filter (<= listLength) xs) -- (index, value) pairs
  where
    listLength = length xs

minFree' :: [Int] -> Int
minFree' xs = search (checklist xs)
