module Ch1MinFree where

-- | Find elements in one list not present in another list.
(\\) :: (Eq a) => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

-- | Find the first natural number not present in xs
minFree :: [Int] -> Int
minFree xs = head ([0 ..] \\ xs)
