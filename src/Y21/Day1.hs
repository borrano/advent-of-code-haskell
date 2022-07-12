module Y21.Day1 (q) where

import Data.List (tails)
import Util (readNums)

q = do
  xs <- readNums "./data21/day1.txt"
  return (q1 xs, q2 xs)

q1 xs = length $ filter id $ zipWith (<) xs (tail xs)

-- i have seen  a windowing function like this somewhere on stackoverflow before
-- TODO : find the source
q2 = q1 . foldr (zipWith (+)) (repeat 0) . take 3 . tails

-- >>> q
-- (1400,1429)
