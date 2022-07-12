module Y20.Day1 (q) where

import Data.IntSet qualified as S
import Data.List (tails, uncons)
import Data.Maybe (mapMaybe)
import Util

q = do
  xs <- readNums "./data20/day1.txt"
  return (q1 2020 xs, q2 2020 xs) -- calculate3 2020 xs

q1 :: Int -> [Int] -> Int
q1 target xs = product $ S.toList $ S.intersection (S.fromList $ fmap (target -) xs) (S.fromList xs)

-- nearly O(n2)
q2 target nums = product [x * r | (x, xs) <- tails' nums, let r = q1 (target - x) xs, r /= 1]

tails' = mapMaybe (uncons) . tails
