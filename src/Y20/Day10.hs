module Y20.Day10 (q) where

import Data.Array qualified as A
import Data.List (sort)
import Util (readNums)

q = do
  nums <- readNums "./data20/day10.txt"
  return (calculate1 nums, calculate2 nums)

calculate1 nums = num 1 * num 3
  where
    xs = sort nums
    ys = zipWith (flip (-)) xs (tail xs)
    num x = 1 + (length $ filter (== x) ys)

calculate2 nums = go (maximum nums) ys
  where
    xs = sort (0 : nums)
    ys = window xs
    window (x : xs) = (x, takeWhile (\x' -> x' - x <= 3) xs) : window xs
    window [] = []
    go n nums = r A.! 0
      where
        r = A.array (0, n) (map f nums)
        f (a, _) | a == n = (a, 1)
        f (x, xs) = (x, sum $ fmap (r A.!) xs)

 