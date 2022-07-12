module Y21.Day7 (q) where

import Data.List (sort)
import Data.List.Extra (splitOn)
import Util


q1 xs = sum $ fmap (distance1 $ median xs) xs
  where
    distance1 a x = abs (a - x)
    median xs = (sort xs) !! (length xs `div` 2)
    
-- why is it floor-ceiling not round? the mean is 481.52 but it gives the minimum with 481 not 482
q2 xs = minimum [sum $ fmap (distance2 a) xs | a <- means xs]
  where
    distance2 a x = (abs (a - x) * (abs (a - x) + 1)) `div` 2
    means xs = [floor a, ceiling a] where a = (fromIntegral $ sum xs) / (fromIntegral $ length xs)

q = do
  nums <- (fmap (readInt) . splitOn ",") <$> readFile "./data21/day7.txt"
  return (q1 nums, q2 nums)

-- >>> q
-- (348996,98231647)
