module Y20.Day3 (q) where

import Data.ByteString.Char8 qualified as BS
import Data.List.Extra (chunksOf)
import Util (readLines)

calculate (h, w) (xs, width) = length $ filter (== '#') path
  where
    path = zipWith (BS.index) (fmap head $ chunksOf h xs) [(w' `mod` width) | w' <- [0, w ..]]

q1 xs = calculate (1, 3) xs

q2 xs = product [calculate x xs | x <- [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]]

q = do
  xs <- readLines "./data20/day3.txt"
  let width = BS.length (head xs)
  return (q1 (xs, width), q2 (xs, width))
