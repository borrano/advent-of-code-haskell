module Y21.Day5 (q) where

import Data.List.Extra (splitOn)
import Data.Map qualified as M
import Util (addTuple, readLinesString)

parseLine xs = (go $ words xs !! 0, go $ words xs !! 2)
  where
    go xs = toTuple $ read <$> splitOn "," xs
    toTuple (a : b : _) = (a, b)

q = do
  xs <- readLinesString "./data21/day5.txt"
  let lines = parseLine <$> xs
  return (q1 $ filter horizontalOrVertical lines, q1 lines)

q1 = intersection . concatMap points

intersection = M.size . M.filter (> 1) . M.fromListWith (+) . fmap (,1)

horizontalOrVertical ((a, b), (c, d)) = a == c || b == d

points ((a, b), (c, d)) = take (num + 1) $ iterate (addTuple aim) (a, b)
  where
    num = gcd (c - a) (d - b)
    aim = ((c - a) `div` num, (d - b) `div` num)

-- >>> q
-- (6007,19349)
