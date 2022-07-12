module Y20.Day17 (q) where

import Data.Functor ((<&>))
import Data.List (group, nub, sort)
import Data.Map qualified as M
import Data.Set qualified as S
import Util (readLinesString)

offsets n = filter (not . all (== 0)) $ go n
  where
    go :: Int -> [[Int]]
    go 1 = [[-1], [0], [1]]
    go n = [a : as | a <- [-1, 0, 1], as <- go (n - 1)]

answer :: Int -> [[Int]] -> Int
answer n = length . (!! 6) . iterate (go)
  where
    offsets' = offsets n
    go xss = [k | (k, v) <- M.assocs (counts xss), v == 3 || (v == 2 && k `S.member` (S.fromList xss))]
    counts xss = M.fromListWith (+) [(zipWith (+) xs offset, 1) | xs <- xss, offset <- offsets']

q = do
  xs2 <- parse <$> readLinesString "./data20/day17.txt"
  let xs3 = (0 :) <$> xs2
  let xs4 = (0 :) <$> xs3
  return (answer 3 xs3, answer 4 xs4)

parse lines = [[x, y] | (x, ys) <- zip [0 ..] lines, (y, c) <- zip [0 ..] ys, c == '#']
