module Y21.Day11 (q) where

import Control.Arrow
import Data.List (unfoldr, findIndex)
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)

q = do
  xs <- (lines) <$> readFile "./data21/day11.txt"
  let map = parse $ xs
  let dims = (length xs, length (head xs))
  let q1 = sum $ take 101 $ fmap (M.size . M.filter (== 0)) $ iterate (turn dims) map
  let q2 = fromMaybe undefined $ findIndex (== (fst dims * snd dims)) $ fmap (M.size . M.filter (== 0)) $ iterate (turn dims) map
  return (q1,  q2)

turn dims xs = go $ M.map (+ 1) xs
  where
    go map =
      let exp = M.filter (> 9) map
          added = M.fromListWith (+) [(n, 1) | k <- M.keys exp, n <- neigbours dims k, map M.! n /= 0]
       in if M.null exp
            then map
            else go $ M.map (\_ -> 0) exp `M.union` (M.unionWith (+) map added)

parse :: [[Char]] -> M.Map (Int, Int) Int
parse lines = M.fromList [((x, y), read [c]) | (x, ys) <- zip [0 ..] lines, (y, c) <- zip [0 ..] ys]

neigbours (len, width) (i, j) =
  [x | i' <- [-1 .. 1], j' <- [-1 .. 1], (i' /= 0 || j' /= 0), let x@(a, b) = (i + i', j + j'), a >= 0 && b >= 0 && a < len && b < width]

-- >>> q
-- (1625,244)
