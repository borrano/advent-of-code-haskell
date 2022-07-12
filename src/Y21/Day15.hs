module Y21.Day15 (q) where

import Data.Array.Unboxed qualified as A
import Data.Foldable
import Data.PQueue.Prio.Min qualified as P
import Data.Set qualified as S

q = do
  xs <- lines <$> readFile "./data21/day15.txt"
  let dim = length xs
  let nums = A.array ((0, 0), (dim, dim)) [((i, j), ((read [v]))) | (i, x) <- zip [0 ..] xs, (j, v) <- zip [0 ..] x]
  return $ (q1 dim 1 nums, q1 dim 5 nums)

ns dim (a, b) = [x | x@(i, j) <- [(a + 1, b), (a - 1, b), (a, b + 1), (a, b - 1)], i >= 0, j >= 0, i < dim, j < dim]

get dim map (a, b) = correct ((map A.! (am, bm)) + ad + bd)
  where
    (ad, am) = a `divMod` dim
    (bd, bm) = b `divMod` dim
    correct n = if n > 9 then (n `mod` 10) + 1 else n

q1 :: Int -> Int -> A.UArray (Int, Int) Int -> Int
q1 dim scale map = go S.empty (insert (-(map A.! (0, 0))) P.empty (0, 0))
  where
    target = (dim * scale)
    go set queue =
      case P.deleteFindMin queue of
        ((_, (c, _)), newqueue) | S.member c set -> go set newqueue
        ((_, (c, score)), _) | c == (target - 1, target - 1) -> score
        ((_, (c, score)), newqueue) -> go newSet (foldl' (insert score) newqueue (filter (flip S.notMember newSet) $ ns target c))
          where
            newSet = (S.insert c set)
    insert score q c = P.insert (score' + distance c) (c, score') q
      where
        score' = score + get dim map c
        distance (a, b) = 2 * target - a - b -- need better heuristic - it does not change much basically it is djkstra algo