module Y21.Day21 (q) where

import Control.Arrow
import Data.Array qualified as A
import Data.List.Extra (chunksOf, foldl')
import Data.Map qualified as M
import Data.Maybe
import Data.Tuple.Extra (both)

q = do
  let p1 = (7, 0)
  let p2 = (10, 0)
  let (turn, n) = q1 p1 p2
  return ((turn + 3) * n, uncurry max $ q2 p1 p2)

move (cur, points) s = (floor' $ cur + s, points + (floor' $ cur + s))
  where
    floor' x
      | x <= 10 = x
      | otherwise = floor' (x - 10)

q1 p1 p2 = process $ go (p1, p2) moves
  where
    process xs = last $ takeWhile ((< 1000) . snd) $ [(i, score) | (i, (_, score)) <- zip [3, 6 ..] xs]
    moves = fmap sum $ chunksOf 3 $ cycle [1 .. 100]
    go (p1, p2) (x : y : xs) = p1' : p2' : go (p1', p2') xs
      where
        p1' = move p1 x
        p2' = move p2 y

q2 p1 p2 = r A.! (0, p1, p2)
  where
    nums = [(p, (p0p, p0s), (p1p, p1s)) | p <- [0, 1], p0p <- [1 .. 10], p0s <- [0 .. 30], p1p <- [1 .. 10], p1s <- [0 .. 30]]
    r = A.listArray ((0, (1, 0), (1, 0)), (1, (10, 30), (10, 30))) (map f nums)
    f (_, _, b) | snd b >= 21 = (0, 1)
    f (_, a, _) | snd a >= 21 = (1, 0)
    f (0, a, b) = sum' [both (f *) (r A.! (1, (move a s), b)) | (s, f) <- moves]
    f (1, a, b) = sum' [both (f *) (r A.! (0, a, (move b s))) | (s, f) <- moves]
    moves = M.assocs $ M.fromListWith (+) [((a + b + c), 1) | a <- [1 .. 3], b <- [1 .. 3], c <- [1 .. 3]]
    sum' xs = foldl' (\(a, b) (c, d) -> ((a + c), (b + d))) (0, 0) xs

