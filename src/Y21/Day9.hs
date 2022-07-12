module Y21.Day9 (q) where

import Data.List.Extra (minimumBy, minimumOn, nub, sort)
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import Util (readInt, readLinesString)

get vec (a, b) = (vec V.! a) UV.! b

neigbours1 ok (i, j) = filter ok $ [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)]

q1 neigbours vec rows = [(v, (i, j)) | (i, row) <- zip [0 ..] rows, (j, v) <- zip [0 ..] row, all (v <) $ fmap (get vec) $ neigbours (i, j)]

q2 f vec x = go S.empty [x]
  where
    go visited ((item, x) : xs) | S.member x visited = go visited xs
    go visited ((item, x) : xs) = if (all (flip S.member visited) xs'') && item /= 9 then go (S.insert x visited) (xs ++ xs') else go (visited) xs
      where
        xs'' = [n | (v, n) <- xs', item > v]
        xs' = [(v, n) | n <- f x, let v = get vec n]
    go visited [] = S.size visited

q = do
  xs <- readLinesString "./data21/day9.txt"
  let (len, width) = (length xs, length (head xs))
  let ok (a, b) = a >= 0 && b >= 0 && a < len && b < width

  let elems = [[readInt $ pure a | a <- x] | x <- xs]
  let vec = V.fromList $ fmap UV.fromList elems
  let indexes = [(i, j) | i <- [0 .. len], j <- [0 .. width]]
  let res = q1 (neigbours1 ok) vec elems
  let q2' = product $ take 3 $ reverse $ sort $ fmap (q2 (neigbours1 ok) vec) (res)
  return (sum $ fmap ((+ 1) . fst) $ q1 (neigbours1 ok) vec elems, q2')

-- >>> q
-- (462,1397760)
