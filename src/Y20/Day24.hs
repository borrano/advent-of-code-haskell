module Y20.Day24(q) where

import Data.List.Extra
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Util (readLinesString)

q1 xs = fmap head $ filter (odd . length) $ group $ sort $ foldl' (calculatePos) (0, 0) <$> xs

q2 :: [(Int, Int)] -> Int
q2 = length . (!! 100) . iterate (go . S.fromList)
  where
    go set = [k | (k, v) <- counts, ((v == 2)) || ((v == 1) && k `S.member` set)]
      where
        counts = M.assocs $ M.fromListWith (+) [(n, 1) | x <- S.toList set, n <- neigbours x]

neigbours (a, b) = [(a, b + 2), (a, b - 2), (a + 1, b - 1), (a + 1, b + 1), (a - 1, b - 1), (a - 1, b + 1)]

expand :: [Char] -> [Char]
expand ('e' : xs) = "ee" ++ expand xs
expand ('w' : xs) = "ww" ++ expand xs
expand (a : b : xs) = a : b : expand xs
expand xs = xs

calculatePos :: (Int, Int) -> Char -> (Int, Int)
calculatePos (!a, !b) 'e' = (a, b + 1)
calculatePos (a, b) 'w' = (a, b - 1)
calculatePos (a, b) 'n' = (a - 1, b)
calculatePos (a, b) 's' = (a + 1, b)

q :: IO (Int, Int)
q = do
  lines <- readLinesString "./data20/day24.txt"
  let xs = expand <$> lines
  let blacks = q1 xs
  return (length $ blacks, q2 blacks)
