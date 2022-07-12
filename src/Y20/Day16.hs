module Y20.Day16 (q) where

import Control.Applicative ((<|>))
import Data.Foldable
import Data.IntSet qualified as S
import Data.List.Extra
import Util (readLinesString)

readInt :: String -> Int
readInt = read

parseLines xs = (parseHeader a, parseOther (x : cs))
  where
    (a : (b : x : _) : (c : cs) : _) = splitOn [[]] xs
    parseOther cs = (fmap readInt . splitOn ",") <$> cs
    parseHeader a = (first . words . dropWhile (/= ':')) <$> a
      where
        first xs = (toRange $ (xs !! 1), toRange $ (xs !! 3))
        toRange x = let [a, b] = splitOn "-" x in (readInt a, readInt b)

type Func = Int -> S.IntSet

go ranges = foldl' (\y -> combine y . make) (const S.empty) $ zip [0 ..] ranges
  where
    combine p1 p2 num = p1 num `S.union` p2 num
    make (i, ((a, b), (c, d))) num = if (num >= a && num <= b) || (num >= c && num <= d) then S.singleton i else S.empty

-- because only a single solution exists we can do this without expanding
solve :: [S.IntSet] -> [Int]
solve xs
  | all ((== 1) . S.size) xs = fmap (head . S.toList) xs
  | otherwise = solve $ fmap go xs
  where
    singles = S.unions $ filter ((== 1) . S.size) xs
    go x = if S.size x == 1 then x else S.difference x singles

q2 g nums@(myticket : _) = result $ zip (order valid) myticket
  where
    result = product . fmap snd . take 6 . sortOn fst
    order = solve . fmap (foldl1' S.intersection) . transpose
    valid = [x | num <- nums, let x = g <$> num, all (not . S.null) x]

q1 g nums = sum $ filter (S.null . g) $ concat nums

q = do
  lines <- readLinesString "./data20/day16.txt"
  let xs@(ranges, nums) = parseLines lines
  let f = go ranges
  return (q1 f nums, q2 f nums)
