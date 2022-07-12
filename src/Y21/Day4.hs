module Y21.Day4 (q) where

import Data.IntSet qualified as S
import Data.List.Extra (find, split, splitOn, transpose)
import Util (readLinesString)

q = do
  (x : _ : xs) <- readLinesString "./data21/day4.txt"
  let boards = [[read <$> words line | line <- board] | board <- splitOn [""] xs]
  let boards' = [[S.fromList line | line <- b ++ (transpose b)] | b <- boards]
  let nums = read <$> (splitOn "," x)
  return (head $ q1 nums boards', last $ q1 nums boards')

play num = fmap (S.delete num)

won = any (S.null)

q1 (num : nums) boards = fmap score wonBoards ++ (q1 nums continue)
  where
    (wonBoards, continue) = split' won $ play num <$> boards
    score b = num * (sum $ S.toList $ S.unions b)
q1 [] boards = []

split' p [] = ([], [])
split' p (x : xs) = let (l, r) = split' p xs in if p x then (x : l, r) else (l, x : r)


-- >>>q


