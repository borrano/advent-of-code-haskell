module Y21.Day3 (q) where

import Data.Foldable (foldl')
import Data.IntSet qualified as S
import Data.List.Extra (maximumOn)
import Util

toDecimal = foldl' (\a x -> x + (a * 2)) 0

-- intset can effficently split integers
-- clean up later

q2 w pivot s f | S.size s == 1 = head $ S.toList s
q2 w pivot s f = if f $ S.size r >= S.size l then q2 (w - 1) (pivot + (2 ^ w)) r f else q2 (w - 1) (pivot - (2 ^ w)) l f
  where
    (l, b, r') = S.splitMember pivot s
    r = if b then S.insert pivot r' else r'

q = do
  xs <- readLinesString "./data21/day3.txt"
  let nums = fmap (\x -> if x == '1' then 1 else 0) <$> xs
  let (len, width) = (length xs, length (head xs))
  let gamma = fmap (>= (len `div` 2)) $ foldr (zipWith (+)) (repeat 0) (nums)
  let epsilon = fromEnum <$> fmap not gamma
  let gamma' = fromEnum <$> gamma
  let q2' = q2 (width - 2) (2 ^ (width - 1)) $ S.fromList $ toDecimal <$> nums
  return ((toDecimal gamma' * toDecimal epsilon), q2' id * q2' not)

-- >>> q
-- (3882564,3385170)
