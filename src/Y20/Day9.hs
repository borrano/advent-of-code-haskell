module Y20.Day9 (q) where

import Data.Foldable
import Data.IntSet qualified as S
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Util

next ((_ :<| as), (_ :<| bs)) x = ((as :|> x), (bs :|> (S.fromList (toList $ fmap (+ x) as))))

exists x (_, xs) = any (S.member x) xs

calculate1 nums =
  fst $ head $ filter (not . uncurry exists) $ zip xs $ scanl next pre xs
  where
    (header, xs) = splitAt 25 nums
    pre = (Seq.fromList header, Seq.fromList [S.fromList [a + b | b <- header] | a <- header])

next2 target (ass, sum) x =
  if sum + x > target
    then let (a :<| as) = ass in next2 target (as, sum - a) x
    else (ass :|> x, sum + x)

calculate2 nums = fst $ head $ filter ((== target) . snd) $ scanl (next2 target) (Seq.empty, 0) nums
  where
    target = calculate1 nums

q = do
  nums <- readNums "./data20/day9.txt"
  let xs = calculate2 nums
  return $ (calculate1 nums, maximum xs + minimum xs)
