module Y20.Day23 (q) where

import Control.Monad
import Control.Monad.ST
import Data.Bifunctor
import Data.List.Extra (splitOn)
import Data.Vector.Unboxed.Mutable qualified as V
import Util

input1 = ([4, 5, 9, 6, 7, 2, 8, 1, 3], 9, 100)

input2 = ([4, 5, 9, 6, 7, 2, 8, 1, 3], 1000000, 10000000)

vread vec start 0 = return []
vread vec start n = do
  x <- V.read vec start
  xs <- vread vec x (n - 1)
  return (x : xs)

answer2 (input, len, count) n = runST $ do
  let xs = take len $ input ++ [10 ..]
  vec <- V.new (len + 1)
  forM_ (zip xs $ (tail xs ++ [head xs])) $ \(i, j) -> V.write vec i j
  myIterateM_ count (go vec) (head input)
  vread vec 1 n
  where
    go vec start = do
      [p1, p2, p3, np] <- vread vec start 4
      V.write vec start np
      let dest = head $ filter (`notElem` [p1, p2, p3]) $ tail $ iterate (\s -> if s - 1 == 0 then len else s - 1) (start)
      V.read vec dest >>= V.write vec p3
      V.write vec dest p1 
      V.read vec start

q :: IO (Int, Int)
q = return (read $ concat $ show <$> answer2 input1 8, product $ answer2 input2 2)
