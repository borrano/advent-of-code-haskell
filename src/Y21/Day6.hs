module Y21.Day6 (q) where

import Data.IntMap qualified as IM
import Data.List.Extra (splitOn)
import Util

turn xs = IM.foldlWithKey' go IM.empty xs
  where
    go map k v | k == 0 = IM.insertWith (+) 6 v $ IM.insert 8 v map
    go map k v = IM.insertWith (+) (k - 1) v map

q = do
  nums <- (fmap (readInt) . splitOn ",") <$> readFile "./data21/day6.txt"
  let nums' = IM.fromListWith (+) $ ((,1) <$> nums)
  let simulate n = sum $ IM.elems $ (!! n) $ iterate turn nums'
  return (simulate 80, simulate 256)
