module Y20.Day15 (q) where

import Control.Monad (foldM, forM_)
import Control.Monad.ST (runST)
import Data.HashMap.Strict qualified as HM
import Data.Int (Int32)
import Data.IntMap.Lazy qualified as M
import Data.List (foldl', unfoldr)
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as Va
import Data.Vector.Unboxed.Mutable qualified as V
import Data.Word (Word32)

-- 
slow input target = snd $ foldl' go (map, last input) [(length input)   .. (target - 1)]
  where
    map = (M.fromList [(x, i) | (i, x) <- zip [1 ..] input])
    go (map, lastItem) i = (M.insert lastItem (i - 1) map, maybe 0 (i - ) (M.lookup lastItem map))

go map lastItem i = do
  item <- V.read map lastItem
  V.write map lastItem i
  return $ if item == 0 then 0 else i - item

answer input target = runST $ do
  map <- V.replicate (target) 0
  forM_ (zip input [1 ..]) $ \(t, s) -> V.write map t s
  result <- foldM (go map) (last input) [(length input) .. (target - 1)]
  return result

q = return $ (answer  input 2020, answer input 30000000)
  where
    input = [2, 0, 6, 12, 1, 3]
