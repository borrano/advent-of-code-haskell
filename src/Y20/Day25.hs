module Y20.Day25 (q) where

import Control.Monad.Par
import Control.Monad.Par qualified as P
import Data.Foldable (asum, find)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

calculate :: Int -> Int -> Int
calculate sn target = go 0 1
  where
    go !index !cur
      | cur == target = index
      | otherwise = go (index + 1) (cur * sn `mod` 20201227)

calculateIndex :: Int -> Int -> Int
calculateIndex sn targetIndex = go 0 1
  where
    go !index !cur
      | index == targetIndex = cur
      | otherwise = go (index + 1) (cur * sn `mod` 20201227)

q = return $ calculateIndex 8252394 (calculate 7 6269621)
