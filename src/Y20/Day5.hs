module Y20.Day5 (q) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 qualified as BS
import Data.List (partition, sort)
import Data.List.Extra (chunksOf)
import Util (readLines, readLinesString, runParser)
import Prelude hiding (takeWhile)

q = do
  lines <- readLinesString "./data20/day5.txt"
  let numbers = toInt <$> lines
  return (maximum numbers, minfree numbers)

toInt xs = foldl (\n a -> n * 2 + a) 0 (go <$> xs)
  where
    go 'B' = 1
    go 'R' = 1
    go _ = 0

-- from bird's functional pearls
minfree xs = go (minimum xs) (length xs, xs)
  where
    go a (n, xs)
      | n == 0 = a
      | m == b - a = go b (n - m, vs)
      | otherwise = go a (m, us)
      where
        (us, vs) = partition (< b) xs
        b = a + 1 + n `div` 2
        m = (length us)
