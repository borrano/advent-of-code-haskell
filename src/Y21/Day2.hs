module Y21.Day2 (q) where

import Data.ByteString.Char8 qualified as BS
import Data.Foldable (foldl')
import Util (readLines, readLinesString)

q = do
  xs <- readLinesString "./data21/day2.txt"
  let commands = ((\[a, b] -> (a, read b)) . words) <$> xs
  return (q1 commands, q2 commands)

q1 = uncurry (*) . foldl' go (0, 0)
  where
    go (a, b) ("forward", x) = (a, b + x)
    go (a, b) ("down", x) = (a + x, b)
    go (a, b) ("up", x) = (a - x, b)

q2 xs = a * b
  where
    (_, a, b) = foldl' go (0, 0, 0) xs
    go (aim, a, b) ("forward", x) = (aim, a + x, b + (aim * x))
    go (aim, a, b) ("down", x) = (aim + x, a, b)
    go (aim, a, b) ("up", x) = (aim - x, a, b)

-- >>> q
-- (1924923,1982495697)
