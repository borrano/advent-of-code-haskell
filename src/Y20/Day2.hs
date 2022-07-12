module  Y20.Day2 (q) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 qualified as BS
import Util (runParser)
import Prelude hiding (takeWhile)

q1 items = length [() | (a, b, c, xs) <- items, let x = BS.count c xs, x >= a && x <= b]

q2 items = length [() | (a, b, c, xs) <- items, let a' = xs BS.!? (a - 1), let b' = xs BS.!? (b - 1), a' /= b' && (a' == Just c || b' == Just c)]

parser = items `sepBy` (satisfy isSpace)
  where
    items = (,,,) <$> (decimal <* (char '-')) <*> (decimal <* whitespace) <*> (anyChar <* char ':') <*> (whitespace *> (takeWhile (not . isSpace)))
    whitespace = many1 (satisfy isSpace)

q = do
  xs <- runParser parser "./data20/day2.txt"
  return (q1 xs, q2 xs)
