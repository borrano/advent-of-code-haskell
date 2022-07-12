module Y20.Day13 (q) where

import Data.List.Extra (minimumOn, splitOn)
import Util

q = do
  (a : b : as) <- readLinesString "./data20/day13.txt"
  let xs = [(-i, read c) | (i, c) <- zip [0 ..] $ splitOn "," b, c /= "x"]
  return (calculate1 (read a) (snd <$> xs), fst $ calculate2 xs)

calculate1 a xs = ((x - a) * y)
  where
    go b = (head $ filter (>= a) $ fmap (b *) [(a `div` b) ..], b)
    (x, y) = minimumOn fst $ fmap go xs

-- from https://stackoverflow.com/questions/35529211/chinese-remainder-theorem-haskell
calculate2 :: [(Integer, Integer)] -> (Integer, Integer)
calculate2 = foldr go (0, 1)
  where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
      where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd (b `mod` a) a
