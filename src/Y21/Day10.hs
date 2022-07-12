module Y21.Day10 (q) where

import Data.Either (lefts, rights)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Util (median)

table = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

open = fst <$> table

closed = snd <$> table

scores =
  [ (')', 3),
    (']', 57),
    ('}', 1197),
    ('>', 25137)
  ]

scores2 =
  [ ('(', 1),
    ('[', 2),
    ('{', 3),
    ('<', 4)
  ]

q = do
  lines <- fmap lines $ readFile "./data21/day10.txt"
  let xs = process <$> lines

  return (q1 xs, q2 xs)

q2 xs = median $ fmap (foldl' (\a x -> a * 5 + x) 0 . mapMaybe (flip lookup scores2)) $ rights $ xs

q1 xs = sum $ mapMaybe (\x -> lookup x scores) $ lefts $ xs

process = foldl' go (Right [])
  where
    go (Left x) _ = Left x
    go (Right acc) x | x `elem` open = Right $ x : acc
    go (Right []) x = Left x
    go (Right (a : acc)) x = case lookup a table of
      Just y | x == y -> Right acc
      _ -> Left x

-- >>> q
-- (436497,2377613374)
