module Y20.Day21 (q) where

import Data.Foldable
import Data.List.Extra (intersperse, splitOn)
import Data.Map qualified as M
import Data.Set qualified as S
import Util

q :: IO (Int, String)
q = do
  lines <- readLinesString "./data20/day21.txt"
  let input = [(words $ filter (`notElem` (",)" :: String)) $ b, words a) | l <- lines, let (a : b : _) = splitOn "(contains" l]
  let possible = M.fromListWith (S.intersection) [(i, S.fromList ing) | (allergen, ing) <- input, i <- allergen]
  let safe = [i | (allergen, ing) <- input, i <- ing, i `S.notMember` (S.unions $ M.elems possible)]
  return $ (length safe, mconcat $ intersperse "," $ head $ go $ M.assocs $ M.map toList possible)

go (x : xs) = [(a : rest) | a <- snd x, rest <- go xs, a `notElem` rest]
go [] = [[]]
