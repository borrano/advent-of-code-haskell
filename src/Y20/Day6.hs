module Y20.Day6 (q) where

import Data.ByteString.Char8 qualified as BS
import Data.Char (ord)
import Data.IntSet qualified as S
import Data.List.Extra
import Util (readLines, readLinesString)

parseFile lines = [[S.fromList $ ord <$> x | x <- xs] | xs <- split null lines]

q = do
  sets <- parseFile <$> readLinesString "./data20/day6.txt"
  let answer f = sum . fmap (S.size . f)
  return $ (sum $ fmap (S.size . S.unions) sets, sum $ fmap (S.size . (foldl1 S.intersection)) sets)
