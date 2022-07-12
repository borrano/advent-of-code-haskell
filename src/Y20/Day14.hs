module Y20.Day14 (q) where

import Data.Bits
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.IntMap qualified as M
import Data.List.Extra (foldl', isPrefixOf, splitOn, unfoldr)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Tuple.Extra (first)
import Util (readLinesString)

q = do
  xs <- readLinesString "./data20/day14.txt"
  let lines = [(mask, addr, value) | (mask, xs) <- go xs, (addr, value) <- xs]
  return (calculate1 lines, calculate2 lines)

go :: [String] -> [(String, [(Int, Int)])]
go [] = []
go (xs : xss) = ((words xs) !! 2, (parseMem . words) <$> l) : go r
  where
    (l, r) = span ("mem" `isPrefixOf`) xss
    parseMem (x : _ : y : xs) = (read $ takeWhile (/= ']') $ drop 4 x, read y)

toBin x = reverse $ take 36 $ (unfoldr (\x -> if x == 0 then Nothing else Just (swap (divMod x 2))) x) ++ repeat 0

sumMemory = sum . M.elems . foldl' (\m (k, v) -> M.insert k v m) M.empty

calculate1 xss = sumMemory [(addr, fromBin $ (zipWith goMask mask $ toBin value)) | (mask, addr, value) <- xss]
  where
    goMask 'X' a = a
    goMask '1' _ = 1
    goMask '0' _ = 0
    fromBin x = foldl' (\x a -> 2 * x + a) 0 x

calculate2 xss = sumMemory $ [(addr', value) | (mask, addr, value) <- xss, addr' <- combinations (reverse $ zipWith goMask mask (toBin addr))]
  where
    goMask 'X' _ = 'X'
    goMask '1' _ = '1'
    goMask '0' 0 = '0'
    goMask '0' 1 = '1'

    combinations ('X' : xss) = [(nums * 2 + i) | nums <- (combinations xss), i <- [0, 1]]
    combinations ('0' : xss) = fmap (* 2) (combinations xss)
    combinations ('1' : xss) = fmap ((+ 1) . (* 2)) (combinations xss)
    combinations [] = [0]
